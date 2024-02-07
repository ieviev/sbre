module rec Sbre.Optimizations

open Sbre.Algorithm
open Sbre.Types
open Sbre.Pat
open System

[<RequireQualifiedAccess>]
type InitialOptimizations =
    | NoOptimizations
    /// ex. Twain ==> (ε|Twain)
    | StringPrefix of prefix:string * transitionNodeId:int
    /// ex. [Tt][Ww][Aa][Ii][Nn] ==> (ε|(?i)Twain)
    | SetsPrefix of prefix:Memory<TSet> * transitionNodeId:int
    /// ex. (Twain|Huck) ==> potential start:[TH][wu][ac][ik]
    | PotentialStartPrefix of prefix:Memory<TSet>

type ActiveBranchOptimizations =
    | ReverseStringPrefix of string
    | ReverseSetsPrefix of TSet array
    | NoOptimizations


let getFixedLength (node: RegexNode<TSet>) =
    let rec loop (acc:int) node : int option =
        match node with
        | Concat(head, tail, info) ->
            loop acc head
            |> Option.bind (fun headLen ->
                loop headLen tail
            )
        | Epsilon -> None
        | Or(nodes, info) -> None
        | Singleton foo -> Some (1 + acc)
        | Loop(Singleton node, low, up, info) ->
            if low = up then Some (low + acc) else None
        | Loop(node, low, up, info) -> None
        | And(nodes, info) -> None
        | Not(node, info) -> None
        | LookAround(_) -> None
    loop 0 node

#if DEBUG
let printPrefixSets (cache:RegexCache<_>) (sets:TSet list) =
    sets
    |> Seq.map cache.PrettyPrintMinterm
    |> String.concat ";"
#endif

let getImmediateDerivatives (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    cache.Minterms()
    |> Array.map (fun minterm ->
        let loc = Pat.Location.getDefault ()
        let state = Sbre.CountingSet.RegexState(cache.NumOfMinterms())
        let der = Algorithm.createStartsetDerivative (cache, minterm, node)
        minterm, der
    )
let getImmediateDerivativesMerged (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    cache.Minterms()
    |> Array.map (fun minterm ->
        let loc = Pat.Location.getDefault ()
        let state = Sbre.CountingSet.RegexState(cache.NumOfMinterms())
        let der = Algorithm.createStartsetDerivative (cache,  minterm, node)
        minterm, der
    )
    |> Seq.groupBy snd
    |> Seq.map (fun (_, group) ->
        group |> Seq.map fst |> Seq.fold (|||) cache.Solver.Empty, group |> Seq.head |> snd
    )

let getNonRedundantDerivatives
    (cache: RegexCache<TSet>)
    (redundantNodes: System.Collections.Generic.HashSet<RegexNode<TSet>>)
    (node: RegexNode<TSet>)
    =
    getImmediateDerivativesMerged cache node
    |> Seq.where (fun (mt, deriv) -> not (redundantNodes.Contains(deriv)))
    |> Seq.toArray

/// strip parts irrelevant for prefix
let rec getPrefixNodeAndComplement (cache:RegexCache<_>) (node:RegexNode<_>) : RegexNode<_> * RegexNode<_> option =
    match node with
    | Concat(Loop(low=0;up=Int32.MaxValue),tail,_ ) -> getPrefixNodeAndComplement cache tail
    | Concat(Loop(node=body;low=n;up=Int32.MaxValue),tail,_ ) ->
        cache.Builder.mkConcat2( cache.Builder.mkLoop(body,n,n),tail ), None

    // TODO: ?
    // | Not(node, info) -> getPrefixNodeAndComplement cache node
    | And(nodes, info) ->
        let existsComplement =
            nodes
            |> Seq.exists (function | Not _ -> true | _ -> false )

        if not existsComplement then

            let prefixes =
                nodes
                |> Seq.choose (fun v ->
                    match v with
                    | Concat(head=Singleton p) -> Some v
                    | _ -> None
                )
                |> Seq.toArray

            if prefixes.Length > 0 then
                cache.Builder.mkOr(prefixes), None
            else

            let trimmed =
                nodes
                |> Seq.map (getPrefixNodeAndComplement cache)
                |> Seq.toArray
            let noComplements = trimmed |> Seq.forall (fun v -> (snd v).IsNone)
            if noComplements then
                cache.Builder.mkOr(trimmed |> Seq.map fst), None
            else
                node, None

        else

        let nonComplementNodes =
            nodes
            |> Seq.where (function | Not _ -> false | _ -> true )
            |> Seq.toArray
        if nonComplementNodes.Length = 0 || nonComplementNodes.Length = nodes.Count then node, None else
        let complement =
            nodes
            |> Seq.choose (function | Not (inner,info) -> Some inner | _ -> None )
            |> cache.Builder.mkOr
            |> Some

        let trimmed =
            nonComplementNodes
            |> Seq.map (getPrefixNodeAndComplement cache)
            |> Seq.toArray

        let noComplements =
            trimmed
            |> Seq.forall (fun v -> (snd v).IsNone)

        if noComplements then
            cache.Builder.mkOr(trimmed |> Seq.map fst), complement
        else
            cache.Builder.mkAnd(nonComplementNodes), complement
    | _ -> node, None


let rec calcPrefixSets (getStateFlags: RegexNode<_> -> RegexStateFlags) (cache: RegexCache<_>) (startNode: RegexNode<_>) =
    let redundant = System.Collections.Generic.HashSet<RegexNode<TSet>>([ cache.False ])

    // nothing to complement if a match has not started
    let prefixStartNode, complementStartset =
        getPrefixNodeAndComplement cache startNode


    let rec loop acc node =
        let isRedundant = not (redundant.Add(node))
        // let all_derivs = getImmediateDerivativesMerged cache node |> Seq.toArray
        let prefix_derivs = getNonRedundantDerivatives cache redundant node
        // a -> t
        let stateFlags = getStateFlags node
        if stateFlags.CanSkip && not (refEq prefixStartNode node) then acc |> List.rev else
        // if stateFlags.CanSkip && not (refEq startNodeWithoutComplement node) then acc |> List.rev else
        if isRedundant then
            acc |> List.rev
        else if node.IsAlwaysNullable  then
            acc |> List.rev
        else

            match prefix_derivs with
            | [| (mt, deriv) |] ->
                let acc' = mt :: acc
                loop acc' deriv
            | _ ->
                // let merged_pred = prefix_derivs |> Seq.map fst |> Seq.fold (|||) cache.Solver.Empty
                prefix_derivs |> Seq.map snd |> Seq.iter (redundant.Add >> ignore)
                acc |> List.rev
    let prefix = loop [] prefixStartNode

    match complementStartset with
    | _ when prefix.IsEmpty -> prefix
    | None -> prefix
    | Some compl ->
        let complementStartset = calcPrefixSets getStateFlags cache compl
        let trimmedPrefix =
            prefix
            |> Seq.takeWhile (fun v ->
                not (cache.Solver.isElemOfSet(v,complementStartset[0]))
            )
            |> Seq.toList
        if trimmedPrefix.IsEmpty then
            [prefix[0]] else trimmedPrefix



let rec calcPotentialMatchStart (getStateFlags: RegexNode<_> -> RegexStateFlags) (cache: RegexCache<_>) (startNode: RegexNode<_>) =
    let redundant = System.Collections.Generic.HashSet<RegexNode<TSet>>([ cache.False ])
    let rec loop acc (nodes:RegexNode<_> list) =
        let stateFlags =
            nodes |> Seq.map getStateFlags
        let shouldExit =
            stateFlags |> Seq.exists (_.CanSkip) || nodes |> Seq.exists (_.IsAlwaysNullable)
        if shouldExit then acc |> List.rev else
        let nonRedundant =
            nodes |> List.where (redundant.Add)
        if nonRedundant.IsEmpty then
            acc |> List.rev
        else
            let prefixDerivsList =
                nonRedundant
                |> List.map (getNonRedundantDerivatives cache redundant)

            let merged_pred =
                prefixDerivsList
                |> Seq.collect id
                |> Seq.map fst
                |> Seq.fold (|||) cache.Solver.Empty

            let remainingNodes =
                prefixDerivsList
                |> Seq.collect id
                |> Seq.map snd
                |> Seq.toList
            let acc' = merged_pred :: acc
            // let dbg = printPrefixSets cache acc'
            loop acc' remainingNodes

    let prefixStartNode, complementStartset =
        getPrefixNodeAndComplement cache startNode
    match complementStartset with
    | Some c -> [] // todo
    | None -> loop [] [prefixStartNode]




let rec applyPrefixSets (cache:RegexCache<_>) (node:RegexNode<TSet>) (sets:TSet list) =
    assert (not node.ContainsLookaround)
    match sets with
    | [] -> node
    | head :: tail ->
        let loc = Pat.Location.getDefault ()
        let state = Sbre.CountingSet.RegexState(cache.NumOfMinterms())
        let der = Algorithm.createStartsetDerivative (cache, head, node)
        applyPrefixSets cache der tail

let findInitialOptimizations
    (nodeToId:RegexNode<TSet> -> int)
    (nodeToStateFlags:RegexNode<TSet> -> RegexStateFlags)
    (c:RegexCache<TSet>) (node:RegexNode<TSet>) (trueStarredNode:RegexNode<TSet>) =
    if node.ContainsLookaround then InitialOptimizations.NoOptimizations else
    match Optimizations.calcPrefixSets nodeToStateFlags c node with
    | prefix when prefix.Length > 1 ->

        let mts = c.Minterms()
        let singleCharPrefixes =
            prefix
            |> Seq.map (fun v ->
                // negated set
                if Solver.elemOfSet v mts[0] then None else
                let chrs = c.MintermChars(v)
                if chrs.Length = 1 then Some (chrs[0]) else None
            )
            |> Seq.takeWhile Option.isSome
            |> Seq.choose id
            |> Seq.rev
            |> Seq.toArray
            |> String
        if singleCharPrefixes.Length > 1 then
            let applied = Optimizations.applyPrefixSets c trueStarredNode (List.take singleCharPrefixes.Length prefix)
            InitialOptimizations.StringPrefix(singleCharPrefixes,nodeToId applied)
        else
            let applied = Optimizations.applyPrefixSets c trueStarredNode prefix
            let mem = Memory(Seq.toArray prefix)
            InitialOptimizations.SetsPrefix(mem,nodeToId applied)
    | _ ->
        match Optimizations.calcPotentialMatchStart nodeToStateFlags c node with
        | potentialStart when potentialStart.Length > 0 ->
            let mem = Memory(Seq.toArray potentialStart)
            InitialOptimizations.PotentialStartPrefix(mem)
        | _ -> InitialOptimizations.NoOptimizations

let tryJumpToStartset (c:RegexCache<TSet>)(loc:byref<Location>) (node:RegexNode<TSet>) : int32 =
    loc.Position
    // let prefix : InitialStartset<_> =
    //     c.Builder.GetInitializedPrefix(node)
    //
    // match prefix with
    // | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->
    //     let arr = arr.Span
    //     let loopEnd = loopEnd.Span
    //     let commonStartsetLocation =
    //         if refEq c.InitialPatternWithoutDotstar node || loopEnd.Length = 0 then
    //             if not loc.Reversed then
    //                 let chars = c.MintermStartsetChars(arr[0])
    //                 c.TryNextStartsetLocationArray(&loc,arr, chars)
    //                 ValueSome loc.Position
    //             else c.TryNextStartsetLocationArrayReversed(&loc,arr)
    //         elif arr.Length = 1 && loopEnd.Length = 1 then
    //             c.TryNextStartsetLocation(&loc,c.Solver.Or(arr[0],loopEnd[0]))
    //             ValueSome loc.Position
    //         else
    //             c.TryNextStartsetLocationArrayWithLoopTerminator(&loc,arr,loopEnd)
    //     match commonStartsetLocation with
    //     | ValueNone ->
    //         Location.final loc
    //
    //     | ValueSome newPos -> newPos
    // | _ ->
    //     // TODO: does occur
    //     loc.Position

let tryJumpToStartset2 (c:RegexCache<TSet>)(loc:byref<Location>) (node:RegexNode<TSet>) : unit =
    ()
    //
    // match c.Builder.GetInitializedPrefix(node) with
    // | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->
    //     let arr = arr.Span
    //     let loopEnd = loopEnd.Span
    //
    //     let commonStartsetLocation =
    //         if loopEnd.Length = 0 then
    //             let chars = c.MintermStartsetChars(arr[0])
    //             c.TryNextStartsetLocationArray(&loc,arr, chars)
    //             ValueSome loc.Position
    //         // elif arr.Length = 1 then
    //         //     let chars = c.MintermStartsetChars(arr[0] ||| loopEnd[0])
    //         //     c.SkipIndexOfAny(&loc,chars)
    //         //     ValueSome loc.Position
    //         else c.TryNextStartsetLocationArrayWithLoopTerminator(&loc,arr,loopEnd)
    //     match commonStartsetLocation with
    //     | ValueNone ->
    //         loc.Position <- Location.final loc
    //     | ValueSome newPos -> loc.Position <- newPos
    // | _ -> ()
    //
