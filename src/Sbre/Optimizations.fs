module rec Sbre.Optimizations

open System.Buffers
open System.Collections.Generic
open Sbre.Algorithm
open Sbre.Types
open Sbre.Pat
open System

[<RequireQualifiedAccess>]
type InitialOptimizations =
    | NoOptimizations
    /// ex. Twain ==> (ε|Twain)
    | StringPrefix of prefix:Memory<char> * transitionNodeId:int
    | SearchValuesPrefix of prefix:Memory<SearchValues<char>> * transitionNodeId:int
    /// ex. [Tt][Ww][Aa][Ii][Nn] ==> (ε|(?i)Twain)
    | SetsPrefix of prefix:Memory<TSet> * transitionNodeId:int
    /// ex. (Twain|Huck) ==> potential start:[TH][wu][ac][ik]
    | PotentialStartPrefix of prefix:Memory<TSet>

type ActiveBranchOptimizations =
    | LimitedSkip of distance:int * termPred:SearchValues<char> * termTransitionId:int * nonTermTransitionId:int
    | NoOptimizations




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
            |> Memory
        if singleCharPrefixes.Length > 1 then
            let applied = Optimizations.applyPrefixSets c trueStarredNode (List.take singleCharPrefixes.Length prefix)
            InitialOptimizations.StringPrefix(singleCharPrefixes,nodeToId applied)
        else
            let applied = Optimizations.applyPrefixSets c trueStarredNode prefix
            let containsSmallSets =
                prefix |> Seq.forall (fun v -> not (c.MintermIsInverted(v)))
                && prefix |> Seq.forall (fun v -> c.MintermChars(v).Length <= 5)
            if containsSmallSets then
                let searchPrefix = prefix |> Seq.map c.MintermSearchValues |> Seq.toArray |> Memory
                InitialOptimizations.SearchValuesPrefix(searchPrefix,nodeToId applied)
            else
                let mem = Memory(Seq.toArray prefix)
                InitialOptimizations.SetsPrefix(mem,nodeToId applied)
    | _ ->
        match Optimizations.calcPotentialMatchStart nodeToStateFlags c node with
        | potentialStart when potentialStart.Length > 0 ->
            let mem = Memory(Seq.toArray potentialStart)
            InitialOptimizations.PotentialStartPrefix(mem)
        | _ -> InitialOptimizations.NoOptimizations


let tryGetLimitedSkip (nodeToId:RegexNode<TSet> -> int) (getStartset:RegexNode<_> -> TSet) (c:RegexCache<_>) (initial:RegexNode<_>) (node:RegexNode<_>) =
    assert(not node.ContainsLookaround)
    let redundant = HashSet([initial])
    let skipTerm = getStartset initial // m.GetOrCreateState(initial).Startset
    match node with
    | Or(nodes, info) ->
        let nonInitial = nodes |> Seq.where (fun v -> not (refEq v initial)) |> c.Builder.mkOr
        let nonTermDerivatives (node: RegexNode<TSet>) =
            let ders1 = Optimizations.getNonRedundantDerivatives c redundant node
            ders1 |> Array.where (fun (mt,_) -> not (Solver.contains skipTerm mt) )

        let nonInitialNonTerm =
            nonTermDerivatives nonInitial

        match nonInitialNonTerm with
        | [| singlePath |] ->
            let path = ResizeArray()
            let rec loop (node: RegexNode<_>) =
                match nonTermDerivatives node with
                | [| (mt,single) |] when (not (node.CanBeNullable || refEq c.False node || c.Solver.IsFull(mt))) ->
                    redundant.Add(node) |> ignore
                    path.Add(mt)
                    loop single
                | _ -> node
            let finalNode = loop (snd singlePath)
            if path.Count < 2 then None else
                if c.MintermIsInverted(skipTerm) then None else
                    // failwith "todo: inverted minterm"
                let chrs = c.MintermChars(skipTerm)
                if chrs.Length > 50 then
                    failwith "todo: too many chars in set"
                Some (
                    ActiveBranchOptimizations.LimitedSkip(
                    distance=path.Count + 1,
                    termPred= c.MintermSearchValues(skipTerm),
                    termTransitionId=nodeToId (createStartsetDerivative (c,  skipTerm, node)),
                    nonTermTransitionId= nodeToId (c.Builder.mkOr [finalNode; initial])
                    // nonTermTransitionId= nodeToId (node)
                    )
                )
        | _ -> None
    | _ -> None


let rec tryGetPendingNullable
    (canBeNull:RegexNode<_> -> bool)
    (node:RegexNode<_>): struct(bool * bool * int) // canBeNull, isPositive, relativePos,
        =
    let baseCase = struct(false,true,0)
    match node with
    // pos. lookahead
    | LookAround(node=lookBody; lookBack=false; negate=false; relativeNullablePos=relativeNullablePos) ->
        let canBeNull = canBeNull lookBody
        (canBeNull,true,relativeNullablePos)
            // struct(bool * bool * int)
    // neg. lookahead
    | LookAround(node=lookBody; lookBack=false; negate=true; relativeNullablePos=relativeNullablePos) ->
        let canBeNull = canBeNull lookBody
        (canBeNull,false,relativeNullablePos)
    | Or(nodes, info) ->
        let pendingNullables =
            nodes
            |> Seq.map (tryGetPendingNullable canBeNull)
            |> Seq.where (fun v -> v <> baseCase)
            |> Seq.toArray
        match pendingNullables with
        | [||] -> (false,true,0)
        | [| single |] -> single
        | atleastOne ->
            let h = atleastOne[0]
            match Seq.forall (fun v -> v = h) atleastOne with
            | true -> h
            | _ ->
                failwith "todo: complex anchor conditions"
    | _ ->
        (false,true,0)

let rec nodeWithoutLookbackPrefix
    (node:RegexNode<_>) =
    match node with
    | Concat(head=LookAround(lookBack = true); tail=tail) ->
        nodeWithoutLookbackPrefix tail
    | _ ->
        node