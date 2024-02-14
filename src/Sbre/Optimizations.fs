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
    | DebugWordBorderPrefix of prefix:Memory<TSet> * transitionNodeId:int

type ActiveBranchOptimizations =
    | LimitedSkip of distance:int * termPred:SearchValues<char> * termTransitionId:int * nonTermTransitionId:int
    | NoOptimizations




#if DEBUG
let printPrefixSets (cache:RegexCache<_>) (sets:TSet list) =
    sets
    |> Seq.map cache.PrettyPrintMinterm
    |> String.concat ";"
#endif

let getImmediateDerivatives createNonInitialDerivative (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    cache.Minterms()
    |> Seq.map (fun minterm ->
        let der = createNonInitialDerivative (minterm, node)
        minterm, der
    )
let getImmediateDerivativesMerged (createNonInitialDerivative) (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    cache.Minterms()
    |> Seq.map (fun minterm ->
        let der = createNonInitialDerivative (minterm, node)
        minterm, der
    )
    |> Seq.groupBy snd
    |> Seq.map (fun (_, group) ->
        group |> Seq.map fst |> Seq.fold (|||) cache.Solver.Empty, group |> Seq.head |> snd
    )

let getNonRedundantDerivatives
    getNonInitialDerivative
    (cache: RegexCache<TSet>)
    (redundantNodes: System.Collections.Generic.HashSet<RegexNode<TSet>>)
    (node: RegexNode<TSet>)
    =
    getImmediateDerivativesMerged getNonInitialDerivative cache node
    |> Seq.where (fun (mt, deriv) -> not (redundantNodes.Contains(deriv)))

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
                cache.Builder.mkOr(trimmed |> Array.map fst), None
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
            |> Seq.toArray
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
            cache.Builder.mkOr(trimmed |> Seq.map fst |> Seq.toArray), complement
        else
            cache.Builder.mkAnd(nonComplementNodes), complement
    | _ -> node, None


let rec calcPrefixSets getNonInitialDerivative (getStateFlags: RegexNode<_> -> RegexStateFlags) (cache: RegexCache<_>) (startNode: RegexNode<_>) =
    let redundant = System.Collections.Generic.HashSet<RegexNode<TSet>>([ cache.False; startNode ])

    // nothing to complement if a match has not started
    let prefixStartNode, complementStartset =
        getPrefixNodeAndComplement cache startNode


    let rec loop (acc:TSet list) node =
        // let isRedundant = not (redundant.Add(node))
        // let all_derivs = getImmediateDerivativesMerged cache node |> Seq.toArray
        let prefix_derivs =
            getNonRedundantDerivatives getNonInitialDerivative cache redundant node
            |> Seq.toArray

        // a -> t
        // if stateFlags.CanSkip && not (refEq startNodeWithoutComplement node) then acc |> List.rev else
        if not acc.IsEmpty && redundant.Contains(node) then
            acc |> List.rev
        elif node.CanBeNullable  then
            acc |> List.rev
        else
            // let pretty =
            //     prefix_derivs
            //     |> (Array.map (fun (mt,node) ->
            //         cache.PrettyPrintMinterm(mt), node
            //     ))
            //     |> Seq.toArray

            match prefix_derivs with
            | [| (mt, deriv) |]  ->
                if refEq deriv node then [] else // anchor infinite loop
                // stop with pending nullable
                let acc' = mt :: acc
                loop acc' deriv
            | _ ->
                acc |> List.rev
    let prefix = loop [] prefixStartNode

    match complementStartset with
    | _ when prefix.IsEmpty -> prefix
    | None -> prefix
    | Some compl ->
        let complementStartset = calcPrefixSets getNonInitialDerivative getStateFlags cache compl
        let trimmedPrefix =
            if complementStartset.Length = 0 then [] else
            prefix
            |> Seq.takeWhile (fun v ->

                not (cache.Solver.isElemOfSet(v,complementStartset[0]))
            )
            |> Seq.toList
        if trimmedPrefix.IsEmpty then
            [prefix[0]] else trimmedPrefix



let rec calcPotentialMatchStart getNonInitialDerivative (getStateFlags: RegexNode<_> -> RegexStateFlags) (cache: RegexCache<_>) (startNode: RegexNode<_>) =
    let redundant = System.Collections.Generic.HashSet<RegexNode<TSet>>(tsetComparer)
    redundant.Add(cache.False) |> ignore
    let nodes = HashSet(tsetComparer)
    let tempList = ResizeArray()
    let rec loop (acc: TSet list) =
        tempList.Clear()
        if nodes.Count > 4 || acc.Length > 5 || nodes.Count = 0 then acc |> List.rev else
        let shouldExit = nodes |> Seq.exists (_.CanBeNullable)
        if shouldExit then acc |> List.rev else

        nodes
        |> Seq.map (getNonRedundantDerivatives getNonInitialDerivative cache redundant)
        |> Seq.iter (tempList.Add)

        let merged_pred =
            tempList
            |> Seq.collect id
            |> Seq.map fst
            |> Seq.fold (|||) cache.Solver.Empty

        // let pretty =
        //     prefixDerivsList
        //     |> Seq.map (Array.map (fun (mt,node) ->
        //         cache.PrettyPrintMinterm(mt), node
        //     ))
        //     |> Seq.toArray

        nodes.Clear()
        tempList
        |> Seq.iter (fun tmp ->
            tmp |> Seq.iter (fun v ->
                nodes.Add(snd v) |> ignore
            )
        )
        loop (merged_pred :: acc)

    let prefixStartNode, complementStartset = getPrefixNodeAndComplement cache startNode
    nodes.Add(prefixStartNode) |> ignore
    redundant.Add(prefixStartNode) |> ignore

    match complementStartset with
    | Some c -> [] // todo
    | None -> loop []




let rec applyPrefixSets getNonInitialDerivative (cache:RegexCache<_>) (node:RegexNode<TSet>) (sets:TSet list) =
    // assert (not node.ContainsLookaround)
    match sets with
    | [] -> node
    | head :: tail ->
        let der = getNonInitialDerivative (head, node)
        applyPrefixSets getNonInitialDerivative cache der tail

let findInitialOptimizations
    getNonInitialDerivative
    (nodeToId:RegexNode<TSet> -> int)
    (nodeToStateFlags:RegexNode<TSet> -> RegexStateFlags)
    (c:RegexCache<TSet>) (node:RegexNode<TSet>) (trueStarredNode:RegexNode<TSet>) =
#if NO_SKIP_LOOKAROUNDS
    if node.ContainsLookaround then InitialOptimizations.NoOptimizations else
#endif
    match Optimizations.calcPrefixSets getNonInitialDerivative nodeToStateFlags c node with
    | prefix when prefix.Length > 1 ->

        let mts = c.Minterms()
        let singleCharPrefixes =
            prefix
            |> Seq.map (fun v ->
                // negated set
                if Solver.elemOfSet v mts[0] then None else
                let chrs = c.MintermChars(v)
                chrs |> Option.bind (fun chrs ->
                    if chrs.Length = 1 then Some (chrs.Span[0]) else None
                )

            )
            |> Seq.takeWhile Option.isSome
            |> Seq.choose id
            |> Seq.rev
            |> Seq.toArray
            |> Memory
        if singleCharPrefixes.Length > 1 then
            let applied = Optimizations.applyPrefixSets getNonInitialDerivative c trueStarredNode (List.take singleCharPrefixes.Length prefix)
            InitialOptimizations.StringPrefix(singleCharPrefixes,nodeToId applied)
        else
            // fail if set too large
//             if smallPrefix.Length = 0 then
// #if EXPERIMENTAL_LOOKAROUNDS
//                 let mem = Memory(Seq.toArray prefix)
//                 let applied = Optimizations.applyPrefixSets getNonInitialDerivative c trueStarredNode prefix
//                 InitialOptimizations.DebugWordBorderPrefix(mem,nodeToId applied)
// #else
//                 InitialOptimizations.NoOptimizations
// #endif
//             else

            let applied = Optimizations.applyPrefixSets getNonInitialDerivative c trueStarredNode prefix
            let containsSmallSets =
                prefix |> Seq.forall (fun v -> not (c.MintermIsInverted(v)))
                && prefix |> Seq.forall (fun v ->
                    let chrs = c.MintermChars(v)
                    chrs.IsSome && chrs.Value.Length <= 5
                )
            if containsSmallSets then
                let searchPrefix = prefix |> Seq.map c.MintermSearchValues |> Seq.toArray
                if searchPrefix |> Seq.exists (fun v -> v.IsNone) then
                    let mem = Memory(Seq.toArray prefix)
                    InitialOptimizations.SetsPrefix(mem,nodeToId applied)
                else
                    let searchPrefix = Array.choose id searchPrefix |> Memory
                    InitialOptimizations.SearchValuesPrefix(searchPrefix,nodeToId applied)
            else
                let mem = Memory(Seq.toArray prefix)
                InitialOptimizations.SetsPrefix(mem,nodeToId applied)
    | _ ->
        match Optimizations.calcPotentialMatchStart getNonInitialDerivative nodeToStateFlags c node with
        | potentialStart when potentialStart.Length > 0 ->
            let mem = Memory(Seq.toArray potentialStart)
            InitialOptimizations.PotentialStartPrefix(mem)
        | _ -> InitialOptimizations.NoOptimizations


let tryGetLimitedSkip getNonInitialDerivative (nodeToId:RegexNode<TSet> -> int) (getStartset:RegexNode<_> -> TSet) (c:RegexCache<_>) (initial:RegexNode<_>) (node:RegexNode<_>) =
    assert(not node.ContainsLookaround)
    let redundant = HashSet([initial; ])
    let skipTerm = getStartset initial // m.GetOrCreateState(initial).Startset
    match node with
    | Or(nodes, info) ->
        let nonInitial = nodes |> Seq.where (fun v -> not (refEq v initial)) |> Seq.toArray |> c.Builder.mkOr
        let nonTermDerivatives (node: RegexNode<TSet>) =
            let ders1 = Optimizations.getNonRedundantDerivatives getNonInitialDerivative c redundant node
            ders1 |> Seq.where (fun (mt,_) -> not (Solver.contains skipTerm mt) ) |> Seq.toArray

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
                if chrs.IsNone || chrs.Value.Length > 100 then
                    None
                else
                let searchValuesSet =
                    c.MintermSearchValues(skipTerm)
                searchValuesSet
                |> Option.map (fun searchValuesSet ->
                    ActiveBranchOptimizations.LimitedSkip(
                    distance=path.Count + 1,
                    termPred= searchValuesSet,
                    termTransitionId=nodeToId (getNonInitialDerivative (skipTerm, node)),
                    nonTermTransitionId= nodeToId (c.Builder.mkOr [|finalNode; initial|])
                    )
                )
        | _ -> None
    | Concat(_) ->
        let nonTermDerivatives (node: RegexNode<TSet>) =
            let ders1 = Optimizations.getNonRedundantDerivatives getNonInitialDerivative c redundant node
            ders1 |> Seq.where (fun (mt,_) -> not (Solver.contains skipTerm mt) ) |> Seq.toArray
        let nonInitialNonTerm = nonTermDerivatives node

        match nonInitialNonTerm with
        | [| singlePath |] ->
            let path = ResizeArray()
            let rec loop (node: RegexNode<_>) =
                match nonTermDerivatives node with
                | [| (mt,single) |] when (not (node.CanBeNullable || refEq c.False node || c.Solver.IsFull(mt))) ->
                    redundant.Add(node) |> ignore
                    path.Add(mt)
                    if path.Count > 25 then
                        ()
                    loop single
                | _ -> node
            let finalNode = loop (snd singlePath)
            if path.Count < 2 then None else
                if c.MintermIsInverted(skipTerm) then None else
                    // failwith "todo: inverted minterm"
                let chrs = c.MintermChars(skipTerm)
                if chrs.IsNone || chrs.Value.Length > 100 then
                    None else
                let searchValuesSet =
                    c.MintermSearchValues(skipTerm)
                searchValuesSet
                |> Option.map (fun searchValuesSet ->
                    ActiveBranchOptimizations.LimitedSkip(
                    distance=path.Count + 1,
                    termPred= searchValuesSet,
                    termTransitionId=nodeToId (getNonInitialDerivative (skipTerm, node)),
                    nonTermTransitionId= nodeToId (c.Builder.mkOr [|finalNode; initial|])
                    // nonTermTransitionId= nodeToId (node)
                    )
                )
        | _ -> None
    | _ -> None


// let rec collectPendingNullables
//     (canBeNull:RegexNode<_> -> bool)
//     (node:RegexNode<_>): Set<int> // canBeNull, isPositive, relativePos,
//         =
//     match node with
//     // pos. lookahead
//     | LookAround(node=lookBody; lookBack=false; relativeTo= rel; pendingNullables=relativeNullablePos) ->
//         let canBeNull = canBeNull lookBody
//         if canBeNull && not relativeNullablePos.IsEmpty then
//             relativeNullablePos
//             |> Seq.map (fun v -> rel + v ) |> set
//         else Set.empty
//     | Or(nodes, info) ->
//         let pendingNullables =
//             nodes
//             |> Seq.collect (collectPendingNullables canBeNull)
//         Set.ofSeq pendingNullables
//     | And(nodes, regexNodeInfo) ->
//         let pendingNullables =
//             nodes
//             |> Seq.collect (collectPendingNullables canBeNull)
//         Set.ofSeq pendingNullables
//     | Loop(node=node) | Not (node=node) ->
//         let pending = (collectPendingNullables canBeNull) node
//         if not pending.IsEmpty then
//             failwith $"todo: collect pending nullables inside: {node}"
//         else Set.empty
//     | Epsilon | Anchor _ | Singleton _ -> Set.empty
//     | LookAround(_) | Concat(head=LookAround(_)) -> Set.empty
//     | Concat(info=info) when info.CanNotBeNullable() -> Set.empty
//     | Concat(head=head; tail=tail) when head.IsAlwaysNullable ->
//         let pendingTail = collectPendingNullables canBeNull tail
//         pendingTail
//     | Concat(head=Anchor _; tail=tail) ->
//         let pendingTail = collectPendingNullables canBeNull tail
//         pendingTail
//     | Concat(head=head; tail=tail) ->
//         let pendingTail = collectPendingNullables canBeNull tail
//         pendingTail


let rec nodeWithoutLookbackPrefix
    (b:RegexBuilder<_>)
    (node:RegexNode<_>) =
    match node with
    | LookAround(lookBack = true) -> Epsilon
    | Anchor _ -> Epsilon
    | Concat(head=LookAround(lookBack = true); tail=tail) ->
        nodeWithoutLookbackPrefix b tail
    | Concat(head=head; tail=tail) ->
        let convertedHead =
            nodeWithoutLookbackPrefix b head
            // nodes |> Seq.map (nodeWithoutLookbackPrefix b)
            // |> Seq.toArray
        match convertedHead with
        | Epsilon -> tail
        | _ ->
            node
    | Or(nodes=xs) ->
        xs
        |> Seq.map (nodeWithoutLookbackPrefix b)
        |> Seq.toArray
        |> b.mkOr
    | And(nodes=xs) | Or(nodes=xs) ->
        xs
        |> Seq.map (nodeWithoutLookbackPrefix b)
        |> b.mkAnd
    | Not(_) ->
        // assert (not node.ContainsLookaround)
        node
    | _ ->
        node

