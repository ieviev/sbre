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
    | StringPrefixCaseIgnore of headSet:SearchValues<char> * tailSet:SearchValues<char> * prefix:Memory<char> * isAscii:bool * transitionNodeId:int
    // | StringPrefixCaseIgnore of engine:System.Text.RegularExpressions.Regex * transitionNodeId:int
    | SearchValuesPrefix of prefix:Memory<SearchValues<char>> * transitionNodeId:int
    /// ex. [Tt][Ww][Aa][Ii][Nn] ==> (ε|(?i)Twain)
    | SetsPrefix of prefix:Memory<TSet> * transitionNodeId:int
    /// ex. (Twain|Huck) ==> potential start:[TH][wu][ac][ik]
    | PotentialStartPrefix of prefix:Memory<TSet>
    | PotentialStartSingle of prefix:SearchValues<char> * inverted:bool
    // | DebugWordBorderPrefix of prefix:Memory<TSet> * transitionNodeId:int

type ActiveBranchOptimizations =
    | LimitedSkip of distance:int * termPred:SearchValues<char> * termTransitionId:int * nonTermTransitionId:int
    | PossibleStringPrefix of prefix:Memory<char> * transitionNodeId:int
    | NoOptimizations


type LengthLookup =
    | FixedLength of length:int
    | FixedLengthSetLookup of lookup:(Memory<TSet>*int)[]
    | PrefixMatchEnd of prefixLength:int * transitionId:int
    | MatchEnd

/// override trivial literal string search
[<RequireQualifiedAccess>]
type OverrideRegex =
    | FixedLengthString of string:Memory<char>
    | FixedLengthStringCaseIgnore of firstSet:SearchValues<char> * string:Memory<char> * isAscii:bool

#if DEBUG
let printPrefixSets (cache:RegexCache<_>) (sets:TSet list) =
    sets
    |> Seq.map cache.PrettyPrintMinterm
    |> String.concat ";"

let printPrettyDerivs (cache:RegexCache<_>) (derivs) =
    derivs
    |> (Array.map (fun (mt, node) -> $"{cache.PrettyPrintMinterm(mt), -13} ==> {node.ToString()}"))
    |> String.concat "\n"
    |> (fun v -> "\n" + v)
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
    | And(nodes, info) ->
        let existsComplement =
            nodes
            |> Seq.exists (function | Not _ -> true | _ -> false )

        if not existsComplement then

            let prefixes =
                nodes
                |> Seq.choose (fun v ->
                    match v with
                    | Concat(head=Singleton p) ->
                        Some v
                    | _ -> None
                )
                |> Seq.toArray

            if prefixes.Length > 0 then
                cache.Builder.mkOrSeq(prefixes), None
            else

            let trimmed =
                nodes
                |> Seq.map (getPrefixNodeAndComplement cache)
                |> Seq.toArray
            let noComplements = trimmed |> Seq.forall (fun v -> (snd v).IsNone)
            if noComplements then
                cache.Builder.mkOrSeq(trimmed |> Seq.map fst), None
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
            |> cache.Builder.mkOrSeq
            |> Some

        let trimmed =
            nonComplementNodes
            |> Seq.map (getPrefixNodeAndComplement cache)
            |> Seq.toArray

        let noComplements =
            trimmed
            |> Seq.forall (fun v -> (snd v).IsNone)

        if noComplements then
            cache.Builder.mkOrSeq(trimmed |> Seq.map fst |> Seq.toArray), complement
        else
            cache.Builder.mkAnd(nonComplementNodes), complement
    | _ -> node, None


let rec calcPrefixSets getNonInitialDerivative (getStateFlags: RegexNode<_> -> RegexStateFlags) (cache: RegexCache<_>) (startNode: RegexNode<_>) =
    let redundant = System.Collections.Generic.HashSet<RegexNode<TSet>>([ cache.False; startNode ])

    // nothing to complement if a match has not started
    let prefixStartNode, complementStartset =
        getPrefixNodeAndComplement cache startNode

    let rec loop (acc:TSet list) node =
        let prefix_derivs =
            getNonRedundantDerivatives getNonInitialDerivative cache redundant node
            |> Seq.toArray

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
    if startNode.DependsOnAnchor then [] else // this should never really happen
    let redundant = System.Collections.Generic.HashSet<RegexNode<TSet>>(tsetComparer)
    redundant.Add(cache.False) |> ignore
    let nodes = HashSet(tsetComparer)
    let tempList = ResizeArray()
    let rec loop (acc: TSet list) =
        tempList.Clear()
        if
            // nodes.Count > 6
            nodes.Count = 0 then acc |> List.rev else
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

let rec applyPrefixSetsWhileNotNullable
    getNonInitialDerivative
    (cache:RegexCache<_>)
    (node:RegexNode<TSet>)
    (sets:TSet list) =
    if node.CanBeNullable then node, sets.Length else
    // assert (not node.ContainsLookaround)
    match sets with
    | [] -> node, sets.Length
    | head :: tail ->
        let der = getNonInitialDerivative (head, node)
        if refEq node der then failwith "bug"
        applyPrefixSetsWhileNotNullable getNonInitialDerivative cache der tail

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
        let caseInsensitivePrefixes =
                prefix
                |> Seq.map (fun v ->
                    // negated set
                    if Solver.elemOfSet v mts[0] then None else
                    let chrs = c.MintermChars(v)
                    chrs |> Option.bind (fun chrs ->
                        if chrs.Length = 1 then Some (chrs.Span[0]) else

                        let up c = Char.IsUpper c || Char.IsWhiteSpace c
                        let low c = Char.IsLower c || Char.IsWhiteSpace c

                        if (chrs.Length = 2) &&
                           ((up chrs.Span[0] && low chrs.Span[1])
                            || (low chrs.Span[0] && up chrs.Span[1]) )
                        then Some (chrs.Span[0]) else
                            match chrs.Length with
                            | 3 ->
                                // TODO: unsure if there are any more caseinsensitive cases like this
                                match chrs.ToArray() with
                                | [|'K';'k';'K'|] -> Some chrs.Span[0]
                                | _ -> None
                            | _ -> None
                    )
                )
                |> Seq.takeWhile Option.isSome
                |> Seq.choose id
                |> Seq.rev
                |> Seq.toArray
                |> Memory
        if caseInsensitivePrefixes.Length > 1 then
            let applied = Optimizations.applyPrefixSets getNonInitialDerivative c trueStarredNode (List.take caseInsensitivePrefixes.Length prefix)
            let tailSet = prefix |> List.head |> c.MintermSearchValues |> Option.defaultWith (fun v -> failwith "bug: invalid startset")
            let headSet = prefix |> List.last |> c.MintermSearchValues |> Option.defaultWith (fun v -> failwith "bug: invalid startset")
            let allAscii = caseInsensitivePrefixes |> Memory.forall Char.IsAscii
            // reverse prefix search is very slow in the span API so we use this
            // let spanString = caseInsensitivePrefixes.ToString()
            // let engine = System.Text.RegularExpressions.Regex(spanString, Text.RegularExpressions.RegexOptions.RightToLeft ||| Text.RegularExpressions.RegexOptions.IgnoreCase ||| Text.RegularExpressions.RegexOptions.Compiled)
            InitialOptimizations.StringPrefixCaseIgnore(headSet,tailSet,caseInsensitivePrefixes, allAscii,nodeToId applied)
        else

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
            // let chr1 = c.MintermSearchValues(potentialStart[0])
            // chr1
            // |> Option.map (fun v ->
            //     let inverted = c.MintermIsInverted(potentialStart[0])
            //     InitialOptimizations.PotentialStartSingle(v,inverted)
            // )
            // // None
            // |> Option.defaultWith (fun v ->
            //     let mem = Memory(Seq.truncate 4 potentialStart |> Seq.toArray)
            //     InitialOptimizations.PotentialStartPrefix(mem)
            // )
            // let mem = Memory(Seq.truncate 5 potentialStart |> Seq.toArray)
            let mem = Memory(potentialStart |> Seq.toArray)
            InitialOptimizations.PotentialStartPrefix(mem)
        | _ -> InitialOptimizations.NoOptimizations


let tryGetLimitedSkip getNonInitialDerivative
    (getStateFlags: RegexNode<_> -> RegexStateFlags)
    (nodeToId:RegexNode<TSet> -> int)
    (getStartset:RegexNode<_> -> TSet)
    (c:RegexCache<_>)
    (revTrueStarNode:RegexNode<_>)
    (node:RegexNode<_>) =
    assert(not node.ContainsLookaround)
    let redundant = HashSet([revTrueStarNode; ])
    let skipTerm = getStartset revTrueStarNode // m.GetOrCreateState(initial).Startset
    match node with
    | Or(nodes, info) when info.CanNotBeNullable() ->
        let nonInitial = nodes |> Seq.where (fun v -> not (refEq v revTrueStarNode)) |> Seq.toArray |> c.Builder.mkOrSeq
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
                    nonTermTransitionId= nodeToId (c.Builder.mkOrSeq [|finalNode; revTrueStarNode|])
                    )
                )
        // alternate
        | [| (m1,path1); (m2,path2) |] when refEq c.False path1 || refEq c.False path2  ->
            let nonFalsePath = if refEq c.False path1 then (m2,path2) else (m1,path1)

            let prefix =
                Optimizations.calcPrefixSets
                    getNonInitialDerivative
                    getStateFlags
                    c
                    (snd nonFalsePath)

            let prefix = (fst nonFalsePath) :: prefix

            let singleCharPrefixes =
                prefix
                |> Seq.map (fun v ->
                    if c.MintermIsInverted(v) then None else
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

                // let pretty = printPrefixSets c prefix
                let applied, reducedLength =
                    Optimizations.applyPrefixSetsWhileNotNullable
                        getNonInitialDerivative
                        c node (List.take singleCharPrefixes.Length (prefix))
                if reducedLength > 0 then failwith "todo optimization bug"

                Some (ActiveBranchOptimizations.PossibleStringPrefix(singleCharPrefixes,nodeToId applied))

            else
            // TODO: optimization potential here
            None

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
                    nonTermTransitionId= nodeToId (c.Builder.mkOrSeq [|finalNode; revTrueStarNode|])
                    // nonTermTransitionId= nodeToId (node)
                    )
                )
        | _ -> None
    | _ -> None


let rec mkNodeWithoutLookbackPrefix
    (b:RegexBuilder<_>)
    (node:RegexNode<_>) =
    match node with
    | LookAround(lookBack = true) -> Epsilon
    | Begin | End -> Epsilon
    | Concat(head=LookAround(lookBack = true); tail=tail) ->
        mkNodeWithoutLookbackPrefix b tail
    | Concat(head=head; tail=tail) ->
        let convertedHead =
            mkNodeWithoutLookbackPrefix b head
            // nodes |> Seq.map (mkNodeWithoutLookbackPrefix b)
            // |> Seq.toArray
        match convertedHead with
        | Epsilon -> tail
        | _ ->
            node
    | Or(nodes=xs) ->
        xs
        |> Seq.map (mkNodeWithoutLookbackPrefix b)
        |> Seq.toArray
        |> b.mkOrSeq
    | And(nodes=xs) | Or(nodes=xs) ->
        xs
        |> Seq.map (mkNodeWithoutLookbackPrefix b)
        |> b.mkAnd
    | Not(_) ->
        // assert (not node.ContainsLookaround)
        node
    | _ ->
        node


let attemptMergeIntersectLang (_cache:RegexCache<TSet>) (mkLang: RegexNode<TSet> -> RegexNode<TSet>[]) (oldNode:RegexNode<TSet>) (languages:RegexNode<TSet> array seq)  =
    languages
    |> Seq.reduce (fun (lang1) (lang2) ->
        Seq.zip lang1 lang2
        |> Seq.indexed
        |> Seq.map (fun (idx,(l1,l2)) ->
            match l1, l2 with
            | n1, n2 | n2, n1 when refEq n1 n2 -> n1
            | f, _ | _, f when refEq f _cache.False -> _cache.False
            | n1, n2 | n2, n1 when refEq n1 _cache.TrueStar -> n2
            | n1, n2 | n2, n1 when refEq n1 _cache.Eps -> if n2.CanBeNullable then _cache.Eps else _cache.False
            // --
            | SingletonStarLoop(pred) as p1, other | other, (SingletonStarLoop(pred) as p1) ->
                let sub = Solver.containsS _cache.Solver pred (other.SubsumedByMinterm(_cache.Solver))
                if sub then other else _cache.Builder.mkAnd2(l1,l2)
            | _ ->
                let newNode = _cache.Builder.mkAnd2(l1,l2)
                _cache.Builder.GetCanonical(newNode,mkLang newNode,(fun v -> newNode))
                // infinite loop danger
                // let sublang1 = mkLang l1
                // let sublang2 = mkLang l2
                // // let merged = attemptMergeIntersectLang _cache mkLang oldNode [sublang1;sublang2]
                // let mknode = (fun _ -> _cache.Builder.mkAnd2Direct(l1,l2) )
                // let canonical = _cache.Builder.GetCanonical(oldNode,merged,mknode)
                // canonical
        )
        |> Seq.toArray
    )


let attemptMergeUnionLang (_cache:RegexCache<TSet>) (mkLang: RegexNode<TSet> -> RegexNode<TSet>[]) (oldNode:RegexNode<TSet>) (languages:RegexNode<TSet> array seq)  =
    languages
    |> Seq.reduce (fun (lang1) (lang2) ->
        Seq.zip lang1 lang2
        |> Seq.indexed
        |> Seq.map (fun (idx,(l1,l2)) ->
            match l1, l2 with
            | n1, n2 | n2, n1 when refEq n1 n2 -> n1
            | n1, other | other, n1 when refEq n1 _cache.False -> other
            | n1, n2 | n2, n1 when refEq n1 _cache.TrueStar -> _cache.TrueStar
            | n1, n2 | n2, n1 when refEq n1 _cache.Eps -> if n2.CanBeNullable then n2 else _cache.Builder.mkLoop(n2,0,1)
            // --
            | SingletonStarLoop(pred) as p1, other | other, (SingletonStarLoop(pred) as p1) ->
                let sub = Solver.containsS _cache.Solver pred (other.SubsumedByMinterm(_cache.Solver))
                if sub then p1 else _cache.Builder.mkOr2(l1,l2)
            | _ ->
                let newNode = _cache.Builder.mkOr2(l1,l2)
                let canon = _cache.Builder.GetCanonical(newNode,mkLang newNode,(fun v -> newNode))
                canon
        )
        |> Seq.toArray
    )


let rec getLengthMapping
    createNonInitialDerivative
    (c:RegexCache<TSet>)
    (node: RegexNode<TSet>) : LengthLookup
    =
    let redundant = HashSet([c.False])
    let rec loop (acc: (TSet array * int) list) (remainingTransitions: (TSet list * TSet * RegexNode<TSet>)[])  =
        let transitions =
            remainingTransitions
            |> Seq.map (fun (preceding, tset, derivative) ->
                let fixLengthOption = Info.Node.getFixedLength derivative
                let newPrecedingList = preceding @ [ tset ]

                match fixLengthOption with
                | None ->
                    // continue looking
                    Result.Error(newPrecedingList, derivative)
                | Some(fixLength) -> Result.Ok(newPrecedingList, fixLength)
            )
            |> Seq.toArray

        let updatedAcc =
            transitions
            |> Seq.choose (fun res ->
                match res with
                | Error _ -> None
                | Ok(prec, len) -> Some(Seq.toArray prec, (len + prec.Length))
            )
            |> Seq.toList
            |> List.append acc

        try
            transitions
            |> Seq.fold
                (fun (acc: (TSet array * int) list) res ->
                    match res with
                    | Error(prec, der) ->
                        let newTransitions =
                            der
                            |> getNonRedundantDerivatives createNonInitialDerivative c redundant
                            |> Seq.map (fun (v1, v2) -> prec, v1, v2)
                            |> Seq.toArray

                        match newTransitions with
                        | [||] -> acc
                        | _ when prec.Length > 4 -> failwith "could not infer length prefix"
                        | _ -> loop acc newTransitions
                    | Ok(_) -> acc
                )
                updatedAcc
        with e -> []

    let nonRedundant =
        node
        |> Optimizations.getNonRedundantDerivatives createNonInitialDerivative c redundant |> Seq.toArray

    let initial: (TSet list * TSet * RegexNode<TSet>) array =
        nonRedundant
        |> Seq.map (fun (v1,v2) -> [],v1,v2)
        |> Seq.toArray
    let result = loop [] initial
    match result with
    | [] -> LengthLookup.MatchEnd
    | _ ->
        result
        |> Seq.map (fun (pref,len) -> Memory(pref),len )
        |> Seq.toArray
        |> LengthLookup.FixedLengthSetLookup


let inferLengthLookup
    createNonInitialDerivative
    (c:RegexCache<TSet>)
    (node: RegexNode<TSet>)
    =
    Info.Node.getFixedLength node
    |> Option.map LengthLookup.FixedLength
    |> Option.defaultWith (fun _ ->
        getLengthMapping createNonInitialDerivative c node
    )

let inferOverrideRegex
    (initialOptimizations:InitialOptimizations)
    (lengthLookup:LengthLookup)
    (c:RegexCache<TSet>)
    (node: RegexNode<TSet>) : OverrideRegex option
    =
    if node.DependsOnAnchor then None else
    match lengthLookup, initialOptimizations with
    | LengthLookup.FixedLength(fl), InitialOptimizations.StringPrefixCaseIgnore(headSet,tailSet,prefix,ascii, _) when fl = prefix.Length ->
        Some (OverrideRegex.FixedLengthStringCaseIgnore(headSet,prefix,ascii))
    | LengthLookup.FixedLength(fl), InitialOptimizations.StringPrefix(prefix,_) when fl = prefix.Length ->
        Some (OverrideRegex.FixedLengthString(prefix))
    | _ ->
        None
