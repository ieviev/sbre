#if RELEASE
module internal rec Sbre.Optimizations
#else
module rec Sbre.Optimizations
#endif

open Sbre.Common
open System.Buffers
open System.Collections.Generic
open System.Text
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Algorithm
open Sbre.Types
open Sbre.Pat
open Sbre.Cache
open System


module Overrides =
    let locateStringsUtf16 (acc:byref<SharedResizeArrayStruct<MatchPosition>>) (input:ReadOnlySpan<char>) (str:ReadOnlySpan<char>) =
        let mutable looping = true
        let mutable currPos = 0
        let strLen = str.Length
        while looping do
            match input.Slice(currPos).IndexOf(str, StringComparison.Ordinal) with
            | -1 -> looping <- false
            | n ->
                let start = currPos + n
                acc.Add({ MatchPosition.Index = start; Length = strLen })
                currPos <- start + strLen
                // if currPos > limit then
                //     looping <- false

    let locateStringsByte (acc:byref<SharedResizeArrayStruct<MatchPosition>>) (input:ReadOnlySpan<byte>) (str:Span<byte>) =
        let mutable looping = true
        let mutable currPos = 0
        let inputLen = input.Length
        let strLen = str.Length
        let limit = inputLen - strLen
        while looping do
            match input.Slice(currPos).IndexOf(str) with
            | -1 -> looping <- false
            | n ->
                let start = currPos + n
                acc.Add({ MatchPosition.Index = start; Length = strLen })
                currPos <- start + strLen


/// initial prefix optimizations
[<RequireQualifiedAccess>]
type InitialOptimizations<'t, 'tchar when 'tchar : struct and 'tchar :> IEquatable<'tchar>> =
    | NoOptimizations
    /// ex. Twain ==> (ε|Twain)
    | StringPrefix of prefix: Memory<'tchar> * transitionNodeId: int
    /// | StringPrefixCaseIgnore of engine:System.Text.RegularExpressions.Regex * transitionNodeId:int
    | StringPrefixCaseIgnore of prefix: Memory<'tchar> * isAscii:bool * transitionNodeId: int
    | SearchValuesPrefix of prefix: Memory<MintermSearchValues<'t>> * transitionNodeId: int
    /// potential start prefix from searchvalues
    | SearchValuesPotentialStart of prefix: Memory<MintermSearchValues<'t>>

/// active dfa state optimizations
/// e.g. a_*b can skip to position of 'b' after finding 'a'
type ActiveBranchOptimizations<'t, 'tchar when 'tchar : struct and 'tchar :> IEquatable<'tchar>> =
    | SkippableLookahead of todo: int
    | LimitedSkipOnePath of
        distance: int *
        skipPred: MintermSearchValues<'t> *
        failPred: MintermSearchValues<'t> *
        skipToEndTransitionId: int *
        cachedTransitions: Memory<int>
    | LimitedSkip2Chars of
        distance: int *
        skipPred: MintermSearchValues<'t> *
        failPred: MintermSearchValues<'t> *
        skipToEndTransitionId: int *
        cachedTransitions: int[]
    | NoOptimizations

/// the default option to find the match end is to match again in reverse
/// often this is overkill and can be replaced with something simpler
type LengthLookup<'t> =
    /// skip match end lookup entirely
    | FixedLength of length: int
    /// work in progress - maybe useless
    | FixedLengthSetLookup of lookup: (struct (Memory<'t> * int))[]
    /// skip some transitions as we already know where match starts
    /// e.g. hello.*world starts looking at hello|->.*end
    | FixedLengthPrefixMatchEnd of prefixLength: int * transitionId: int
    /// default match end lookup
    | MatchEnd

/// override for trivial literal string search
[<RequireQualifiedAccess>]
type OverrideRegex<'tchar when 'tchar : struct and 'tchar :> IEquatable<'tchar>> =
    | FixedLengthString of string: Memory<'tchar>
    | FixedLengthStringCaseIgnore of string: Memory<'tchar> * isAscii: bool

#if DEBUG
let printPrefixSets (cache: RegexCache<_>) (sets: uint64 list) =
    sets
    |> Seq.map cache.PrettyPrintMinterm
    |> Seq.map (fun v ->
        match v with
        | @"[^\n]" -> "."
        | c when c.Length > 25 -> "φ" // dont expand massive sets
        | c -> c
    )
    |> String.concat ";"

let printPrefixSets2 (cache: RegexCache<_>) (sets: BitVector list) =
    sets
    |> Seq.map (fun v -> (box cache.Solver :?> BitVectorSolver).PrettyPrint(v, cache.CharsetSolver) )
    |> Seq.map (fun v ->
        match v with
        | @"[^\n]" -> "."
        | c when c.Length > 25 -> "φ" // dont expand massive sets
        | c -> c
    )
    |> String.concat ";"

let printPrettyDerivs (cache: RegexCache<'t>) (derivs:('t*'a)[]) =
    derivs
    |> (Array.map (fun (mt, node) -> $"{cache.PrettyPrintMinterm(unbox mt), -13} ==> {node.ToString()}"))
    |> String.concat "\n"
    |> (fun v -> "\n" + v)
#endif

let getImmediateDerivatives
    createNonInitialDerivative
    (cache: RegexCache<_>)
    (node: RegexNode<'t>)
    =
    cache.Minterms()
    |> Seq.map (fun minterm ->
        let der = createNonInitialDerivative (minterm, node)
        minterm, der
    )

let getImmediateDerivativesMerged
    (createNonInitialDerivative)
    (cache: RegexCache<_>)
    (node: RegexNode<'t>)
    =
    cache.Minterms()
    |> Seq.map (fun minterm ->
        let der = createNonInitialDerivative (minterm, node)
        minterm, der
    )
    |> Seq.groupBy snd
    |> Seq.map (fun (_, group) ->

        group |> Seq.map fst |> Seq.fold (fun acc v -> cache.Solver.Or(acc,v) ) cache.Solver.Empty, group |> Seq.head |> snd
    )

let getNonRedundantDerivatives
    getNonInitialDerivative
    (cache: RegexCache<'t>)
    (redundantNodes: System.Collections.Generic.HashSet<RegexNode<'t>>)
    (node: RegexNode<'t>)
    =
    getImmediateDerivativesMerged getNonInitialDerivative cache node
    |> Seq.where (fun (mt, deriv) -> not (redundantNodes.Contains(deriv)))

/// strip parts irrelevant for prefix
let rec getPrefixNode
    (cache: RegexCache<'t>)
    (node: RegexNode<'t>)
    : RegexNode<'t>
    =
    match node with
    | Concat(Loop(low = 0; up = Int32.MaxValue), tail, _) -> getPrefixNode cache tail
    | Concat(Loop(node = body; low = n; up = Int32.MaxValue), tail, _) ->
        cache.Builder.mkConcat2 (cache.Builder.mkLoop (body, n, n), tail)
    | And(nodes, info) -> node
    | _ -> node


let rec calcPrefixSets
    getNonInitialDerivative
    (getStateFlags: RegexNode<'t> -> RegexStateFlags)
    (cache: RegexCache<'t>)
    (startNode: RegexNode<'t>) : 't list
    =
    let redundant =
        System.Collections.Generic.HashSet<RegexNode<'t>>([ cache.False; startNode ])

    // nothing to complement if a match has not started
    let prefixStartNode = getPrefixNode cache startNode

    let rec loop (acc: 't list) node =
        if (not acc.IsEmpty && redundant.Contains(node)) || node.CanBeNullable then
            acc |> List.rev
        else
            let prefix_derivs =
                getNonRedundantDerivatives getNonInitialDerivative cache redundant node
                |> Seq.toArray
            // let pretty =
            //     prefix_derivs
            //     |> (Array.map (fun (mt,node) ->
            //         cache.PrettyPrintMinterm(mt), node
            //     ))
            //     |> Seq.toArray
            match prefix_derivs with
            | [| (mt, deriv) |] ->
                if refEq deriv node then
                    []
                else // anchor infinite loop
                    // stop with pending nullable
                    let acc' = mt :: acc
                    loop acc' deriv
            | _ ->
                acc |> List.rev

    let prefix = loop [] prefixStartNode
    prefix



let rec calcPotentialMatchStart
    (options:SbreOptions)
    getNonInitialDerivative
    (getStateFlags: RegexNode<_> -> RegexStateFlags)
    (cache: RegexCache<_>)
    (startNode: RegexNode<_>)
    =
    if startNode.DependsOnAnchor then
        []
    else
        let redundant = System.Collections.Generic.HashSet<RegexNode<'t>>(tsetComparer)
        redundant.Add(cache.False) |> ignore
        let nodes = HashSet(tsetComparer)
        let tempList = new SharedResizeArray<_>(128)

#if DEBUG
        let backToBdd node =
            Minterms.transformBack
                cache.Builder
                cache.BddBuilder
                cache.Solver
                cache.CharsetSolver
                node
#endif
        let rec loop(acc: 't list) =
            tempList.Clear()
            if nodes.Count > options.FindPotentialStartSizeLimit || acc.Length > options.MaxPrefixLength || nodes.Count = 0 then
                acc |> List.rev
            else
                let shouldExit = nodes |> Seq.exists (_.CanBeNullable)

                if shouldExit then
#if DEBUG
                    // let p =
                    //     nodes
                    //     |> Seq.where (fun v -> v.CanBeNullable)
                    //     |> Seq.map (fun v -> v.CanBeNullable, backToBdd(v).ToStringLong())
                    //     |> Seq.toArray
#endif
                    acc |> List.rev
                else

                    let mutable ss = cache.Solver.Empty
                    for n in nodes do
                        let r = getNonRedundantDerivatives getNonInitialDerivative cache redundant n
                        for n in r do
                            ss <- cache.Solver.Or(ss,fst n)
                        tempList.Add(r)
                    // nodes
                    // |> Seq.map (getNonRedundantDerivatives getNonInitialDerivative cache redundant)
                    // |> Seq.iter (tempList.Add)

                    // let merged_pred =
                    //     let mutable ss = cache.Solver.Empty
                    //     for iseq in tempList do
                    //         for n in iseq do
                    //             ss <- cache.Solver.Or(ss,fst n)
                    //     ss

                    nodes.Clear()
                    for iseq in tempList do
                        iseq |> Seq.iter (fun v -> nodes.Add(snd v) |> ignore)
                    loop (ss :: acc)

        let prefixStartNode = getPrefixNode cache startNode
        nodes.Add(prefixStartNode) |> ignore
        redundant.Add(prefixStartNode) |> ignore
        let sets = loop []
        sets





let rec applyPrefixSets
    getNonInitialDerivative
    (cache: RegexCache<_>)
    (node: RegexNode<'t>)
    (sets: 't list)
    =
    match sets with
    | [] -> node
    | head :: tail ->
        let der = getNonInitialDerivative (head, node)
        applyPrefixSets getNonInitialDerivative cache der tail

let rec applyPrefixSetsWhileNotNullable
    getNonInitialDerivative
    (cache: RegexCache<_>)
    (node: RegexNode<'t>)
    (sets: 't list)
    =
    if node.CanBeNullable then
        node, sets.Length
    else
        match sets with
        | [] -> node, sets.Length
        | head :: tail ->
            let der = getNonInitialDerivative (head, node)
            applyPrefixSetsWhileNotNullable getNonInitialDerivative cache der tail


let caseInsensitivePrefixes prefix (c:RegexCache<_>) =
    let mts = c.Minterms()
    prefix
    |> Seq.map (fun v ->
        // negated set
        if c.Solver.elemOfSet v mts[0] then
            None
        else
            let chrs = c.MintermChars(v)
            chrs
            |> Option.bind (fun chrs ->
                if chrs.Length = 1 then
                    Some(chrs.Span[0])
                else
                    let up c = Char.IsUpper c
                    let low c = Char.IsLower c
                    if
                        (chrs.Length = 2)
                        && ((up chrs.Span[0] && low chrs.Span[1])
                            || (low chrs.Span[0] && up chrs.Span[1]))
                    then
                        Some(chrs.Span[0])
                    else
                        None
            )
        )
        |> Seq.takeWhile Option.isSome
        |> Seq.choose id
        |> Seq.rev
        |> Seq.toArray
        |> Memory

let findInitialOptimizations
    (options:SbreOptions)
    (getNonInitialDerivative: 't * RegexNode<'t> -> RegexNode<'t>)
    (nodeToId: RegexNode<'t> -> int)
    (nodeToStateFlags: RegexNode<'t> -> RegexStateFlags)
    (c: RegexCache<'t>)
    (node: RegexNode<'t>)
    (trueStarredNode: RegexNode<'t>): InitialOptimizations<'t,char>
    =
        match Optimizations.calcPrefixSets getNonInitialDerivative nodeToStateFlags c node with
        | prefix when prefix.Length > 1 ->
            let singleCharPrefixes =
                prefix
                |> Seq.map (fun v ->
                    if c.MintermIsInverted(v) then
                        None
                    else
                        let chrs = c.MintermChars(v)

                        chrs
                        |> Option.bind (fun chrs ->
                            if chrs.Length = 1 then Some(chrs.Span[0]) else None
                        )
                )
                |> Seq.takeWhile Option.isSome
                |> Seq.choose id
                |> Seq.rev
                |> Seq.toArray
                |> Memory

            if singleCharPrefixes.Length > 1 then
                let applied =
                    Optimizations.applyPrefixSets
                        getNonInitialDerivative
                        c
                        trueStarredNode
                        (List.take singleCharPrefixes.Length prefix)
                InitialOptimizations.StringPrefix(singleCharPrefixes, nodeToId applied)
            else

                let caseiprefix = caseInsensitivePrefixes prefix c
                if caseiprefix.Length > 1 then
                    let applied =
                        Optimizations.applyPrefixSets
                            getNonInitialDerivative
                            c
                            trueStarredNode
                            (List.take caseiprefix.Length prefix)
                    let isAscii =
                        caseiprefix |> Memory.forall (fun v -> Char.IsAscii(v))
                    InitialOptimizations.StringPrefixCaseIgnore(caseiprefix, isAscii, nodeToId applied)
                else



                let applied =
                    Optimizations.applyPrefixSets getNonInitialDerivative c trueStarredNode prefix

                let searchPrefix = prefix |> Seq.map c.MintermSearchValues |> Seq.toArray
                InitialOptimizations.SearchValuesPrefix(Memory(searchPrefix), nodeToId applied)

        | _ ->
            match
                Optimizations.calcPotentialMatchStart
                    options
                    getNonInitialDerivative
                    nodeToStateFlags
                    c
                    node
            with
            | potentialStart when potentialStart.Length > 0 ->
                let searchPrefix =
                    potentialStart |> Seq.map c.MintermSearchValues |> Seq.toArray |> Memory
                InitialOptimizations.SearchValuesPotentialStart(searchPrefix)
            | _ -> InitialOptimizations.NoOptimizations


let convertInitialOptimizations (initOpts:InitialOptimizations<'t,char>) : InitialOptimizations<'t,byte> =
    match initOpts with
    | InitialOptimizations.NoOptimizations -> InitialOptimizations.NoOptimizations
    | InitialOptimizations.StringPrefix(prefix, transitionNodeId) ->
        InitialOptimizations.StringPrefix(Memory.forceConvertToAscii prefix, transitionNodeId)
    | InitialOptimizations.SearchValuesPrefix(prefix, transitionNodeId) ->
        InitialOptimizations.SearchValuesPrefix(prefix, transitionNodeId)
    | InitialOptimizations.SearchValuesPotentialStart(prefix) ->
        if prefix |> Memory.forall (_.CanUseAscii() ) then
            InitialOptimizations.SearchValuesPotentialStart(prefix)
        else InitialOptimizations.NoOptimizations
    | InitialOptimizations.StringPrefixCaseIgnore(prefix, isAscii, transitionNodeId) ->
        InitialOptimizations.NoOptimizations

let tryGetLimitedSkip
    (options:SbreOptions)
    (getNonInitialDerivative:'t * RegexNode<_> -> RegexNode<_>)
    (getStateFlags: RegexNode<_> -> RegexStateFlags)
    (nodeToId: RegexNode<'t> -> int)
    (getStartset: RegexNode<_> -> 't)
    (c: RegexCache<_>)
    (revTrueStarNode: RegexNode<_>)
    (node: RegexNode<_>)
    =
    assert (not node.ContainsLookaround)
    let redundant = HashSet([ revTrueStarNode ])
    let mutable failTSet = getStartset revTrueStarNode
    let skipTermSize = c.MintermChars(failTSet)
    // todo: tune this
    if skipTermSize.IsNone || skipTermSize.Value.Length > 30 then
        None
    else
        match node with
        | Concat(_) ->
            let nonTermDerivatives(node: RegexNode<'t>) =
                let ders1 =
                    Optimizations.getNonRedundantDerivatives
                        getNonInitialDerivative
                        c
                        redundant
                        node

                ders1 |> Seq.where (fun (mt, _) -> not (c.Solver.contains failTSet mt)) |> Seq.toArray

            let nonTSetDerivatives (failTSet:'t) (node: RegexNode<'t>) =
                let ders1 =
                    Optimizations.getNonRedundantDerivatives
                        getNonInitialDerivative
                        c
                        redundant
                        node

                ders1 |> Seq.where (fun (mt, _) -> not (c.Solver.contains failTSet mt)) |> Seq.toArray

            let nonInitialNonTerm = nonTermDerivatives node

            let rec loop (skipMt:'t) (acc:'t list) (node: RegexNode<_>) =
                match nonTermDerivatives node with
                | [| (mt, single) |] when mt = skipMt &&
                    (not (node.CanBeNullable || refEq c.False node || c.Solver.IsFull(mt)))
                    ->
                    redundant.Add(node) |> ignore
                    loop skipMt (mt :: acc) single
                | _ -> (acc |> List.rev), node

            let rec loopN (skipMt:'t) (acc:'t list) (nodes: RegexNode<'t>[]) =
                let ders =
                    nodes
                    |> Seq.collect (nonTSetDerivatives skipMt)
                    |> Seq.distinct
                    |> Seq.toArray
                let allMts =
                    ders
                    |> Seq.map fst
                    |> Solver.mergeSets c.Solver
                let shouldExit =
                    ders
                    |> Seq.map snd
                    |> Seq.exists (fun v -> v.CanBeNullable || refEq c.False v)
                let canContinue =
                    skipMt = c.Solver.Not(allMts)
                if canContinue && not shouldExit then
                    redundant.Add(node) |> ignore
                    loopN skipMt (skipMt :: acc) (ders |> Array.map snd)
                else
                    (acc |> List.rev), node


            let findRemainingSkipOneBranch (startPred1:'t,startNode1) remaining =
                match remaining with
                | [| (startMt,potentialPath) |] when startPred1 = startMt ->
                    let path, skipToEndNode = loop startMt [] (potentialPath)
                    if path.Length = 0 then None else
                    let skipPred = c.MintermSearchValues(startMt)
                    let failPred = c.MintermSearchValues(c.Solver.Not(startMt))
                    // if path.Length < 20 then None else
                    Some(
                        ActiveBranchOptimizations.LimitedSkipOnePath(
                            // distance = path.Length + 2,
                            distance = path.Length + 2,
                            skipPred = skipPred,
                            failPred = failPred,
                            skipToEndTransitionId = nodeToId skipToEndNode,
                            cachedTransitions = Memory(Array.init (path.Length + 3) (fun v -> -1))
                        )
                    )
                | _ ->
                    None

            let findRemainingSkipOneBranchLeftToRight (failSet:'t) remaining =

                match remaining with
                | [| (p1,n1); (p2,n2) |] ->
                    let path, skipToEndNode = loopN failSet [] (Array.map snd remaining)
                    if path.Length = 0 then None else
                    let failSv = c.MintermSearchValues(failSet)
                    let skipSv = c.MintermSearchValues(c.Solver.Not(failSet))
                    // if path.Length < 20 then None else
                    Some(
                        ActiveBranchOptimizations.LimitedSkip2Chars(
                            distance = path.Length + 2, // +1 index, +2 already taken derivs
                            skipPred = skipSv,
                            failPred = failSv,
                            skipToEndTransitionId = nodeToId c.False, // should not be possible
                            cachedTransitions = Array.init (path.Length + 2) (fun v -> -1)
                        )
                    )
                | _ ->

                    None

            match nonInitialNonTerm with
            // p1 = (2, (⊤*cb{0,5}a)?)
            // p2 = (4, (⊤*cb{0,5}|b{0,4})a)
            | [| p1; p2 |] when not (refEq (snd p1) c.False) && not (refEq (snd p2) c.False)  ->
                None
            | [| p1 |] when not (refEq (snd p1) c.False) ->
                let immediatep1 = nonTermDerivatives (snd p1)
                findRemainingSkipOneBranch p1 immediatep1
            | _ ->
                    None


        | _ -> None

let findActiveBranchOptimizations
    (options:SbreOptions)
    (getNonInitialDerivative:'t * RegexNode<_> -> RegexNode<_>)
    (nodeToId: RegexNode<'t> -> int)
    (nodeToStateFlags: RegexNode<'t> -> RegexStateFlags)
    (c: RegexCache<'t>)
    (node: RegexNode<'t>)
    =
        raise (NotImplementedException())
        // let minterms = c.Minterms()
        //
        // let derivatives =
        //     minterms
        //     |> Array.map (fun minterm ->
        //         getNonInitialDerivative(minterm,node)
        //     )
        //
        // let ders = Array.zip minterms derivatives
        // let pretty = Optimizations.printPrettyDerivs c ders
        //
        // // todo: lots of potential here
        //
        // let condition = (fun d ->
        //     match d with
        //     | LookAround(node=dnode) ->
        //         not (refEq dnode node)
        // )
        //
        // let startsetPredicate =
        //     Seq.zip minterms derivatives
        //     |> Seq.where (fun (_, d) -> condition d)
        //     |> Seq.map fst
        //     |> Solver.mergeSets c.Solver
        //
        //
        //
        // let a = 1
        // failwith "todo"
        // ActiveBranchOptimizations.NoOptimizations

let rec mkNodeWithoutLookbackPrefix (b: RegexBuilder<_>) (node: RegexNode<_>) =
    match node with
    | LookAround(lookBack = true) -> Epsilon
    | Begin
    | End -> Epsilon
    | Concat(head = LookAround(lookBack = true); tail = tail) -> mkNodeWithoutLookbackPrefix b tail
    | Concat(head = head; tail = tail) when head.IsAlwaysNullable ->
        let convertedTail = mkNodeWithoutLookbackPrefix b tail
        b.mkConcat2(head,convertedTail)
    | Concat(head = head; tail = tail) ->
        let convertedHead = mkNodeWithoutLookbackPrefix b head
        match convertedHead with
        | Epsilon -> mkNodeWithoutLookbackPrefix b tail
        | _ -> b.mkConcat2(convertedHead,tail)
    | Or(nodes = xs) -> xs |> Seq.map (mkNodeWithoutLookbackPrefix b) |> Seq.toArray |> b.mkOrSeq
    | And(nodes = xs)
    | Or(nodes = xs) -> xs |> Seq.map (mkNodeWithoutLookbackPrefix b) |> b.mkAnd
    | Not(_) ->
        // assert (not node.ContainsLookaround)
        node
    | _ -> node


let attemptMergeIntersectLang
    (_cache: RegexCache<'t>)
    (mkLang: RegexNode<'t> -> RegexNode<'t>[])
    (oldNode: RegexNode<'t>)
    (languages: RegexNode<'t> array seq)
    =
    languages
    |> Seq.reduce (fun (lang1) (lang2) ->
        Seq.zip lang1 lang2
        |> Seq.indexed
        |> Seq.map (fun (idx, (l1, l2)) ->
            match l1, l2 with
            | n1, n2
            | n2, n1 when refEq n1 n2 -> n1
            | f, _
            | _, f when refEq f _cache.False -> _cache.False
            | n1, n2
            | n2, n1 when refEq n1 _cache.TrueStar -> n2
            | n1, n2
            | n2, n1 when refEq n1 _cache.Eps ->
                if n2.CanBeNullable then _cache.Eps else _cache.False
            // --
            | SingletonStarLoop(pred) as p1, other
            | other, (SingletonStarLoop(pred) as p1) ->
                let sub =
                    Solver.containsS _cache.Solver pred (other.SubsumedByMinterm(_cache.Solver))

                if sub then other else _cache.Builder.mkAnd2 (l1, l2)
            | _ ->
                let mapCanonical(node: RegexNode<'t>) =
                    node.TryGetInfo
                    |> ValueOption.map (fun info ->
                        if info.IsCanonical then
                            node
                        else if info.HasCanonicalForm.IsSome then
                            info.HasCanonicalForm.Value
                        else
                            let canonForm =
                                _cache.Builder.GetCanonical(node, mkLang node, (fun v -> node))

                            canonForm
                    )
                    |> ValueOption.defaultValue node

                let l1 = mapCanonical l1
                let l2 = mapCanonical l2

                let newNode = _cache.Builder.mkAnd2 (l1, l2)
                _cache.Builder.GetCanonical(newNode, mkLang newNode, (fun v -> newNode))
        )
        |> Seq.toArray
    )


let attemptMergeUnionLang
    (_cache: RegexCache<'t>)
    (mkLang: RegexNode<'t> -> RegexNode<'t>[])
    (oldNode: RegexNode<'t>)
    (languages: RegexNode<'t> array seq)
    =
    languages
    |> Seq.reduce (fun (lang1) (lang2) ->
        Seq.zip lang1 lang2
        |> Seq.indexed
        |> Seq.map (fun (idx, (l1, l2)) ->
            match l1, l2 with
            | n1, n2
            | n2, n1 when refEq n1 n2 -> n1
            | n1, other
            | other, n1 when refEq n1 _cache.False -> other
            | n1, n2
            | n2, n1 when refEq n1 _cache.TrueStar -> _cache.TrueStar
            | n1, n2
            | n2, n1 when refEq n1 _cache.Eps ->
                if n2.CanBeNullable then n2 else _cache.Builder.mkLoop (n2, 0, 1)
            // --
            | SingletonStarLoop(pred) as p1, other
            | other, (SingletonStarLoop(pred) as p1) ->
                let sub =
                    Solver.containsS _cache.Solver pred (other.SubsumedByMinterm(_cache.Solver))

                if sub then p1 else _cache.Builder.mkOr2 (l1, l2)
            | _ ->
                let mapCanonical(node: RegexNode<'t>) =
                    node.TryGetInfo
                    |> ValueOption.map (fun info ->
                        if info.IsCanonical then
                            node
                        else if info.HasCanonicalForm.IsSome then
                            info.HasCanonicalForm.Value
                        else
                            let canonForm =
                                _cache.Builder.GetCanonical(node, mkLang node, (fun v -> node))

                            canonForm
                    )
                    |> ValueOption.defaultValue node

                let l1 = mapCanonical l1
                let l2 = mapCanonical l2

                let newNode = _cache.Builder.mkOr2 (l1, l2)

                let canon =
                    _cache.Builder.GetCanonical(newNode, mkLang newNode, (fun v -> newNode))

                canon
        )
        |> Seq.toArray
    )


let rec getFixedPrefixLength (c: RegexCache<'t>) (node: RegexNode<_>) =
    let rec loop (acc: int) node : int option * RegexNode<_> option =
        match node with
        | Concat(head, tail, _) ->
            let headprf = loop acc head

            match headprf with
            | Some n, None -> loop n tail
            | _ ->
                match acc with
                | 0 -> None, None
                | n -> Some n, Some node
        | Epsilon -> Some(0 + acc), None
        | Or(nodes, _)
        | And(nodes, _) -> None, Some node
        | Singleton _ -> Some(1 + acc), None
        | Loop(Singleton _, low, up, _) when low = up -> Some(low + acc), None
        | Loop(Singleton _ as body, low, up, _) when low <> 0 ->
            let remainingUp = if up = Int32.MaxValue then up else up - low
            Some(low + acc), Some(c.Builder.mkLoop (body, 0, remainingUp)) // could be inferred
        | Loop _ -> None, Some node
        | Not _ -> None, Some node
        | LookAround _ -> Some(0 + acc), None
        | Begin
        | End -> Some(0 + acc), None

    let r = loop 0 node
    r

let rec getLengthMapping
    getNodeId
    createNonInitialDerivative
    (c: RegexCache<'t>)
    (node: RegexNode<'t>)
    : LengthLookup<'t>
    =
    let redundant = HashSet([ c.False ])

    let rec loop
        (acc: ('t array * int) list)
        (remainingTransitions: ('t list * 't * RegexNode<'t>)[])
        =
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
                (fun (acc: ('t array * int) list) res ->
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
        |> Optimizations.getNonRedundantDerivatives createNonInitialDerivative c redundant
        |> Seq.toArray

    let initial: ('t list * 't * RegexNode<'t>) array =
        nonRedundant |> Seq.map (fun (v1, v2) -> [], v1, v2) |> Seq.toArray

    let result = loop [] initial

    match result with
    | [] ->
        let fixedPrefix = getFixedPrefixLength c node

        match fixedPrefix with
        | Some len, Some remaining ->
            let stateId = getNodeId remaining
            LengthLookup.FixedLengthPrefixMatchEnd(len, stateId)
        | _ -> LengthLookup.MatchEnd
    | _ ->
        result
        |> Seq.map (fun (pref, len) -> struct (Memory(pref), len))
        |> Seq.toArray
        |> LengthLookup.FixedLengthSetLookup


let inferLengthLookup
    getNodeId
    createNonInitialDerivative
    (c: RegexCache<'t>)
    (node: RegexNode<'t>)
    =
    Info.Node.getFixedLength node
    |> Option.map LengthLookup.FixedLength
    |> Option.defaultWith (fun _ ->
        LengthLookup.MatchEnd
        // getLengthMapping getNodeId createNonInitialDerivative c node
    )

let inferOverrideRegex
    (initialOptimizations: InitialOptimizations<'t,char>)
    (lengthLookup: LengthLookup<'t>)
    (c: RegexCache<'t>)
    (node: RegexNode<'t>)
    (reverseNode: RegexNode<'t>)
    : OverrideRegex<char> option
    =
    if node.DependsOnAnchor || node.HasZerowidthHead ||
       reverseNode.DependsOnAnchor || reverseNode.HasZerowidthHead then
        None
    else
        match lengthLookup, initialOptimizations with
        | LengthLookup.FixedLength(fl), InitialOptimizations.StringPrefix(prefix, _) when
            fl = prefix.Length
            ->
            Some(OverrideRegex.FixedLengthString(prefix))
        | _ -> None


let convertOverrideRegex (initOpts:OverrideRegex<char> option) : OverrideRegex<byte> option =
    match initOpts with
    | None -> None
    | Some (OverrideRegex.FixedLengthString(s)) ->
        match Memory.tryConvertToAscii s with
        | ValueSome ascii -> Some (OverrideRegex.FixedLengthString(ascii))
        | _ -> None
    | Some (OverrideRegex.FixedLengthStringCaseIgnore(str,isAscii)) ->
        None
