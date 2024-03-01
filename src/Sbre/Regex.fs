namespace Sbre

open System
open System.Buffers
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Reflection.Metadata
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open System.Text.RuntimeRegexCopy
open Microsoft.FSharp.Core
open Sbre.Algorithm
open Sbre.Info
open Sbre.Optimizations
open Sbre.Types
open Sbre.Pat
open System.Runtime.InteropServices

[<Struct>]
type MatchResult = {
    Value: string
    Index: int
    Length: int
}

[<Struct>]
type SingleMatchResult = {
    Success: bool
    Value: string
    Index: int
    Length: int
}

[<CLIMutable>]
[<Struct>]
type MatchPosition = {
    Index: int
    Length: int
} with

    member this.GetText(input: ReadOnlySpan<char>) =
        input.Slice(this.Index, this.Length).ToString()


[<AbstractClass>]
type GenericRegexMatcher() =
    abstract member IsMatch: input: ReadOnlySpan<char> -> bool
    abstract member Replace: input: ReadOnlySpan<char> * replacement: ReadOnlySpan<char> -> string
    abstract member Matches: input: ReadOnlySpan<char> -> MatchResult seq
    abstract member EnumerateMatches: input: ReadOnlySpan<char> -> Span<MatchPosition>
    abstract member MatchPositions: input: ReadOnlySpan<char> -> MatchPosition seq
    // abstract member MatchText: input:ReadOnlySpan<char> -> string option
    abstract member Match: input: ReadOnlySpan<char> -> SingleMatchResult
    abstract member Count: input: ReadOnlySpan<char> -> int



[<Sealed>]
type MatchingState(node: RegexNode<TSet>) =
    member val Id = -1 with get, set
    member val Node = node with get, set
    member val Startset: TSet = Unchecked.defaultof<_> with get, set
    member val Flags: RegexStateFlags = RegexStateFlags.None with get, set

    // -- optimizations
    // member val PendingNullablePositions: int[] = [||] with get, set
    member val PendingNullablePositions: Memory<int> = Unchecked.defaultof<_> with get, set

    // member val PendingNullablePositions: Set<int> = Set.empty with get, set
    member val ActiveOptimizations: ActiveBranchOptimizations = ActiveBranchOptimizations.NoOptimizations with get, set
    member val StartsetChars: SearchValues<char> = Unchecked.defaultof<_> with get, set
    member val StartsetIsInverted: bool = Unchecked.defaultof<_> with get, set

type RegexSearchMode =
    | FirstNullable
    | MatchEnd

[<Sealed>]
type RegexMatcher<'t when 't: struct>
    (
        uncanonicalizedNode: RegexNode<TSet>,
        _cache: RegexCache<TSet>
    ) =
    inherit GenericRegexMatcher()
    let InitialDfaStateCapacity = 1024
    let _stateCache = Dictionary<RegexNode<TSet>, MatchingState>()
    let mutable _stateArray = Array.zeroCreate<MatchingState> InitialDfaStateCapacity
    let mutable _flagsArray = Array.zeroCreate<RegexStateFlags> InitialDfaStateCapacity
    let _minterms = _cache.Minterms()
    let _mintermsLog = BitOperations.Log2(uint64 _minterms.Length) + 1
    let mutable _dfaDelta: int[] = Array.init (InitialDfaStateCapacity <<< _mintermsLog) (fun _ -> 0) // 0 : initial state
    let mutable _revStartStates: int[] = Array.init (10 <<< _mintermsLog) (fun _ -> 0) // 0 : initial state


    let rec _isNullable(loc: inref<Location>, node: RegexNode<_>) : bool =
        // short-circuit
        if node.CanNotBeNullable then false
        elif node.IsAlwaysNullable then true else
        match node with
        | Epsilon -> true
        | Singleton _ -> false
        | Or(xs, _) ->
            use mutable e = xs.GetEnumerator()
            let mutable found = false
            while not found && e.MoveNext() do
                found <- _isNullable (&loc, e.Current)
            found
        | And(xs, _) ->
            use mutable e = xs.GetEnumerator()
            let mutable forall = true
            while forall && e.MoveNext() do
                forall <- _isNullable (&loc, e.Current)
            forall
        | Loop(R, low, _, _) -> low = 0 || (_isNullable (&loc, R))
        | Not(inner, _) ->
            not (_isNullable (&loc, inner))
        | Concat(head, tail, _) ->
            _isNullable (&loc, head) && _isNullable (&loc, tail)
        | LookAround(body, _, _, _,_) -> _isNullable(&loc,body)
        | End -> loc.Position = loc.Input.Length
        | Begin -> loc.Position = 0


    let rec _createDerivative (
        loc: inref<Location>,
        loc_pred: TSet,
        node: RegexNode<TSet>
    ) : RegexNode<TSet> =

        let cachedTransition =
            if loc.Position = loc.Input.Length && node.DependsOnAnchor  then
                Algorithm.RegexNode.getEndCachedTransition(loc_pred, node)
            elif loc.Position = 0 && node.DependsOnAnchor  then
                Algorithm.RegexNode.getStartCachedTransition(loc_pred, node)
            else
                Algorithm.RegexNode.getCachedTransition(loc_pred, node)


        let result =
            match cachedTransition with
            | ValueSome inf -> inf
            | _ ->

            match node with
            | Epsilon -> _cache.False
            | Singleton pred ->
                if Solver.elemOfSet pred loc_pred then _cache.Eps else _cache.False
            | Loop(R, low, up, info) ->
                let inline decr x =
                    if x = Int32.MaxValue || x = 0 then x else x - 1
                let case1 =
                    low = 0
                    || info.IsAlwaysNullable = true
                    || not (_isNullable (&loc, R))
                let R_decr = _cache.Builder.mkLoop (R, decr low, decr up)
                match case1 with
                | true ->
                    let R' = _createDerivative(&loc, loc_pred, R)
                    _cache.Builder.mkConcat2 (R', R_decr)
                | false ->
                    _createDerivative ( &loc, loc_pred, _cache.Builder.mkConcat2 (R, R_decr) )
            // Derx (R | S) = Derx (R) | Derx (S)
            | Or(xs, _) ->
                let pool = ArrayPool<RegexNode<TSet>>.Shared
                let rentedArray = pool.Rent(xs.Count)
                use mutable e = xs.GetEnumerator()
                let mutable i = 0
                let mutable count = 0
                while e.MoveNext() do
                    let der = _createDerivative(&loc, loc_pred, e.Current)
                    if not (refEq _cache.False der) then
                        rentedArray[count] <- der
                        count <- count + 1
                    i <- i + 1
                let mem = rentedArray.AsMemory(0,count)
                mem.Span.Sort(physComparison)
                let res = _cache.Builder.mkOr(&mem)
                pool.Return(rentedArray)
                res

            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, _) ->
                // optimized
                // let pool = ArrayPool<RegexNode<TSet>>.Shared
                // let rentedArray = pool.Rent(xs.Count)
                // use mutable e = xs.GetEnumerator()
                // let mutable i = 0
                // while e.MoveNext() do
                //     rentedArray[i] <- _createDerivative(&loc, loc_pred, e.Current)
                //     i <- i + 1
                // let mem = rentedArray.AsMemory(0,i)
                // mem.Span.Sort(physComparison)
                // let res = _cache.Builder.mkAnd(&mem)
                // pool.Return(rentedArray)
                // res
                // orig
                let derivatives = ResizeArray()
                let mutable foundFalse = false
                for n in xs do
                    if not foundFalse then
                        let der = _createDerivative (&loc, loc_pred, n)
                        match der with
                        | _ when refEq _cache.False der ->
                            foundFalse <- true
                        | _ -> derivatives.Add der
                if foundFalse then _cache.False else
                let result = _cache.Builder.mkAnd(derivatives)
                result
            // Derx(~R) = ~Derx (R)
            | Not(inner, _) ->
                _cache.Builder.mkNot(_createDerivative (&loc, loc_pred, inner))
            | Concat(head, tail, _) when head.IsAlwaysNullable ->
                let R' = _createDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                let S' = _createDerivative (&loc, loc_pred, tail)
                if refEq _cache.Builder.uniques._false S' then
                    R'S
                else
                    if refEq R'S _cache.False then S' else
                    _cache.Builder.mkOrSeq [| R'S ;S'|]
            | Concat(head, tail, _) when head.HasZerowidthHead ->
                let R' = _createDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                let S' = _createDerivative (&loc, loc_pred, tail)

                let lookaheadEpsilon =
                    // small semantic detail when lookaround is not in the end
                    // ex. "1\b-2" \b has to be nullable immediately
                    match R' with
                    | LookAround(node=Epsilon;lookBack=false) -> false
                    | _ -> true

                if not (_isNullable (&loc, head)) && lookaheadEpsilon then
                    R'S
                else
                    if refEq R'S _cache.False then S' else
                    _cache.Builder.mkOrSeq [| R'S ;S'|]
            // Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, _) ->
                let R' = _createDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                if _isNullable (&loc, head) then
                    let S' = _createDerivative (&loc, loc_pred, tail)
                    if refEq _cache.Builder.uniques._false S' then
                        R'S
                    else
                        if refEq R'S _cache.False then S' else
                        _cache.Builder.mkOrSeq [| R'S ;S'|]
                else R'S
            // Lookahead
            | LookAround(node=R; lookBack=false; relativeTo= rel; pendingNullables= pendingNulls; info = _) ->
                let der_R = _createDerivative (&loc, loc_pred, R)
                match der_R with
                // start a new pending match
                | _ when pendingNulls.IsEmpty && _isNullable(&loc, der_R) ->
                    _cache.Builder.mkLookaround(der_R, false, rel+1, zeroList)
                | _ -> _cache.Builder.mkLookaround(der_R, false, rel+1, pendingNulls)
            // Lookback
            | LookAround(node=R; lookBack=true; relativeTo= _; pendingNullables= _; info = _) ->
                let der_R = _createDerivative (&loc, loc_pred, R)
                _cache.Builder.mkLookaround(der_R, true, 0, Set.empty)
            | Begin | End -> _cache.False


#if NO_CACHE_BUILDER
#else
        if loc.Position = loc.Input.Length && node.DependsOnAnchor then
            node.TryGetInfo
            |> ValueOption.iter (fun v ->
                v.EndTransitions.TryAdd(loc_pred,result) |> ignore
            )
        elif loc.Position = 0 && node.DependsOnAnchor then
            node.TryGetInfo
            |> ValueOption.iter (fun v ->
                v.StartTransitions.TryAdd(loc_pred,result) |> ignore
            )
        else
            node.TryGetInfo
            |> ValueOption.iter (fun v ->
                v.Transitions.TryAdd(loc_pred,result) |> ignore
            )
#endif

        result


    and _tryCanonicalize(node: RegexNode<TSet>) =
        match node.TryGetInfo with
        | ValueSome info when info.IsCanonical -> node
        | ValueSome info when info.HasCanonicalForm.IsSome -> info.HasCanonicalForm.Value
        | ValueSome info when not info.PendingNullables.IsEmpty -> node
        | _ -> node


    and getNonInitialDerivative (mt,node) =
        let loc = Location.getNonInitial()
        _createDerivative(&loc,mt, node)


    and mkLang node =
        _minterms
        |> Array.map (fun mt ->
            let loc = Location.getNonInitial()
            _createDerivative(&loc,mt, node)
        )


    and _canonicalize(node: RegexNode<TSet>) =

        match node.TryGetInfo with
        | ValueSome info when info.IsCanonical -> node
        | ValueSome info when info.HasCanonicalForm.IsSome -> info.HasCanonicalForm.Value
        | ValueSome info when not info.PendingNullables.IsEmpty -> node
        | _ ->
        // if node.DependsOnAnchor || node.ContainsLookaround then node else
        if node.DependsOnAnchor  then node else

        match node with
        | Concat(head, tail, _) ->
            let ch = _canonicalize head
            let ct = _canonicalize tail
            let cat = _cache.Builder.mkConcat2(ch,ct)
            _cache.Builder.GetCanonical(node,mkLang cat,(fun v -> cat))
        | Or(nodes=nodes; info = info) ->
            let canonNodes =  nodes |> Seq.map _canonicalize |> Seq.toArray
            let languages = canonNodes |> Seq.map mkLang
            let mergedLanguage = attemptMergeUnionLang _cache mkLang node languages
            let mknode = (fun _ -> _cache.Builder.mkOrSeq(canonNodes) )
            _cache.Builder.GetCanonical(node,mergedLanguage, mknode)
        | Singleton _ ->
            let mknode = (fun _ -> node )
            _cache.Builder.GetCanonical(node, mkLang node,mknode)
        | Loop(regexNode, low, up, _) ->
            let inner = _canonicalize regexNode
            let mknode = (fun _ -> _cache.Builder.mkLoop(inner, low,up) )
            _cache.Builder.GetCanonical(node,mkLang node,mknode)
        | And (nodes=nodes) -> // node
            let canonNodes =  nodes |> Seq.map _canonicalize |> Seq.toArray
            let languages = canonNodes |> Seq.map mkLang
            let mergedLanguage = attemptMergeIntersectLang _cache mkLang node languages
            let mknode = (fun _ -> _cache.Builder.mkAnd(canonNodes) )
            _cache.Builder.GetCanonical(node,mergedLanguage, mknode)
        | Not(node=inner) -> // node
            let canon_inner = _canonicalize inner
            let mknode = (fun _ -> _cache.Builder.mkNot(canon_inner) )
            _cache.Builder.GetCanonical(node, mkLang node, mknode)
        // | LookAround(regexNode, lookBack, relativeTo, pendingNullables, regexNodeInfo) ->
        //     let canon_inner = _canonicalize regexNode
        //     let mknode = (fun _ -> _cache.Builder.mkLookaround(canon_inner,lookBack,relativeTo,pendingNullables) )
        //     _cache.Builder.GetCanonical(node, mkLang node, mknode)
        | LookAround _ -> node
        | Begin | End
        | Epsilon -> node


    let _createStartset(state: MatchingState, initial: bool) =
        // todo: performance sensitive
        if state.Flags.ContainsLookaround then () else

        let minterms = _cache.Minterms()

        let derivatives =
            minterms
            |> Array.map (fun minterm ->
                match RegexNode.getCachedTransition (minterm, state.Node) with
                | ValueSome v -> v
                | _ ->
                    let mutable loc = Location.getNonInitial()
                    _createDerivative(&loc, minterm, state.Node)
            )

        // debug pretty minterms
        // let prettyMinterms =
        //     Seq.zip minterms derivatives
        //     |> Seq.map (fun (mt,d) ->
        //         $"{_cache.PrettyPrintMinterm(mt)} : {_cache.PrettyPrintNode(d)}"
        //     )
        //     |> String.concat "\n"
        //
        // let startsetPredicateStr =
        //     Seq.zip minterms derivatives
        //     |> Seq.where (fun (mt,d) ->
        //         not (refEq d _cache.False)
        //     )
        //     |> Seq.map (fun (mt,d) ->
        //         $"{_cache.PrettyPrintMinterm(mt)} : {_cache.PrettyPrintNode(d)}"
        //     )
        //     |> String.concat "\n"

        let condition =
            if initial then
                (fun d -> not (refEq d state.Node || refEq d _cache.False))
            else
                (fun d -> not (refEq d state.Node))

        let startsetPredicate =
            Seq.zip minterms derivatives
            |> Seq.where (fun (_, d) -> condition d)
            |> Seq.map fst
            |> Seq.fold (|||) _cache.Solver.Empty

        // let dbg_startset = _cache.PrettyPrintMinterm(startsetPredicate)
        // invert empty startset (nothing to skip to)
        let setChars = _cache.MintermSearchValues(startsetPredicate)
        let isInverted = Solver.elemOfSet startsetPredicate minterms[0]
        // TODO:
        match setChars.Mode with
        | MintermSearchMode.InvertedSearchValues
        | MintermSearchMode.SearchValues ->
            state.Startset <- startsetPredicate
            state.StartsetChars <- setChars.SearchValues
            state.StartsetIsInverted <- isInverted
        | _ ->
            let isInverted = Solver.elemOfSet startsetPredicate minterms[0]
            state.Startset <- startsetPredicate
            state.StartsetChars <- Unchecked.defaultof<_>
            state.StartsetIsInverted <- isInverted



    let rec _getOrCreateState(revTruestar, origNode, isInitial) =
#if CANONICAL
        let node = _canonicalize origNode
#else
        let node = origNode
#endif
        // if not (refEq origNode node) then
        //     failwith $"canon: {origNode} ==> {node}"
        // let node = origNode

        match _stateCache.TryGetValue(node) with
        | true, v -> v // a dfa state already exists for this regex
        | _ ->
            let state = MatchingState(node)
            _stateCache.Add(node, state)
            state.Id <- _stateCache.Count
            let nodeFlags = node.GetFlags()

            // TODO: grow state space if needed (? probably never needed)
            if _stateArray.Length = state.Id then
                // if _stateArray.Length > 50000 then
                if _stateArray.Length > 1000000 then
                    failwith "state space blowup!"
                // failwith "TODO: resize DFA"
                let newsize = _stateArray.Length * 2
                Array.Resize(&_stateArray, newsize)
                Array.Resize(&_flagsArray, newsize)
                Array.Resize(&_dfaDelta, newsize <<< _mintermsLog)

            _stateArray[state.Id] <- state

            if nodeFlags.IsAlwaysNullable then
                state.Flags <- state.Flags ||| RegexStateFlags.AlwaysNullableFlag
            if nodeFlags.CanBeNullable then
                state.Flags <- state.Flags ||| RegexStateFlags.CanBeNullableFlag
            if nodeFlags.ContainsLookaround then
                state.Flags <- state.Flags ||| RegexStateFlags.ContainsLookaroundFlag
            if refEq _cache.False node then
                state.Flags <- state.Flags ||| RegexStateFlags.DeadendFlag
            if isInitial then
                state.Flags <- state.Flags ||| RegexStateFlags.InitialFlag

            // generate startset
            _createStartset (state, isInitial)
            if
                not (_cache.Solver.IsEmpty(state.Startset) || _cache.Solver.IsFull(state.Startset))
            then
                if isNull state.StartsetChars then () else
                state.Flags <- state.Flags ||| RegexStateFlags.CanSkipFlag
            else
                ()

            if not isInitial
               // && not (state.Flags.HasFlag(RegexStateFlags.CanSkipFlag))
               && not (state.Flags.HasFlag(RegexStateFlags.ContainsLookaroundFlag)) then
                // see if limited skip possible
                let limitedSkip =
                    Optimizations.tryGetLimitedSkip
                        (fun (mt,node) ->
                            let mutable loc = Location.getNonInitial()
                            _createDerivative(&loc,mt,node) )
                        (fun v -> _getOrCreateState(revTruestar,v,false).Flags )
                        (fun v -> _getOrCreateState(revTruestar,v,false).Id )
                        (fun v -> _getOrCreateState(revTruestar,v,false).Startset )
                        _cache
                            revTruestar
                            state.Node
                match limitedSkip with
                | Some ls ->
                    if isNull state.StartsetChars then () else
                    state.ActiveOptimizations <- ls
                    state.Flags <-
                        state.Flags |||
                        RegexStateFlags.CanSkipFlag |||
                        RegexStateFlags.ActiveBranchOptimizations
                | _ ->
                    ()

            if node.ContainsLookaround && node.CanBeNullable && not isInitial then
                state.PendingNullablePositions <- node.PendingNullables |> Seq.toArray |> Memory
                if state.PendingNullablePositions.Length > 0 then
                    state.Flags <- state.Flags ||| RegexStateFlags.IsPendingNullableFlag
            if node.DependsOnAnchor then
                state.Flags <- state.Flags ||| RegexStateFlags.DependsOnAnchor

            _flagsArray[state.Id] <- state.Flags
            state

// #if CANONICAL
//     // do _cache.Builder.CanonicalizeCallback <- Some _canonicalize
//     do _cache.Builder.InitCanonical(_minterms)
//     let R_canonical = _canonicalize uncanonicalizedNode
// #else
//     let R_canonical = uncanonicalizedNode
// #endif
//
    do _cache.Builder.InitCanonical(_minterms)
    let R_canonical = _canonicalize uncanonicalizedNode
    let reverseNode = RegexNode.rev _cache.Builder R_canonical
    let reverseTrueStarredNode = _cache.Builder.mkConcat2 (_cache.TrueStar, reverseNode)
    let trueStarredNode = _cache.Builder.mkConcat2 (_cache.TrueStar, R_canonical)
    let _noprefix = mkNodeWithoutLookbackPrefix _cache.Builder R_canonical
    let DFA_TR_rev = _getOrCreateState(reverseTrueStarredNode,reverseTrueStarredNode, true).Id // R_rev
    let DFA_R_noPrefix = _getOrCreateState(reverseTrueStarredNode,_noprefix, false).Id


#if OPTIMIZE
    let _initialOptimizations =
        let opts =
            Optimizations.findInitialOptimizations
                (fun (mt,node) ->
                    let mutable loc = Location.getNonInitial()
                    _createDerivative(&loc,mt,node) )
                (fun node -> _getOrCreateState(reverseTrueStarredNode,node,false).Id )
                (fun node -> _getOrCreateState(reverseTrueStarredNode,node,false).Flags )
                _cache reverseNode reverseTrueStarredNode
        let cannotUsePrefix =
            match opts with
            | InitialOptimizations.SetsPrefix(prefix=prefix)
            | InitialOptimizations.SetsPotentialStart prefix ->
                let chrs = _cache.MintermChars(prefix.Span[0])
                chrs.IsNone
            | _ -> false
        if cannotUsePrefix then InitialOptimizations.NoOptimizations else opts
    let _lengthLookup =
        Optimizations.inferLengthLookup (fun node -> _getOrCreateState(reverseTrueStarredNode,node,false).Id ) getNonInitialDerivative _cache _noprefix
#else
    let _initialOptimizations =
        InitialOptimizations.NoOptimizations
    let _lengthLookup =
        LengthLookup.MatchEnd
#endif

    let _regexOverride =
        Optimizations.inferOverrideRegex _initialOptimizations _lengthLookup _cache R_canonical

    member this.IsMatchRev
        (
            loc: byref<Location>
        ) : bool =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)
        let mutable looping = true
        let mutable found = false
        let mutable currentStateId = DFA_TR_rev


        while looping do
            let flags = _flagsArray[currentStateId]
            // let mutable dfaState = _stateArray[currentStateId]
#if SKIP
            if (flags.CanSkipInitial && this.TrySkipInitialRev(&loc, &currentStateId))
                then ()
            else
#endif
            if this.StateIsNullable(flags, &loc, currentStateId) then
                found <- true
                looping <- false

            if loc.Position > 0 then
                this.TakeTransition(flags, &currentStateId, &loc)
                loc.Position <- loc.Position - 1
            else
                looping <- false
        found

    override this.IsMatch(input) =
        let mutable loc = Location.createReversedSpan input
        let mutable _toplevelOr = _cache.False
        if not reverseTrueStarredNode.DependsOnAnchor then
            this.IsMatchRev(&loc)
        else
        // ismatch with anchors is more complex than nullability
        match this.llmatch_all_count_only(input) with
        | 0 -> false
        | _ -> true


    override this.Match(input) : SingleMatchResult =
        let firstMatch = this.MatchPositions(input) |> Seq.toArray
        match firstMatch.Length = 0 with
        | true -> {
            Success = false
            Value = ""
            Index = 0
            Length = 0
          }
        | false ->
            let result = firstMatch[0]
            {
                Success = true
                Value = input.Slice(result.Index, result.Length).ToString()
                Index = result.Index
                Length = result.Length
            }

    /// replace all occurrences in string
    override this.Replace(input, replacement) =
        let sb = System.Text.StringBuilder(input.ToString())
        let mutable offset = 0

        for result in this.MatchPositions(input) do
            let start = offset + result.Index
            sb.Remove(start, result.Length + 1).Insert(start, replacement) |> ignore
            offset <- replacement.Length - result.Length - 1

        sb.ToString()

    /// return all matches on input
    override this.Matches(input) =
        let mr = ResizeArray()

        for result in this.llmatch_all input do
            mr.Add(
                {
                    Value = input.Slice(result.Index, result.Length).ToString()
                    Index = result.Index
                    Length = result.Length
                }
            )

        mr

    /// counts the number of matches
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Count(input) = this.llmatch_all_count_only input

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateStartset(state: MatchingState, initial: bool) =
        _createStartset (state, initial)

    /// initialize regex in DFA
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetOrCreateState(node: RegexNode<TSet>) : MatchingState =
        _getOrCreateState (reverseTrueStarredNode,node, false)

    member private this.GetDeltaOffset(stateId: int, mintermId: int) =
        (stateId <<< _mintermsLog) ||| mintermId

    //
    member private this.TryNextDerivative
        (
            currentState: byref<int>,
            mintermId: int,
            loc: inref<Location>
        ) =
        let minterm = _cache.MintermById(mintermId)
        let targetState = this.GetOrCreateState(
            this.CreateDerivative( &loc, minterm, _stateArray[currentState].Node))
        targetState.Id

#if DEBUG
    member this.GetStateAndFlagsById(stateId: int) = _stateArray[stateId]
#endif


    member this.TakeAnchorTransition
        (
            currentState: byref<int>,
            loc: inref<Location>,
            mtId: int
        ) =
        if (loc.Position = loc.Input.Length) then
            let dfaOffset = this.GetDeltaOffset(currentState, mtId)
            let nextStateId = _dfaDelta[dfaOffset]
            let cachedStateId = _revStartStates[nextStateId]
            // existing transition in dfa
            if cachedStateId > 0
            then currentState <- cachedStateId
            else
            let dfaOffset = this.GetDeltaOffset(currentState, mtId)
            let nextState = this.TryNextDerivative(&currentState, mtId, &loc)
            _revStartStates[dfaOffset] <- nextState
            currentState <- nextState
        else
            // dont cache start at all
            let nextState = this.TryNextDerivative(&currentState, mtId, &loc)
            currentState <- nextState

    member this.TakeTransition
        (
            flags: RegexStateFlags,
            currentState: byref<int>,
            loc: inref<Location>
        ) =
        let mintermId = _cache.MintermId(loc)
        let dfaOffset = this.GetDeltaOffset(currentState, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        // caching workaround until context implementation
        if flags.CannotBeCached && (loc.Position = loc.Input.Length || loc.Position = 0) then
            this.TakeAnchorTransition(&currentState,&loc,mintermId)
        else if
            // existing transition in dfa
            nextStateId > 0
        then
            currentState <- nextStateId
        else

        // new transition
        if obj.ReferenceEquals(null, _stateArray[nextStateId]) then
            let nextState = this.TryNextDerivative(&currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member private this.StateIsNullable
        (
            flags: RegexStateFlags,
            loc: byref<Location>,
            stateId: int
        ) : bool =
        flags.CanBeNullable && (
            flags.IsAlwaysNullable ||
            this.IsNullable (&loc, _stateArray[stateId].Node))

    // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsNullable(loc: inref<Location>, node: RegexNode<_>) : bool =
        _isNullable(&loc,node)

    member this.CreateDerivative ( loc: inref<Location>, loc_pred: TSet, node: RegexNode<TSet>
    ) : RegexNode<TSet> =

        let canonNode = node
            // too expensive
            // match node.TryGetInfo with
            // | ValueSome info when info.IsCanonical -> node
            // | _ ->
            //     if node.DependsOnAnchor then node else
            //     match loc.Kind with
            //     | LocationKind.StartPos -> node
            //     | LocationKind.Center -> _canonicalize node
            //     | LocationKind.EndPos -> node
            //     | _ -> failwith "?"

        _createDerivative(&loc,loc_pred,canonNode)

    /// end position with DFA
    member this.DfaEndPosition
        (
            loc: byref<Location>,
            startStateId: int
        ) : int32 =
        let mutable looping = true
        let mutable currentStateId = startStateId
        let mutable currentMax = -2

        while looping do
            // let mutable dfaState = _stateArray[currentStateId]
            let flags = _flagsArray[currentStateId]

            if flags.IsDeadend then
                looping <- false
            else
            if flags.CanSkipLeftToRight then
                this.TrySkipActiveFwd(flags,&loc,&currentStateId) |> ignore
                // this.TrySkipActiveLeftToRight(&loc, &currentStateId)

            // set max nullability after skipping
            if this.StateIsNullable(flags, &loc, currentStateId) then
                this.HandleNullableFwd(flags, &currentMax, loc, currentStateId)

            if loc.Position < loc.Input.Length then
                this.TakeTransition(flags, &currentStateId, &loc)
                loc.Position <- loc.Position + 1
            else
                looping <- false
        currentMax

    member this.TrySkipInitialRev(loc:byref<Location>, currentStateId:byref<int>) : bool =
        match _initialOptimizations with
        | InitialOptimizations.StringPrefix(prefix, transitionNodeId) ->
            let slice = loc.Input.Slice(0, loc.Position)
            let resultStart = slice.LastIndexOf(prefix.Span)
            if resultStart = -1 then
                loc.Position <- Location.final loc
                false
            else
                currentStateId <- transitionNodeId
                loc.Position <- resultStart
                true
        | InitialOptimizations.SinglePotentialStart (prefix,inverted) ->
            let slice = loc.Input.Slice(0, loc.Position)
            let resultStart =
                if not inverted then slice.LastIndexOfAny(prefix)
                else (slice.LastIndexOfAnyExcept(prefix))
            if resultStart = -1 then
                loc.Position <- Location.final loc
                false
            else
                loc.Position <- resultStart + 1
                false
        | InitialOptimizations.StringPrefixCaseIgnore(headSet,tailSet, prefix, ascii, transitionNodeId) ->
            // case insensitive Span.LastIndexOf is SLOW AND NOT VECTORIZED
            let mutable resultStart = loc.Position
            let mutable found = false
            let prefixSpan = prefix.Span.Slice(0,prefix.Length-1)
            let textSpan = loc.Input
            while not found  do
                let mutable slice = textSpan.Slice(0, resultStart)
                resultStart <- slice.LastIndexOfAny(tailSet)
                if resultStart = -1 then found <- true else
                slice <- textSpan.Slice(0, resultStart)
                match slice.EndsWith(prefixSpan, StringComparison.OrdinalIgnoreCase) with
                | true ->
                    resultStart <- resultStart - prefix.Span.Length + 1
                    found <- true
                | _ -> ()

            if resultStart = -1 then
                loc.Position <- Location.final loc
                false
            else
                currentStateId <- transitionNodeId
                loc.Position <- resultStart
                true
        | InitialOptimizations.SearchValuesPrefix(prefix, transitionNodeId) ->
            let pspan = prefix.Span
            let skipResult = _cache.TryNextStartsetLocationSearchValuesReversed( &loc, pspan )
            match skipResult with
            | ValueSome resultEnd ->
                let suffixStart = resultEnd - prefix.Length
                currentStateId <- transitionNodeId
                loc.Position <- suffixStart
                true
            | ValueNone ->
                // no matches remaining
                loc.Position <- Location.final loc
                false
        | InitialOptimizations.SetsPrefix(prefix, transitionNodeId) ->
            let skipResult = _cache.TryNextStartsetLocationArrayReversed( &loc, prefix.Span )
            match skipResult with
            | ValueSome resultEnd ->
                let suffixStart = resultEnd - prefix.Length
                currentStateId <- transitionNodeId
                loc.Position <- suffixStart
                true
            | ValueNone ->
                // no matches remaining
                loc.Position <- Location.final loc
                false
        | InitialOptimizations.SearchValuesPotentialStart (prefix,_) ->
            let skipResult = _cache.TryNextStartsetLocationSearchValuesReversed( &loc, prefix.Span )
            match skipResult with
            | ValueSome resultEnd ->
                let n = resultEnd <> loc.Position
                loc.Position <- resultEnd
                n
            | ValueNone ->
                // no matches remaining
                loc.Position <- Location.final loc
                false
        | InitialOptimizations.SetsPotentialStart prefix ->
            let skipResult = _cache.TryNextStartsetLocationArrayReversed( &loc, prefix.Span )
            match skipResult with
            | ValueSome resultEnd ->
                let n = resultEnd <> loc.Position
                loc.Position <- resultEnd
                n
            | ValueNone ->
                // no matches remaining
                loc.Position <- Location.final loc
                false
            // let svals = _cache.MintermSearchValues(prefix.Span[0]).Value
            // let isInverted = _cache.MintermIsInverted(prefix.Span[0])
            // _cache.TryNextStartsetLocationRightToLeft( &loc, svals, isInverted)
            // false
        | InitialOptimizations.NoOptimizations -> false



    member this.TrySkipActiveFwd(flags:RegexStateFlags, loc: byref<Location>, currentStateId:byref<int>) =
        let dfaState = _stateArray[currentStateId]
        // if loc.Position = loc.Input.Length then false else
        // if flags.HasFlag(RegexStateFlags.ActiveBranchOptimizations) then
        //     match dfaState.ActiveOptimizations with
        //     // | ActiveBranchOptimizations.PossibleStringPrefix(prefix,transId) ->
        //     //     let limitedSlice = loc.Input.Slice(0, loc.Position)
        //     //     let pspan = prefix.Span
        //     //     if limitedSlice.EndsWith(pspan) then
        //     //         loc.Position <- loc.Position - pspan.Length
        //     //         currentStateId <- transId
        //     //         true
        //     //     else
        //     //         false
        //     | LimitedSkip(distance, termPred, termTransitionId, nonTermTransitionId) ->
        //         if loc.Input.Length < loc.Position + distance then // no more matches
        //             loc.Position <- Location.final loc
        //             false
        //         else
        //         let limitedSlice = loc.Input.Slice(loc.Position, distance)
        //         match limitedSlice.IndexOfAny(termPred) with
        //         | -1 ->
        //             loc.Position <- loc.Position + distance
        //             currentStateId <- nonTermTransitionId
        //             true
        //         | idx ->
        //             let newPos = loc.Position + distance - idx
        //             // let relativepos = loc.Input.Slice(loc.Position - distance + idx, 10)
        //             // let debug = relativepos.ToString()
        //             // let newState = _stateArray[termTransitionId]
        //             loc.Position <- newPos
        //             currentStateId <- termTransitionId
        //             true // mark nullable
        //     | _ -> false
        // else
        // default single char skip
        let isInverted = dfaState.StartsetIsInverted
        let setChars = dfaState.StartsetChars
        let slice = loc.Input.Slice(loc.Position)
        let sharedIndex =
            if isInverted then slice.IndexOfAnyExcept(setChars)
            else slice.IndexOfAny(setChars)
        if sharedIndex = -1 then
            loc.Position <- Location.final loc
        else
            loc.Position <- loc.Position + sharedIndex
        false


    member this.TrySkipActiveRev(flags:RegexStateFlags,loc:byref<Location>, currentStateId:byref<int>, acc: byref<SharedResizeArrayStruct<int>>) : bool =
        let dfaState = _stateArray[currentStateId]
        // if loc.Position = 0 then false else
        if flags.HasActiveBranchOptimizations then
            match dfaState.ActiveOptimizations with
            | ActiveBranchOptimizations.PossibleStringPrefix(prefix,transId) ->
                let limitedSlice = loc.Input.Slice(0, loc.Position)
                let pspan = prefix.Span
                if limitedSlice.EndsWith(pspan) then
                    loc.Position <- loc.Position - pspan.Length
                    currentStateId <- transId
                    true
                else
                    false
            | LimitedSkip(distance, termPred, termTransitionId, nonTermTransitionId) ->
                if distance > loc.Position then // no more matches
                    loc.Position <- Location.final loc
                    false
                else
                let limitedSlice = loc.Input.Slice(loc.Position - distance, distance)
                match limitedSlice.LastIndexOfAny(termPred) with
                | -1 ->
                    loc.Position <- loc.Position - distance
                    currentStateId <- nonTermTransitionId
                    true
                | idx ->
                    let newPos = loc.Position - distance + idx
                    loc.Position <- newPos
                    currentStateId <- termTransitionId
                    true // mark nullable
            | NoOptimizations -> false
        else
            let tmp_loc = loc.Position
            _cache.TryNextStartsetLocationRightToLeft(
                &loc,
                dfaState.StartsetChars,
                dfaState.StartsetIsInverted
            )
            // adding all skipped locations todo: optimize this to ranges
            if tmp_loc > loc.Position && this.StateIsNullable(flags, &loc, currentStateId) then
                for i = tmp_loc downto loc.Position + 1 do
                    acc.Add(i)
                true
            else
                false
     // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
     member this.TrySkipActiveLeftToRight(loc: byref<Location>, currentStateId:byref<int>) : unit =
        let dfaState = _stateArray[currentStateId]

        let isInverted = dfaState.StartsetIsInverted
        let setChars = dfaState.StartsetChars
        let slice = loc.Input.Slice(loc.Position)
        let sharedIndex =
            if isInverted then slice.IndexOfAnyExcept(setChars)
            else slice.IndexOfAny(setChars)
        if sharedIndex = -1 then
            loc.Position <- Location.final loc
        else
            loc.Position <- loc.Position + sharedIndex

    member this.HandleNullableRev(flags:RegexStateFlags,acc: byref<SharedResizeArrayStruct<int>>,loc,currentStateId) =
        if flags.IsPendingNullable then
            let span = _stateArray[currentStateId].PendingNullablePositions.Span
            for i = span.Length - 1 downto 0 do
                acc.Add (span[i] + loc.Position)
        else acc.Add loc.Position

    member this.HandleNullableFwd(flags:RegexStateFlags,currentMax:byref<int>,loc,currentStateId) =
        if flags.IsPendingNullable then
            let pending = _stateArray[currentStateId].PendingNullablePositions.Span
            for p in pending do
               currentMax <- max currentMax (loc.Position - p)
        else
            currentMax <- loc.Position

    /// unoptimized collect all nullable positions
    member this.CollectReverseNullablePositions
        (
            acc: byref<SharedResizeArrayStruct<int>>,
            loc: byref<Location>
        ) : SharedResizeArrayStruct<int> =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)
        let mutable looping = true
        let mutable currentStateId = DFA_TR_rev

        while looping do
            let flags = _flagsArray[currentStateId]
            // let dfaState = _stateArray[currentStateId]
#if SKIP
            if (flags.CanSkipInitial && this.TrySkipInitialRev(&loc, &currentStateId))

#if SKIP_ACTIVE
               || (flags.CanSkip && this.TrySkipActiveRev(flags,&loc, &currentStateId, &acc))
#endif
                then ()
            else
#endif
            if this.StateIsNullable(flags, &loc, currentStateId) then
                this.HandleNullableRev(flags,&acc,loc,currentStateId)

            if loc.Position > 0 then
                this.TakeTransition(flags, &currentStateId, &loc)
                loc.Position <- loc.Position - 1
            else
                looping <- false
        acc

    member this.PrintAllDerivatives
        (
            acc: byref<SharedResizeArrayStruct<int>>,
            loc: byref<Location>
        ) : string list =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)
        let mutable looping = true
        let mutable currentStateId = DFA_TR_rev
        let ders = ResizeArray()

        while looping do
            let flags = _flagsArray[currentStateId]
            if this.StateIsNullable(flags, &loc, currentStateId) then
                if flags.IsPendingNullable then
                    for pos in _stateArray[currentStateId].PendingNullablePositions.Span do
                        acc.Add pos
                else acc.Add loc.Position

            if loc.Position > 0 then
                this.TakeTransition(flags, &currentStateId, &loc)
                let state = _stateArray[currentStateId]

                let modified =
                    match state.Node with
                    | Or(nodes=nodes) ->
                        nodes
                        |> Seq.where (function
                            Concat(head=Pat.TrueStar _cache.Solver) -> false
                            | _ -> true
                        )
                        |> _cache.Builder.mkOrSeq
                    | _ -> state.Node

                // ders.Add(state.Node.ToString())
                ders.Add(modified.ToString())
                loc.Position <- loc.Position - 1
            else
                looping <- false
        ders |> Seq.toList

    member this.getMatchEnd(loc: byref<Location>) : int =
        match _lengthLookup with
        | LengthLookup.FixedLength fl -> loc.Position + fl
        | LengthLookup.FixedLengthPrefixMatchEnd (fl,stateId) ->
            loc.Position <- loc.Position + fl
            this.DfaEndPosition(&loc, stateId)
        // makes no real difference
        // | FixedLengthSetLookup lookup when loc.Position <> 0 ->
        //     let mutable found = false
        //     let mutable lengthResult = 0
        //     let mutable i = 0
        //     while not found && i < lookup.Length do
        //         let struct(pref,len) = lookup[i]
        //         if _cache.HasMintermPrefix(&loc,pref.Span) then
        //             found <- true
        //             lengthResult <- len
        //         i <- i + 1
        //     lengthResult
        | _ -> this.DfaEndPosition(&loc, DFA_R_noPrefix)

    member this.llmatch_all_override(acc:byref<SharedResizeArrayStruct<MatchPosition>>, loc: byref<Location>, overridden:OverrideRegex) =
        let tspan = loc.Input
        match overridden with
        | OverrideRegex.FixedLengthString s ->
            let pspan = s.Span
            let mutable looping = true
            let mutable currPos = 0
            let textLength = s.Length
            while looping do
                    // LastIndexOf with ignore case is NOT VECTORIZED
                    match tspan.Slice(currPos).IndexOf(pspan, StringComparison.Ordinal) with
                    | -1 -> looping <- false
                    | n ->
                        let start = currPos + n
                        acc.Add({ MatchPosition.Index = start; Length = textLength })
                        currPos <- start + textLength
        | OverrideRegex.FixedLengthStringCaseIgnore (headSet,s, ascii) ->
            let pspan = s.Span
            let mutable looping = true
            let mutable currPos = 0
            let textLength = s.Length
            // ascii in .net is implicitly vectorized - use it if possible
            if ascii then
                while looping do
                    // LastIndexOf with ignore case is NOT VECTORIZED
                    match tspan.Slice(currPos).IndexOf(pspan, StringComparison.OrdinalIgnoreCase) with
                    | -1 -> looping <- false
                    | n ->
                        let start = currPos + n
                        acc.Add({ MatchPosition.Index = start; Length = textLength })
                        currPos <- start + textLength
            // case insensitive unicode is suboptimal for now
            else
                while looping do
                    let slice = tspan.Slice(currPos)
                    let start = slice.IndexOfAny(headSet)
                    if start = -1 then looping <- false else
                    match slice.Slice(start).StartsWith(pspan, StringComparison.OrdinalIgnoreCase) with
                    | false -> currPos <- currPos + start + 1
                    | _ ->
                        acc.Add({ MatchPosition.Index = start; Length = textLength })
                        currPos <- currPos + start + textLength



    member this.llmatch_all(input: ReadOnlySpan<char>) : SharedResizeArrayStruct<MatchPosition> =
        let mutable matches = new SharedResizeArrayStruct<MatchPosition>(256)
        let mutable loc = Location.createReversedSpan input
        match _regexOverride with
        | Some regOverride ->
            this.llmatch_all_override(&matches,&loc,regOverride)
        | _ ->
            let mutable acc = new SharedResizeArrayStruct<int>(512)
            let allPotentialStarts =
                this.CollectReverseNullablePositions(&acc, &loc)
            loc.Reversed <- false
            let mutable nextValidStart = 0
            let startSpans = allPotentialStarts.AsSpan()
            for i = (startSpans.Length - 1) downto 0 do
                let currStart = startSpans[i]
                if currStart >= nextValidStart then
                    loc.Position <- currStart
                    let matchEnd = this.getMatchEnd(&loc)
                    matches.Add({ MatchPosition.Index = currStart; Length = (matchEnd - currStart) })
                    nextValidStart <- matchEnd
            acc.Dispose()
        matches





    member this.llmatch_all_count_only(input: ReadOnlySpan<char>) : int =
        let mutable loc = Location.createReversedSpan input
        use mutable matches = new SharedResizeArrayStruct<MatchPosition>(256)
        match _regexOverride with
        | Some regOverride ->
            this.llmatch_all_override(&matches,&loc,regOverride)
        | _ ->
            use mutable acc = new SharedResizeArrayStruct<int>(512)
            let allPotentialStarts =
                this.CollectReverseNullablePositions(&acc, &loc)
            loc.Reversed <- false
            let mutable nextValidStart = 0
            let startSpans = allPotentialStarts.AsSpan()
            for i = (startSpans.Length - 1) downto 0 do
                let currStart = startSpans[i]
                if currStart >= nextValidStart then
                    loc.Position <- currStart
                    let matchEnd = this.getMatchEnd(&loc)
                    match matchEnd with
                    | -2 ->
                        failwith "invalid match"
                        ()
                    | _ ->
                        matches.Add({ MatchPosition.Index = currStart; Length = (matchEnd - currStart) })
                        nextValidStart <- matchEnd
        matches.size

    /// return just the positions of matches without allocating the result
    override this.MatchPositions(input) = (this.llmatch_all input).AsArray()
    override this.EnumerateMatches(input) = (this.llmatch_all input).AsSpan()

#if DEBUG
    override this.ProcessedPattern =
        AppContext.SetData("RegexNode.MaxPrintWidth", 100000)
        let a = _cache.PrettyPrintNode(R_canonical)
        AppContext.SetData("RegexNode.MaxPrintWidth", null)
        a
#endif

    // accessors
    member this.TrueStarredPattern = trueStarredNode
    member this.ReverseTrueStarredPattern = reverseTrueStarredNode
    member this.RawPattern = R_canonical
    member this.RawPatternWithoutLookback = _stateArray[DFA_R_noPrefix].Node

    member this.ReversePattern = reverseNode

    member this.Cache = _cache


module Helpers =
    let createMatcher
        (
            bddBuilder: RegexBuilder<BDD>,
            bddMinterms: BDD array,
            charsetSolver,
            converter,
            symbolicBddnode,
            regexTree: RegexTree
        )
        : GenericRegexMatcher
        =
        match bddMinterms.Length with
        // | n when n < 32 ->
        //     let solver = UInt32Solver(minterms, charsetSolver)
        //     let uintbuilder = RegexBuilder(converter, solver, charsetSolver)
        //     let trueStarredNode  = (Minterms.transform uintbuilder charsetSolver solver) trueStarPattern
        //     let rawNode = (Minterms.transform uintbuilder charsetSolver solver) symbolicBddnode
        //     let optimizations = RegexFindOptimizations(regexTree.Root, RegexOptions.NonBacktracking)
        //     let reverseNode = RegexNode.rev uintbuilder rawNode
        //     let cache =
        //         Sbre.RegexCache(
        //             solver,
        //             charsetSolver,
        //             _implicitDotstarPattern = trueStarredNode,
        //             _rawPattern = rawNode,
        //             _reversePattern = reverseNode,
        //             _builder = uintbuilder,
        //             _optimizations = optimizations
        //         )
        //     RegexMatcher<uint32>(trueStarredNode,rawNode,reverseNode,cache) :> GenericRegexMatcher
        | n when n < 64 ->
            let solver = TSolver(bddMinterms, charsetSolver)
#if DEBUG
            Debug.debuggerSolver <- Some solver
#endif
            let uintbuilder = RegexBuilder(converter, solver, charsetSolver)

            let rawNode = (Minterms.transform bddBuilder uintbuilder charsetSolver solver) symbolicBddnode


            let cache =
                Sbre.RegexCache(
                    solver,
                    charsetSolver,
                    bddMinterms,
                    _rawPattern = rawNode,
                    _builder = uintbuilder
                )

            RegexMatcher<TSet>(rawNode, cache)
        | n -> failwith $"bitvector too large, size: {n}"




[<Sealed>]
type Regex(pattern: string, [<Optional; DefaultParameterValue(false)>] _experimental: bool) =
    inherit GenericRegexMatcher()
    let pattern = pattern.Replace("⊤", @"[\s\S]")
    // experimental parser!
    let regexTree =
        ExtendedRegexParser.Parse(
            pattern,
            RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking ||| RegexOptions.Multiline,
            CultureInfo.InvariantCulture
        )
    let charsetSolver = CharSetSolver()
    let runtimeBddBuilder = SymbolicRegexBuilder<BDD>(charsetSolver, charsetSolver)
    let converter = RegexNodeConverter(runtimeBddBuilder, null)
    let regexBuilder = RegexBuilder(converter, charsetSolver, charsetSolver)
    let symbolicBddnode: RegexNode<BDD> =
        RegexNodeConverter.convertToSymbolicRegexNode (
            charsetSolver,
            regexBuilder,
            regexTree.Root
        )
    let minterms = symbolicBddnode |> Minterms.compute runtimeBddBuilder

    let matcher =
        Helpers.createMatcher (
            regexBuilder,
            minterms,
            charsetSolver,
            converter,
            symbolicBddnode,
            regexTree
        )

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Count(input) = matcher.Count(input)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.IsMatch(input) = matcher.IsMatch(input)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.MatchPositions(input) = matcher.MatchPositions(input)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.EnumerateMatches(input) = matcher.EnumerateMatches(input)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Matches(input) = matcher.Matches(input)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Replace(input, replacement) = matcher.Replace(input, replacement)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Match(input) = matcher.Match(input)

    member this.Matcher: GenericRegexMatcher = matcher

    member this.TSetMatcher: RegexMatcher<TSet> = matcher :?> RegexMatcher<TSet>
    member this.InitialReversePrefix =
        Sbre.Optimizations.findInitialOptimizations
            (fun (mt,node) ->
                let mutable loc = Location.getNonInitial()
                this.TSetMatcher.CreateDerivative(&loc,mt,node) )
            (fun node -> this.TSetMatcher.GetOrCreateState(node).Id)
            (fun node -> this.TSetMatcher.GetOrCreateState(node).Flags)
            this.TSetMatcher.Cache
            this.TSetMatcher.ReversePattern
            this.TSetMatcher.ReverseTrueStarredPattern


#if DEBUG
    // member this.UInt16Matcher: RegexMatcher<uint16> = matcher :?> RegexMatcher<uint16>
    // member this.ByteMatcher: RegexMatcher<byte> = matcher :?> RegexMatcher<byte>
#endif
