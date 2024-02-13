namespace Sbre

open System
open System.Buffers
open System.Collections.Generic
open System.Globalization
open System.Numerics
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
    member val PendingNullablePositions: int[] = [||] with get, set
    member val ActiveOptimizations: ActiveBranchOptimizations = ActiveBranchOptimizations.NoOptimizations with get, set
    member val StartsetChars: SearchValues<char> = Unchecked.defaultof<_> with get, set
    member val StartsetIsInverted: bool = Unchecked.defaultof<_> with get, set

    member this.BuildFlags(c: RegexCache<TSet>) =
        let mutable flags = RegexStateFlags.None
        let nodeFlags = node.GetFlags()

        if nodeFlags.IsAlwaysNullable then
            flags <- flags ||| RegexStateFlags.AlwaysNullableFlag

        if nodeFlags.CanBeNullable then
            flags <- flags ||| RegexStateFlags.CanBeNullableFlag
        if nodeFlags.ContainsLookaround then
            flags <- flags ||| RegexStateFlags.ContainsLookaroundFlag
        if c.InitialPatternWithoutDotstar.ContainsLookaround then
            flags <- flags ||| RegexStateFlags.ContainsLookaroundFlag

        if refEq c.False node then
            flags <- flags ||| RegexStateFlags.DeadendFlag

        // TODO: limitedSkip

        this.Flags <- flags

    member this.SetStartset(c: RegexCache<TSet>, set: TSet) =
        let setChars = c.MintermSearchValues(set)
        match setChars with
        | None ->
            let minterms = c.Minterms()
            let isInverted = Solver.elemOfSet set minterms[0]
            this.Startset <- set
            this.StartsetChars <- Unchecked.defaultof<_>
            this.StartsetIsInverted <- isInverted
        | Some setChars ->
            let minterms = c.Minterms()
            let isInverted = Solver.elemOfSet set minterms[0]
            this.Startset <- set
            this.StartsetChars <- setChars
            this.StartsetIsInverted <- isInverted



type RegexSearchMode =
    | FirstNullable
    | MatchEnd

[<Sealed>]
type RegexMatcher<'t when 't: struct>
    (
        trueStarredNode: RegexNode<TSet>,
        reverseTrueStarredNode: RegexNode<TSet>,
        initialNode: RegexNode<TSet>,
        reverseNode: RegexNode<TSet>,
        _cache: RegexCache<TSet>
    ) =
    inherit GenericRegexMatcher()
    // INITIALIZE
    let InitialDfaStateCapacity = 1024
    let _stateCache = Dictionary<RegexNode<TSet>, MatchingState>()
    let mutable _stateArray = Array.zeroCreate<MatchingState> InitialDfaStateCapacity
    let mutable _flagsArray = Array.zeroCreate<RegexStateFlags> InitialDfaStateCapacity
    let _minterms = _cache.Minterms()
    let _mintermsLog = BitOperations.Log2(uint64 _minterms.Length) + 1
    let _initialInfo = initialNode.TryGetInfo

    let _initialIsNegation =
        match initialNode with
        | Not _ -> true
        | _ -> false

    let mutable _dfaDelta: int[] = Array.init (1024 <<< _mintermsLog) (fun _ -> 0) // 0 : initial state

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
        | LookAround(body, _, _, _,info) -> _isNullable(&loc,body)
        | Anchor regexAnchor ->
            match regexAnchor with
            | End -> loc.Position = loc.Input.Length
            | Begin -> loc.Position = 0


    let rec _createDerivative (
        loc: inref<Location>,
        loc_pred: TSet,
        node: RegexNode<TSet>
        // useCache: bool
    ) : RegexNode<TSet> =

        let cachedTransition =
            if node.DependsOnAnchor && loc.Position = loc.Input.Length then
                Algorithm.RegexNode.getEndCachedTransition(loc_pred, node)
            elif node.DependsOnAnchor && loc.Position = 0 then
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
                let arr = ResizeArray()
                use mutable e = xs.GetEnumerator()
                while e.MoveNext() do
                    arr.Add((_createDerivative(&loc, loc_pred, e.Current)))
                arr.ToArray() |> _cache.Builder.mkOr
            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, _) ->
                let derivatives = ResizeArray()
                for n in xs do derivatives.Add (_createDerivative (&loc, loc_pred, n))
                _cache.Builder.mkAnd(derivatives)
            // Derx(~R) = ~Derx (R)
            | Not(inner, _) ->
                _cache.Builder.mkNot(_createDerivative (&loc, loc_pred, inner))
            // pos. lookback prefix (?<=aaa)bbb
            | Concat(LookAround(node=lookBody; lookBack=true) as look, tail, _) ->
                let lookDer = _createDerivative (&loc, loc_pred, lookBody)
                let lookBodyNullable = _isNullable(&loc, lookBody)
                let pendingLookaround = _cache.Builder.mkLookaround(lookDer, true)
                let R'S = _cache.Builder.mkConcat2 (pendingLookaround, tail)
                if lookBodyNullable then
                    let S' = _createDerivative (&loc, loc_pred, tail)
                    _cache.Builder.mkOr ( [|R'S ;S'|] )
                else
                    if refEq lookDer _cache.False then _cache.False else
                    R'S
            // pos. lookahead suffix aaa(?=bbb)
            | Concat(head, (LookAround(
                node=regexNode; lookBack=false; relativeTo = rel;
                pendingNullables = (pendingNullables)) as tail) , _) ->
                let headIsNullable = _isNullable(&loc,head)
                let updatedNullables = if headIsNullable then rel :: pendingNullables else pendingNullables
                let updatedRel = if updatedNullables.IsEmpty then rel else rel + 1
                let updatedLookaroundBody = _createDerivative(&loc, loc_pred, regexNode)
                let R' = _createDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                if headIsNullable then
                    let S' =
                        _cache.Builder.mkLookaround(
                            updatedLookaroundBody, false, updatedRel, (updatedNullables)
                    )
                    _cache.Builder.mkOr([| R'S ;S' |])
                else R'S
            // TODO; reimplement
            // pos.lookahead prefix: (?=\w)a  // (?=\w\w)aa ==> (?=\w)a
            | Concat(LookAround(node=lookBody; lookBack=false; relativeTo = _; pendingNullables = _) as look, tail, _) ->
                // TODO: either rewrite this or throw a not supported exception
                // but good enough because no one uses this anyway
                match lookBody with
                | Singleton _ ->
                    let lookDer = _createDerivative (
                        &loc, loc_pred, _cache.Builder.mkConcat2(lookBody,_cache.TrueStar)
                    )
                    let S' = _createDerivative (&loc, loc_pred, tail)
                    _cache.Builder.mkAnd ( seq { lookDer ;S' } )
                | Epsilon ->
                    let S' = _createDerivative (&loc, loc_pred, tail)
                    S'
                | _ -> failwith "Sbre does not support inner lookarounds, use intersections! (&)"

            | Concat(head, tail, _) when head.IsAlwaysNullable ->
                let R' = _createDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                let S' = _createDerivative (&loc, loc_pred, tail)
                if refEq _cache.Builder.uniques._false S' then
                    R'S
                else
                    if refEq R'S _cache.False then S' else
                    _cache.Builder.mkOr ([| R'S ;S'|] )
            | Concat(head, tail, _) when head.HasZerowidthHead ->
                // failwith "todo: complex lookaround derivative"
                // assert (head.DependsOnAnchor || head.ContainsLookaround)
                let R' = _createDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                let S' = _createDerivative (&loc, loc_pred, tail)
                if // head.DependsOnAnchor &&
                   not (_isNullable (&loc, head)) &&
                   R'.CanNotBeNullable then
                    R'S
                else
                if refEq _cache.Builder.uniques._false S' then
                    R'S
                else
                    if refEq R'S _cache.False then S' else
                    _cache.Builder.mkOr ([| R'S ;S'|] )

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
                        _cache.Builder.mkOr ([| R'S ;S'|] )
                else R'S

            | LookAround(lookBody, lookBack, rel, relativeNullablePos,_) ->
                match lookBack with
                | false ->
                    // lookahead
                    let remainingLookBody = _createDerivative (&loc, loc_pred, lookBody)
                    let remainingIsNullable = _isNullable(&loc, remainingLookBody)

                    match remainingLookBody with
                    // start a new pending match
                    | _ when relativeNullablePos.IsEmpty && remainingIsNullable ->
                        _cache.Builder.mkLookaround(
                        remainingLookBody, lookBack, rel+1, zeroList)
                    | _ ->
                    // add pending nullable only if hasnt matched yet
                    let updatedPositions =
                        if remainingIsNullable then relativeNullablePos
                        else relativeNullablePos
                    let pendingLookaround = _cache.Builder.mkLookaround(
                        remainingLookBody, lookBack, (rel+1),updatedPositions)
                    if refEq _cache.False remainingLookBody then
                        _cache.False
                    else
                        pendingLookaround
                | true ->

                    let remainingLookBody = _createDerivative (&loc, loc_pred, lookBody)
                    match remainingLookBody with
                    | n when refEq n _cache.False -> _cache.False
                    | n when refEq n _cache.Eps -> _cache.Eps
                    | _ ->
                        let pendingLookaround = _cache.Builder.mkLookaround( remainingLookBody, lookBack)
                        pendingLookaround


            | Anchor _ -> _cache.False


#if NO_CACHE_BUILDER
#else
        if node.DependsOnAnchor && loc.Position = loc.Input.Length then
            node.TryGetInfo
            |> ValueOption.iter (fun v ->
                v.EndTransitions.TryAdd(loc_pred,result) |> ignore
            )
        elif node.DependsOnAnchor && loc.Position = 0 then
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
        match setChars with
        | None ->
            let minterms = _cache.Minterms()
            let isInverted = Solver.elemOfSet startsetPredicate minterms[0]
            state.Startset <- startsetPredicate
            state.StartsetChars <- Unchecked.defaultof<_>
            state.StartsetIsInverted <- isInverted
        | Some setChars ->
            let minterms = _cache.Minterms()
            let isInverted = Solver.elemOfSet startsetPredicate minterms[0]
            state.Startset <- startsetPredicate
            state.StartsetChars <- setChars
            state.StartsetIsInverted <- isInverted


    let rec _getOrCreateState(node, isInitial) =
        match _stateCache.TryGetValue(node) with
        | true, v -> v // a dfa state already exists for this regex
        | _ ->
            let state = MatchingState(node)
            _stateCache.Add(node, state)
            state.Id <- _stateCache.Count

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
            state.BuildFlags(_cache)
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

            if node.HasCounter then
                state.Flags <- state.Flags ||| RegexStateFlags.HasCounterFlag

            if not isInitial
               && not (state.Flags.HasFlag(RegexStateFlags.CanSkipFlag))
               && not (state.Flags.HasFlag(RegexStateFlags.ContainsLookaroundFlag)) then
                // see if limited skip possible
                let limitedSkip =
                    Optimizations.tryGetLimitedSkip
                        (fun (mt,node) ->
                            let mutable loc = Location.getNonInitial()
                            _createDerivative(&loc,mt,node) )
                        (fun v -> _getOrCreateState(v,false).Id )
                        (fun v -> _getOrCreateState(v,false).Startset )
                        _cache
                            reverseTrueStarredNode
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
                match Optimizations.collectPendingNullables (fun v -> v.CanBeNullable) state.Node with
                | n when n.IsEmpty -> ()
                | nullables ->
                    state.PendingNullablePositions <- nullables |> Seq.toArray
                    state.Flags <- state.Flags ||| RegexStateFlags.IsPendingNullableFlag
            if node.DependsOnAnchor then
                state.Flags <- state.Flags ||| RegexStateFlags.DependsOnAnchor

            _flagsArray[state.Id] <- state.Flags
            state

    // ⊤*(R_rev)
    let DFA_TR_rev = _getOrCreateState(reverseTrueStarredNode, true).Id // R_rev

    // T*R
    let _ = _getOrCreateState(trueStarredNode, true).Id
    // R_rev
    let _ = _getOrCreateState(reverseNode, false).Id
    // R
    let _ = _getOrCreateState(initialNode, false).Id
    let DFA_R_noPrefix =
        _getOrCreateState(nodeWithoutLookbackPrefix _cache.Builder initialNode, false).Id
        // else
        //     DFA_R_orig

    let _initialOptimizations =
        let opts =
            Optimizations.findInitialOptimizations
                (fun (mt,node) ->
                    let mutable loc = Location.getNonInitial()
                    _createDerivative(&loc,mt,node) )
                (fun node -> _getOrCreateState(node,false).Id )
                (fun node -> _getOrCreateState(node,false).Flags )
                _cache reverseNode reverseTrueStarredNode
        let cannotUsePrefix =
            match opts with
            | InitialOptimizations.SetsPrefix(prefix=prefix)
            | InitialOptimizations.PotentialStartPrefix prefix ->
                let chrs = _cache.MintermChars(prefix.Span[0])
                chrs.IsNone
            | _ -> false
        if cannotUsePrefix then InitialOptimizations.NoOptimizations else opts

    let _initialFixedLength =
        Node.getFixedLength reverseNode


    override this.IsMatch(input) =
        let mutable currPos = 0
        let mutable _ = Location.createSpan input currPos
        let mutable _toplevelOr = _cache.False
        match this.llmatch_all_count_only(input) with
        | 0 -> false
        | _ -> true
        // match this.DfaMatchEnd(&startLocation, DFA_TR, searchMode=RegexSearchMode.FirstNullable) with
        // | -2 -> false
        // | _ -> true

    override this.Match(input) : SingleMatchResult =
        let firstMatch = this.MatchPositions(input) |> Seq.tryHead

        match firstMatch with
        | None -> {
            Success = false
            Value = ""
            Index = 0
            Length = 0
          }
        | Some result ->
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
    override this.Count(input) = this.llmatch_all_count_only (input)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateStartset(state: MatchingState, initial: bool) =
        _createStartset (state, initial)

    /// initialize regex in DFA
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetOrCreateState(node: RegexNode<TSet>) : MatchingState =
        _getOrCreateState (node, false)

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
        // if flags.CannotBeCached || loc.Position = loc.Input.Length then
        if flags.CannotBeCached && (loc.Position = loc.Input.Length || loc.Position = 0) then
            let nextState = this.TryNextDerivative(&currentState, mintermId, &loc)
            // _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState
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

    member this.IsNullable(loc: inref<Location>, node: RegexNode<_>) : bool =
        _isNullable(&loc,node)

    member this.CreateDerivative ( loc: inref<Location>, loc_pred: TSet, node: RegexNode<TSet>
    ) : RegexNode<TSet> = _createDerivative(&loc,loc_pred,node)

    /// end position with DFA
    member this.DfaEndPosition
        (
            loc: byref<Location>,
            startStateId: int
#if DEBUG
            ,
            ?debugFn: MatchingState -> unit
#endif
        ) : int32 =
        assert (loc.Position > -1)
        let mutable looping = true
        let mutable currentStateId = startStateId
        let mutable currentMax = -2

        while looping do
            let mutable dfaState = _stateArray[currentStateId]
            let flags = dfaState.Flags
#if DEBUG
            debugFn |> Option.iter (fun fn -> fn (dfaState))
#endif
            if flags.IsDeadend then
                looping <- false
            else
            if flags.CanSkip then
                let ss = dfaState.Startset
                if isNull dfaState.StartsetChars then () else
                _cache.TryNextStartsetLocation(&loc, ss)

            // set max nullability after skipping
            if this.StateIsNullable(flags, &loc, currentStateId) then
                if flags.IsPendingNullable then
                    let pending = _stateArray[currentStateId].PendingNullablePositions
                    for p in pending do
                       currentMax <- max currentMax (loc.Position - p)
                else
                    currentMax <- loc.Position

            if loc.Position < loc.Input.Length then
                this.TakeTransition(flags, &currentStateId, &loc)

                loc.Position <- Location.nextPosition loc
            else
                looping <- false

        currentMax


    member this.DfaMatchEnd
        (
            loc: byref<Location>,
            startStateId: int,
            searchMode:RegexSearchMode
        ) : int32 =
        assert (loc.Position > -1)
        let mutable looping = true
        let mutable currentStateId = startStateId
        let mutable currentMax = -2

        while looping do
            let mutable dfaState = _stateArray[currentStateId]
            let flags = dfaState.Flags
            if flags.IsDeadend then
                looping <- false
            else

            if flags.CanSkip then
                let ss = dfaState.Startset
                _cache.TryNextStartsetLocation(&loc, ss)

            // set max nullability after skipping
            if this.StateIsNullable(flags, &loc, currentStateId) then
                currentMax <- loc.Position
                match searchMode with
                | RegexSearchMode.FirstNullable ->
                    loc.Position <- Location.final loc
                | _ -> ()

            if not (Location.isFinal loc) then
                this.TakeTransition(flags, &currentStateId, &loc)
                loc.Position <- Location.nextPosition loc
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
        | InitialOptimizations.SearchValuesPrefix(prefix, transitionNodeId) ->
            let skipResult = _cache.TryNextStartsetLocationSearchValuesReversed( &loc, prefix.Span )
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
        | InitialOptimizations.DebugWordBorderPrefix(_, _) ->
            failwith "todo"
            // let mutable doneLooping = false
            // let mutable result = ValueNone
            // let pretty1 = prefix.Span.Slice(1,2).ToArray()
            // let pretty2 = prefix.Span.Slice(1,2).ToArray() |> Array.map _cache.PrettyPrintMinterm
            // let m0 = prefix.Span[0]
            // let m3 = prefix.Span[3]
            // let p2 = 1
            // while not doneLooping do
            //     let skipResult = _cache.TryNextStartsetLocationArrayReversed( &loc, pretty1.AsSpan() )
            //     match skipResult with
            //     | ValueSome resultEnd ->
            //         let suffixStart = resultEnd - prefix.Length
            //         let mtTail = _cache.CharToMinterm(loc.Input[resultEnd])
            //         let mtHead = _cache.CharToMinterm(loc.Input[suffixStart])
            //
            //         // let currSlice = loc.Input.Slice(suffixStart)
            //         let currSlice = loc.Input.Slice(suffixStart-5)
            //         let w = 1
            //         failwith "todo"
            //     | ValueNone ->
            //         doneLooping <- true
            // match result with
            // | ValueSome resultEnd ->
            //     let suffixStart = resultEnd - prefix.Length
            //     currentStateId <- transitionNodeId
            //     loc.Position <- suffixStart
            //     true
            // | ValueNone ->
            //     // no matches remaining
            //     loc.Position <- Location.final loc
            //     false
        | InitialOptimizations.PotentialStartPrefix prefix ->
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
        | InitialOptimizations.NoOptimizations -> false


    member this.TrySkipActiveRev(flags:RegexStateFlags,loc:byref<Location>, currentStateId:byref<int>, acc: byref<SharedResizeArrayStruct<int>>) : bool =
        let dfaState = _stateArray[currentStateId]
        if flags.HasFlag(RegexStateFlags.ActiveBranchOptimizations) then
            match dfaState.ActiveOptimizations with
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
                    // let relativepos = loc.Input.Slice(loc.Position - distance + idx, 10)
                    // let debug = relativepos.ToString()
                    // let newState = _stateArray[termTransitionId]
                    loc.Position <- newPos
                    currentStateId <- termTransitionId

                    true // mark nullable
            | _ -> failwith "todo"
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


    member this.HandleNullableRev(flags:RegexStateFlags,acc: byref<SharedResizeArrayStruct<int>>,loc,currentStateId) =
        if flags.IsPendingNullable then
            let span = _stateArray[currentStateId].PendingNullablePositions.AsSpan()
            for i = span.Length - 1 downto 0 do
                acc.Add (span[i] + loc.Position)
        else acc.Add loc.Position

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
                    // TODO: multiple nulls here
                if flags.IsPendingNullable then
                    for pos in _stateArray[currentStateId].PendingNullablePositions |> Seq.rev do
                        acc.Add pos
                    // (loc.Position + _stateArray[currentStateId].PendingNullable)
                else acc.Add loc.Position

            if loc.Position > 0 then
                this.TakeTransition(flags, &currentStateId, &loc)
                let state = _stateArray[currentStateId]
                ders.Add(state.Node.ToString())
                loc.Position <- loc.Position - 1
            else
                looping <- false
        ders |> Seq.toList

    member this.llmatch_all(input: ReadOnlySpan<char>) : ResizeArray<MatchPosition> =

        let matches = ResizeArray(100)
        let mutable loc = Location.createReversedSpan input
        use mutable acc = new SharedResizeArrayStruct<int>(100)
        let allPotentialStarts =
            if reverseTrueStarredNode.IsAlwaysNullable then
                for i = loc.Input.Length downto 0 do
                    acc.Add(i)
                acc
            else
                this.CollectReverseNullablePositions(&acc, &loc)
        loc.Reversed <- false
        let mutable nextValidStart = 0
        let startSpans = allPotentialStarts.AsSpan()

        for i = (startSpans.Length - 1) downto 0 do
            let currStart = startSpans[i]
            if currStart >= nextValidStart then
                loc.Position <- currStart
                let matchEnd =
                    match _initialFixedLength with
                    | Some fl -> currStart + fl
                    | _ -> this.DfaEndPosition(&loc, DFA_R_noPrefix)
                match matchEnd with
                | -2 -> ()
                | _ ->
                    matches.Add({ MatchPosition.Index = currStart; Length = (matchEnd - currStart) })
                    nextValidStart <- matchEnd

        matches


    member this.llmatch_all_count_only(input: ReadOnlySpan<char>) : int =

        let mutable matchCount = 0
        let mutable loc = Location.createReversedSpan input
        use mutable acc = new SharedResizeArrayStruct<int>(100)
        let allPotentialStarts =
            // if _flagsArray[DFA_TR_rev].IsAlwaysNullable then
            //     for i = loc.Input.Length downto 0 do
            //         acc.Add(i)
            //     acc
            // else
            this.CollectReverseNullablePositions(&acc, &loc)
        loc.Reversed <- false
        let mutable nextValidStart = 0
        let startSpans = allPotentialStarts.AsSpan()
        for i = (startSpans.Length - 1) downto 0 do
            let currStart = startSpans[i]
            if currStart >= nextValidStart then
                loc.Position <- currStart
                let matchEnd =
                    match _initialFixedLength with
                    | Some fl -> currStart + fl
                    | _ -> this.DfaEndPosition(&loc, DFA_R_noPrefix)
                match matchEnd with
                | -2 -> ()
                | _ ->
                    matchCount <- matchCount + 1
                    nextValidStart <- matchEnd
        matchCount

    /// return just the positions of matches without allocating the result
    override this.MatchPositions(input) = this.llmatch_all input


    // accessors
    member this.TrueStarredPattern = trueStarredNode
    member this.ReverseTrueStarredPattern = reverseTrueStarredNode

    member this.RawPattern = initialNode
    member this.RawPatternWithoutLookback = _stateArray[DFA_R_noPrefix].Node

    member this.ReversePattern = reverseNode

    member this.Cache = _cache


module Helpers =
    let createMatcher
        (
            bddBuilder: RegexBuilder<BDD>,
            minterms: BDD array,
            charsetSolver,
            converter,
            symbolicBddnode,
            regexTree: RegexTree
        )
        : GenericRegexMatcher
        =
        match minterms.Length with
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
            let solver = UInt64Solver(minterms, charsetSolver)
#if DEBUG
            Debug.debuggerSolver <- Some solver
#endif
            let uintbuilder = RegexBuilder(converter, solver, charsetSolver)
            uintbuilder.InitializeUniqueMap(bddBuilder)

            let rawNode = (Minterms.transform bddBuilder uintbuilder charsetSolver solver) symbolicBddnode
            let trueStarredNode = uintbuilder.mkConcat2(uintbuilder.uniques._trueStar, rawNode)

            if not (regexTree.Root.Options.HasFlag(RegexOptions.RightToLeft)) then
                regexTree.Root.Options <- RegexOptions.RightToLeft

            let reverseNode = RegexNode.rev uintbuilder rawNode

            let cache =
                Sbre.RegexCache(
                    solver,
                    charsetSolver,
                    _implicitDotstarPattern = trueStarredNode,
                    _rawPattern = rawNode,
                    _reversePattern = reverseNode,
                    _builder = uintbuilder
                )

            let revTrueStarred = cache.Builder.mkConcat2 (cache.TrueStar, reverseNode)
            RegexMatcher<uint64>(trueStarredNode, revTrueStarred, rawNode, reverseNode, cache) //:> GenericRegexMatcher
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
    member this.UInt16Matcher: RegexMatcher<uint16> = matcher :?> RegexMatcher<uint16>
    member this.ByteMatcher: RegexMatcher<byte> = matcher :?> RegexMatcher<byte>
#endif
