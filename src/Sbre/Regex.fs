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
open Sbre.CountingSet
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
    member val RelativeNullable: int = 0 with get, set
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
        // TODO: could be optimized
        if c.InitialPatternWithoutDotstar.ContainsLookaround then
            flags <- flags ||| RegexStateFlags.ContainsLookaroundFlag

        if refEq c.False node then
            flags <- flags ||| RegexStateFlags.DeadendFlag

        // TODO: limitedSkip

        this.Flags <- flags

    member this.SetStartset(c: RegexCache<TSet>, set: TSet) =
        let setChars = c.MintermSearchValues(set)
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


    let _createStartset(state: MatchingState, initial: bool) =
        if state.Flags.ContainsLookaround then
            state.Startset <- _cache.Solver.Empty
        else

        let minterms = _cache.Minterms()

        let derivatives =
            minterms
            |> Array.map (fun minterm ->
                match RegexNode.getCachedTransition (minterm, state.Node.TryGetInfo) with
                | ValueSome v -> v
                | _ -> createStartsetDerivative (_cache, minterm, state.Node)
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

        // TODO: check for sequences
        let startsetPredicate =
            Seq.zip minterms derivatives
            |> Seq.where (fun (_, d) -> condition d)
            |> Seq.map fst
            |> Seq.fold (|||) _cache.Solver.Empty

        // let dbg_startset = _cache.PrettyPrintMinterm(startsetPredicate)
        // invert empty startset (nothing to skip to)
        state.SetStartset(_cache, startsetPredicate)

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

            // TODO: skipping
            if
                not (_cache.Solver.IsEmpty(state.Startset) || _cache.Solver.IsFull(state.Startset))
            then
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
                        (fun v -> _getOrCreateState(v,false).Id )
                        (fun v -> _getOrCreateState(v,false).Startset )
                        _cache
                            reverseTrueStarredNode
                            state.Node
                match limitedSkip with
                | Some ls ->
                    state.ActiveOptimizations <- ls
                    state.Flags <-
                        state.Flags |||
                        RegexStateFlags.CanSkipFlag |||
                        RegexStateFlags.ActiveBranchOptimizations
                | _ ->
                    ()


                ()
            if node.ContainsLookaround && node.CanBeNullable then
                match Optimizations.tryGetPendingNullable (fun v -> _getOrCreateState(v,false).Flags.CanBeNullable ) state.Node with
                | ValueSome rel ->
                    state.RelativeNullable <- rel
                    state.Flags <- state.Flags ||| RegexStateFlags.IsRelativeNullableFlag
                | ValueNone -> ()
            _flagsArray[state.Id] <- state.Flags
            state

    // ⊤*(R_rev)
    let DFA_TR_rev = _getOrCreateState(reverseTrueStarredNode, true).Id // R_rev

    // T*R
    let DFA_TR = _getOrCreateState(trueStarredNode, true).Id
    // R_rev
    let DFA_R_rev = _getOrCreateState(reverseNode, false).Id
    // R
    let DFA_R_orig = _getOrCreateState(initialNode, false).Id
    let DFA_R_noPrefix =
        if initialNode.ContainsLookaround then
            _getOrCreateState(nodeWithoutLookbackPrefix initialNode, false).Id
        else
            DFA_R_orig

    let _initialOptimizations =
        Optimizations.findInitialOptimizations
            (fun node -> _getOrCreateState(node,false).Id )
            (fun node -> _getOrCreateState(node,false).Flags )
            _cache reverseNode reverseTrueStarredNode

    let _initialFixedLength =
        Optimizations.getFixedLength reverseNode


    override this.IsMatch(input) =
        let mutable currPos = 0
        let mutable startLocation = Location.createSpan input currPos
        let mutable _toplevelOr = _cache.False
        match this.DfaMatchEnd(&startLocation, DFA_TR, searchMode=RegexSearchMode.FirstNullable) with
        | -2 -> false
        | _ -> true

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

        // caching workaround
        if flags.ContainsLookaround then
            let nextState = this.TryNextDerivative(&currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
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
        flags.CanBeNullable && (flags.IsAlwaysNullable || this.IsNullable (&loc, _stateArray[stateId].Node))


    member this.HandleOptimizedNullable(unique:OptimizedUnique, loc: inref<Location>) : bool =
        let currChar = _cache.CurrentChar(loc)
        let prevCharOpt = _cache.PrevChar(loc)
        match unique with
        | WordBorder ->
            match prevCharOpt, currChar with
            | ValueNone, ValueSome c |  ValueSome c, ValueNone -> RegexCharClass.IsBoundaryWordChar(c)
            | ValueSome prev, ValueSome curr ->
                let c1 = not (RegexCharClass.IsBoundaryWordChar(prev)) && RegexCharClass.IsBoundaryWordChar(curr)
                let c2 = (RegexCharClass.IsBoundaryWordChar(prev)) && not (RegexCharClass.IsBoundaryWordChar(curr))
                c1 || c2
            | _ -> true// impossible case

        | StartOfString -> failwith "todo"
        | NotStartOfString -> loc.Position > 0

    member this.IsNullable(loc: inref<Location>, node: RegexNode<_>) : bool =
            // short-circuit
            // if Info.Node.canNotBeNullable node then
            if node.CanNotBeNullable then
                false
            elif node.IsAlwaysNullable then
                true
            else
            // short-circuit common cases
            match _cache.OptimizedUniques.TryGetValue(node) with
            | true, v -> this.HandleOptimizedNullable(v, &loc)
            | _ ->
            match node with
            // Nullx () = true
            | Epsilon -> true
            // Nullx (ψ) = false
            | Singleton _ -> false
            // Nullx (R) or Nullx (S)
            | Or(xs, info) ->
                use mutable e = xs.GetEnumerator()
                let mutable found = false
                while not found && e.MoveNext() do
                    found <- this.IsNullable (&loc, e.Current)
                found
            // Nullx (R) and Nullx (S)
            | And(xs, info) ->
                // short-circuit failure condition
                let canNotBeNullableCheck =
                    match info.LookupPrev with
                    | false -> false
                    | _ ->
                        let c1 =
                            match info.MustStartWithWordBorder with
                            | Some true ->
                                let isWordBorder =
                                    this.HandleOptimizedNullable(OptimizedUnique.WordBorder, &loc)
                                not isWordBorder
                            | _ -> false
                        let c2 =
                            match info.PrevCharRequired with
                            | Some v ->
                                match _cache.PrevChar(loc) with
                                | ValueNone -> true
                                | ValueSome s ->
                                    Solver.notElemOfSet v (_cache.CharToMinterm(s))
                            | _ -> false
                        c1 || c2
                if canNotBeNullableCheck then false else

                use mutable e = xs.GetEnumerator()
                let mutable forall = true
                while forall && e.MoveNext() do
                    forall <- this.IsNullable (&loc, e.Current)
                forall
            // Nullx (R{m, n}) = m = 0 or Nullx (R)
            | Loop(R, low, _, info) -> low = 0 || (this.IsNullable (&loc, R))
            // not(Nullx (R))
            | Not(inner, info) ->
                // if info.NodeFlags.IsCounter then
                //     failwith "counter nullability"
                //     // match state.CounterCanExit(node) with
                //     // | ValueSome(true) -> not (this.IsNullable (cache,&loc, inner))
                //     // | ValueSome(false) -> true
                //     // | _ -> not (this.IsNullable (cache,state, &loc, inner))
                // else
                    not (this.IsNullable (&loc, inner))
            // Nullx (R) and Nullx (S)
            | Concat(head, tail, info) ->
                // if info.NodeFlags.IsCounter then
                //     let counter = state.TryGetCounter(node)
                //     match counter with
                //     | ValueNone -> false
                //     | ValueSome counter ->
                //     let canExit = counter.CanExit()
                //     match canExit with
                //     | true -> this.IsNullable (&loc, tail)
                //     | false -> false
                // else
                    this.IsNullable (&loc, head) && this.IsNullable (&loc, tail)

            // lookaround optimization
            // | LookAround(Singleton pred, lookBack, negate, _) ->
            //     match lookBack with
            //     | true ->
            //         match _cache.PrevChar(loc) with
            //         | ValueNone -> negate
            //         | ValueSome prevc ->
            //             let mt = _cache.CharToMinterm(prevc)
            //             let isSet = Solver.elemOfSet pred mt
            //             if negate then not isSet else isSet
            //     | _ ->
            //         match _cache.CurrentChar(loc) with
            //         | ValueNone -> negate
            //         | ValueSome nextc ->
            //             let mt = _cache.CharToMinterm(nextc)
            //             let isSet = Solver.elemOfSet pred mt
            //             if negate then not isSet else isSet



            | LookAround(body, lookBack, negate, _) ->
                this.IsNullable(&loc,body)
                // let mutable _tlo = _cache.False
                // match lookBack, negate with
                // // Nullx ((?=R)) = IsMatch(x, R)
                // | false, false ->
                //     let mutable loc2 = Location.clone &loc
                //     let startstate = this.GetOrCreateState(body)
                //     match this.DfaMatchEnd(&loc2,startstate.Id, searchMode=RegexSearchMode.FirstNullable) with
                //     | -2 -> false
                //     | _ -> true
                // // Nullx ((?!R)) = not IsMatch(x, R)
                // | false, true ->
                //     let mutable loc2 = Location.clone &loc
                //     let startstate = this.GetOrCreateState(body)
                //     match this.DfaMatchEnd(&loc2,startstate.Id, searchMode=RegexSearchMode.FirstNullable) with
                //     | -2 -> true
                //     | _ -> false
                // | true, false -> // Nullx ((?<=R)) = IsMatch(xr, Rr)
                //     let mutable revnode = (RegexNode.rev _cache.Builder body)
                //     let revstate = this.GetOrCreateState(revnode)
                //     let mutable revloc = Location.createSpanRev loc.Input loc.Position loc.Reversed
                //     match this.DfaMatchEnd(&revloc,revstate.Id, searchMode=RegexSearchMode.FirstNullable) with
                //     | -2 -> false
                //     | _ -> true
                // | true, true -> // Nullx ((?<!R)) = not IsMatch(x r, Rr)
                //     let R_rev = RegexNode.rev _cache.Builder body
                //     let revstate = this.GetOrCreateState(R_rev)
                //     let mutable revloc = Location.createSpanRev loc.Input loc.Position loc.Reversed
                //     match this.DfaMatchEnd(&revloc,revstate.Id, searchMode=RegexSearchMode.FirstNullable) with
                //     | -2 -> true
                //     | _ -> false

    member this.CreateDerivative (
        loc: inref<Location>,
        loc_pred: TSet,
        node: RegexNode<TSet>
    ) : RegexNode<TSet> =

        let result =
            // let info = node.GetFlags()

            match node with
            // Derx (R) = ⊥ if R ∈ ANC or R = ()

            | Epsilon -> _cache.False
            // Der s⟨i⟩ (ψ) = if si ∈ [[ψ]] then () else ⊥
            | Singleton pred ->
                if Solver.elemOfSet pred loc_pred then Epsilon else _cache.False

            // Derx (R{m, n}) =
            // if m=0 or Null ∀(R)=true or Nullx (R)=false
            // then Derx (R)·R{m −1, n −1}
            // else Derx (R·R{m −1, n −1})
            | Loop(R, low, up, info) ->

                // CSA
                let inline decr x =
                    if x = Int32.MaxValue || x = 0 then x else x - 1

                let case1 =
                    low = 0
                    || info.IsAlwaysNullable = true
                    || not (this.IsNullable (&loc, R))

                match case1 with
                | true ->
                    // Derx (R)·R{m −1, n −1, l}
                    // let derR = createDerivative (cache, state, &loc, loc_pred, R)
                    let derR = this.CreateDerivative(&loc, loc_pred, R)
                    _cache.Builder.mkConcat2 (derR, _cache.Builder.mkLoop (R, decr low, decr up))

                | false ->
                    // Derx (R·R{m −1, n −1, l})
                    this.CreateDerivative (
                        // state,
                        &loc,
                        loc_pred,
                        _cache.Builder.mkConcat2 (R, _cache.Builder.mkLoop (R, decr low, decr up))
                    )



            // Derx (R | S) = Derx (R) | Derx (S)
            | Or(xs, info) ->
                // let isWordBorder =
                //     refEq node _cache.Builder.anchors._wordBorder.Value

                // if isWordBorder then
                //     _cache.False
                // else
                let derivatives = ResizeArray()
                for n in xs do
                    derivatives.Add (this.CreateDerivative(&loc, loc_pred, n))
                derivatives |> _cache.Builder.mkOr

            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, info) as head ->
                let failed =
                    match info.LookupPrev with
                    | false -> false
                    | _ ->
                        match info.MustStartWithWordBorder with
                        | Some true ->
                            let isWordBorder =
                                this.HandleOptimizedNullable(OptimizedUnique.WordBorder, &loc)
                            not isWordBorder
                        | _ -> false

                let mutable foundFalse = failed
                //
                let derivatives = ResizeArray()
                use mutable e = xs.GetEnumerator()

                while not foundFalse && e.MoveNext() do
                    let der = this.CreateDerivative (&loc, loc_pred, e.Current)
                    if refEq _cache.False der then
                        foundFalse <- true
                    else
                        derivatives.Add(der)
                if foundFalse then _cache.False else
                // for n in xs do
                //     derivatives.Add (this.CreateDerivative (state,&loc, loc_pred, n))
                _cache.Builder.mkAnd(derivatives)

            // Derx(~R) = ~Derx (R)
            | Not(inner, info) ->
                let derR = this.CreateDerivative (&loc, loc_pred, inner)
                _cache.Builder.mkNot (derR)
            // Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, info) ->
                if head.ContainsLookaround then
                    this.CreateLookaroundDerivative(&loc, loc_pred, node)
                else

                let R' = this.CreateDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)

                if this.IsNullable (&loc, head) then
                    let S' = this.CreateDerivative (&loc, loc_pred, tail)
                    if refEq _cache.Builder.uniques._false S' then
                        R'S
                    else
                        if refEq R'S _cache.False then S' else
                        _cache.Builder.mkOr ( seq { R'S ;S' } )
                else
                    R'S
            | LookAround(lookBody, lookBack, negate, pendingNullable) ->
                match lookBack with
                | false ->
                    // lookahead
                    let remainingLookaround = this.CreateDerivative (&loc, loc_pred, lookBody)
                    let pendingLookaround = _cache.Builder.mkLookaround(remainingLookaround, lookBack, negate, pendingNullable + 1)
                    if refEq _cache.False remainingLookaround then
                        _cache.False
                    else
                        pendingLookaround
                | true ->
                    failwith "todo"

        if not node.ContainsLookaround && not node.HasCounter then
            _cache.Builder.AddTransitionInfo(loc_pred, node, result)

        result

    member this.CreateLookaroundDerivative (
        loc: inref<Location>,
        loc_pred: TSet,
        node: RegexNode<TSet>
    ) : RegexNode<TSet> =
        match node with
        | Concat(LookAround(lookBody, lookBack, negate, pendingNullable), tail, info) ->
            let remainingLookaround = this.CreateDerivative (&loc, loc_pred, lookBody)
            match lookBack with
            | true ->
                if refEq _cache.Builder.uniques._false remainingLookaround then
                    _cache.False
                else
                    match remainingLookaround with
                    | Epsilon -> tail
                    | _ ->
                        let pendingLookaround = _cache.Builder.mkLookaround(remainingLookaround, lookBack, negate, pendingNullable + 1)
                        let R'S = _cache.Builder.mkConcat2 (pendingLookaround, tail)
                        if this.IsNullable(&loc, remainingLookaround) then
                            let S' = this.CreateDerivative (&loc, loc_pred, tail)
                            _cache.Builder.mkOr ( seq { R'S ;S' } )
                        else
                            R'S

                    // let R'S = _cache.Builder.mkConcat2 (R', tail)

                // if this.IsNullable (&loc, lookBody) then
                //     let S' = this.CreateDerivative (&loc, loc_pred, tail)
                //     if refEq _cache.Builder.uniques._false S' then
                //         R'S
                //     else
                //         if refEq R'S _cache.False then S' else
                //         _cache.Builder.mkOr ( seq { R'S ;S' } )
            | false ->
                failwith "todo Lookahead"
        | _ ->
            failwith "todo lookaround der"

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
                _cache.TryNextStartsetLocation(&loc, ss)

            // set max nullability after skipping
            if this.StateIsNullable(flags, &loc, currentStateId) then
                if flags.IsRelativeNullable then
                    currentMax <- max currentMax (loc.Position - _stateArray[currentStateId].RelativeNullable)
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
                    loc.Position <- newPos
                    currentStateId <- termTransitionId
                    true
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
            let dfaState = _stateArray[currentStateId]

#if SKIP
            if (flags.IsInitial && this.TrySkipInitialRev(&loc, &currentStateId)) ||
               (flags.CanSkip && this.TrySkipActiveRev(flags,&loc, &currentStateId, &acc)) then
                ()
            else
#endif
            if this.StateIsNullable(flags, &loc, currentStateId) then
                let pos =
                    if flags.IsRelativeNullable then
                        (loc.Position + _stateArray[currentStateId].RelativeNullable)
                    else loc.Position
                acc.Add pos

            if loc.Position > 0 then
                this.TakeTransition(flags, &currentStateId, &loc)
                loc.Position <- loc.Position - 1
            else
                looping <- false

        acc

    member this.llmatch_all(input: ReadOnlySpan<char>) : ResizeArray<MatchPosition> =

        let matches = ResizeArray(100)
        let mutable loc = Location.createReversedSpan input
        use mutable acc = new SharedResizeArrayStruct<int>(100)
        let allPotentialStarts =
            if reverseTrueStarredNode.CanBeNullable then
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
            if _flagsArray[DFA_TR_rev].CanBeNullable then
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
                    matchCount <- matchCount + 1
                    nextValidStart <- matchEnd
        matchCount

    /// return just the positions of matches without allocating the result
    override this.MatchPositions(input) = this.llmatch_all input


    // accessors
    member this.TrueStarredPattern = trueStarredNode
    member this.ReverseTrueStarredPattern = reverseTrueStarredNode

    member this.RawPattern = initialNode

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
            (fun node -> this.TSetMatcher.GetOrCreateState(node).Id)
            (fun node -> this.TSetMatcher.GetOrCreateState(node).Flags)
            this.TSetMatcher.Cache
            this.TSetMatcher.ReversePattern
            this.TSetMatcher.ReverseTrueStarredPattern
#if DEBUG
    member this.UInt16Matcher: RegexMatcher<uint16> = matcher :?> RegexMatcher<uint16>
    member this.ByteMatcher: RegexMatcher<byte> = matcher :?> RegexMatcher<byte>
#endif
