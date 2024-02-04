namespace Sbre

open System
open System.Buffers
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Runtime.CompilerServices
open System.Text.RegularExpressions.Symbolic
open System.Text.RuntimeRegexCopy.Symbolic
open System.Text.RuntimeRegexCopy
open Microsoft.FSharp.Core
open Sbre.Algorithm
open Sbre.CountingSet
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



type MatchingState(node: RegexNode<TSet>) =
    member val Id = -1 with get, set
    member val Node = node with get, set
    member val Startset: TSet = Unchecked.defaultof<_> with get, set
    member val Flags: RegexStateFlags = RegexStateFlags.None with get, set

    // -- optimizations
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

        this.Flags <- flags

    member this.SetStartset(c: RegexCache<TSet>, set: TSet) =
        let setChars = c.MintermStartsetChars(set)
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
    let _minterms = _cache.Minterms()
    let _mintermsLog = BitOperations.Log2(uint64 _minterms.Length) + 1
    let _initialInfo = initialNode.TryGetInfo

    let _initialIsNegation =
        match initialNode with
        | Not(_) -> true
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
                let temp_location = Location.getDefault ()
                let blankState = RegexState(_cache.NumOfMinterms())

                match RegexNode.getCachedTransition (minterm, state.Node.TryGetInfo) with
                | ValueSome v -> v
                | _ -> createDerivative (_cache, blankState, &temp_location, minterm, state.Node)
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
                (fun (d) -> not (refEq d state.Node) && not (refEq d _cache.False))
            else
                (fun (d) -> not (refEq d state.Node))

        // TODO: check for sequences
        let startsetPredicate =
            Seq.zip minterms derivatives
            |> Seq.where (fun (_, d) -> condition d)
            |> Seq.map fst
            |> Seq.fold (|||) _cache.Solver.Empty

        // let dbg_startset = _cache.PrettyPrintMinterm(startsetPredicate)
        // invert empty startset (nothing to skip to)
        state.SetStartset(_cache, startsetPredicate)
        // state.Startset <- startsetPredicate

        //
        if initial && _cache.Optimizations.IsUseful then
            match _cache.Optimizations.FindMode with
            // | FindNextStartingPositionMode.FixedDistanceString_LeftToRight ->
            //     // _cache.Optimizations.MinRequiredLength
            //     // _cache.Optimizations.MaxPossibleLength
            //     state.Flags <- state.Flags ||| RegexStateFlags.UseDotnetOptimizations
            // | FindNextStartingPositionMode.FixedDistanceSets_LeftToRight
            // | FindNextStartingPositionMode.LeadingString_LeftToRight
            // | FindNextStartingPositionMode.FixedDistanceString_LeftToRight  ->
            //     state.Flags <- state.Flags ||| RegexStateFlags.UseDotnetOptimizations
            // | FindNextStartingPositionMode.FixedDistanceChar_LeftToRight
            // | FindNextStartingPositionMode.LeadingSet_LeftToRight -> ()
            // ------------------------------
            | FindNextStartingPositionMode.LeadingString_RightToLeft ->
                state.Flags <- state.Flags ||| RegexStateFlags.UseDotnetOptimizations
            | FindNextStartingPositionMode.LeadingChar_RightToLeft
            | FindNextStartingPositionMode.LeadingSet_RightToLeft -> ()
            | _ -> ()
    // failwith $"todo optimizations: {_cache.Optimizations.FindMode}"

    let _getOrCreateState(node, isInitial) =
        match _stateCache.TryGetValue(node) with
        | true, v -> v // a dfa state already exists for this regex
        | _ ->
            let state = MatchingState(node)
            _stateCache.Add(node, state)
            state.Id <- _stateCache.Count

            // TODO: grow state space if needed (? probably never needed)
            if _stateArray.Length = state.Id then
                if _stateArray.Length > 50000 then
                    failwith "state space blowup!"
                // failwith "TODO: resize DFA"
                let newsize = _stateArray.Length * 2
                Array.Resize(&_stateArray, newsize)
                Array.Resize(&_dfaDelta, newsize <<< _mintermsLog)

            _stateArray[state.Id] <- state
            state.BuildFlags(_cache)
            // let isInitial = refEq trueStarredNode node
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
            // failwith $"can not skip: {state.Node.ToString()}"

            // state depends on counter
            // let rec stateDependsOnCounter (node:RegexNode<TSet>) =
            //     if node.HasCounter then true else
            //     match node with
            //     // | Concat(head, tail, info) when head.CanBeNullable -> stateDependsOnCounter tail
            //     | Or(nodes, info) when head.CanBeNullable -> stateDependsOnCounter tail
            //     | _ -> false

            // if stateDependsOnCounter node then
            if node.HasCounter then
                state.Flags <- state.Flags ||| RegexStateFlags.HasCounterFlag
            // | HasCounterFlag = 128uy

            // workaround to detect match end
            // match node with
            // | Or(nodes, info) when info.NodeFlags.HasCounter ->
            //     if nodes.Count = 2 && nodes.Contains(initialNode) then
            //         state.Flags <- state.Flags ||| RegexStateFlags.InitialFlag
            // | _ -> ()

            state

    // T*R
    let DFA_TR = _getOrCreateState(trueStarredNode, true).Id // T*R: 1
    // R
    let DFA_R = _getOrCreateState(initialNode, false).Id // R: 2
    // R_rev
    let DFA_R_rev = _getOrCreateState(reverseNode, false).Id
    // ⊤*(R_rev)
    let DFA_TR_rev = _getOrCreateState(reverseTrueStarredNode, true).Id // R_rev


    override this.IsMatch(input) =
        let mutable currPos = 0
        let mutable startLocation = Location.createSpan input currPos
        let mutable _toplevelOr = _cache.False
        let rstate = RegexState(_cache.NumOfMinterms())

        match this.DfaEndPosition(rstate, &startLocation, 1) with
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

        for result in this.llmatch_all (input) do
            mr.Add(
                {
                    Value = input.Slice(result.Index, result.Length).ToString()
                    Index = result.Index
                    Length = result.Length
                }
            )

        mr

    /// counts the number of matches
    override this.Count(input) = this.llmatch_all_count_only (input)

    member this.CreateStartset(state: MatchingState, initial: bool) =
        _createStartset (state, initial)

    /// initialize regex in DFA
    member this.GetOrCreateState(node: RegexNode<TSet>) : MatchingState =
        _getOrCreateState (node, false)

    member private this.GetDeltaOffset(stateId: int, mintermId: int) =
        (stateId <<< _mintermsLog) ||| mintermId

    member private this.TryNextDerivative
        (
            regexState: CountingSet.RegexState,
            currentState: byref<int>,
            mintermId: int,
            loc: inref<Location>
        ) =
        let minterm = _cache.MintermById(mintermId)
        let sourceState = _stateArray[currentState]

        let targetDerivative =
            createDerivative (_cache, regexState, &loc, minterm, sourceState.Node)

        let targetState = this.GetOrCreateState(targetDerivative)
        targetState.Id

#if DEBUG
    member this.GetStateAndFlagsById(stateId: int) = _stateArray[stateId]
#endif

    member this.TakeTransition
        (
            rstate: CountingSet.RegexState,
            flags: RegexStateFlags,
            currentState: byref<int>,
            mintermId: int,
            loc: inref<Location>
        ) =
        let dfaOffset = this.GetDeltaOffset(currentState, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        // caching workaround
        if flags.ContainsLookaround || flags.HasCounter then
            let nextState = this.TryNextDerivative(rstate, &currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState
        else if

            // existing transition in dfa
            nextStateId > 0
        then
            currentState <- nextStateId
        else

        // new transition
        let targetState = _stateArray[nextStateId]

        if obj.ReferenceEquals(null, targetState) then
            let nextState = this.TryNextDerivative(rstate, &currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState

    member private this.RemoveInitialBranch(initial: RegexNode<_>, node: RegexNode<_>) =
        match node with
        | Or(nodes, info) -> _cache.Builder.mkOr (nodes.Remove(initial))
        | _ -> node

    member private this.StateIsNullable
        (
            flags: RegexStateFlags,
            rstate,
            loc: byref<Location>,
            dfaState: MatchingState
        ) : bool =
        flags.CanBeNullable && flags.IsAlwaysNullable
        || RegexNode.isNullable (_cache, rstate, &loc, dfaState.Node)

    /// end position with DFA
    member this.DfaEndPosition
        (
            rstate: RegexState,
            loc: byref<Location>,
            startStateId: int
#if DEBUG
            ,
            ?debugFn: (MatchingState * CountingSet.RegexState -> unit)
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
            debugFn |> Option.iter (fun fn -> fn (dfaState, rstate))
#endif
            if flags.IsDeadend then
                looping <- false
            else

            if flags.CanSkip then
                // if flags &&& RegexStateFlags.UseDotnetOptimizations = RegexStateFlags.UseDotnetOptimizations then
                //     _cache.Optimizations.TryFindNextStartingPositionLeftToRight(loc.Input, &loc.Position, loc.Position) |> ignore
                // else
                let ss = dfaState.Startset
                _cache.TryNextStartsetLocation(&loc, ss)

            // set max nullability after skipping
            if this.StateIsNullable(flags, rstate, &loc, dfaState) then
                currentMax <- loc.Position

            if loc.Position < loc.Input.Length then
                // let mintermId = _cache.MintermId(&loc)
                if flags.HasCounter then
                    failwith "todo: counters"
                //     let mintermId = _cache.MintermId(&loc)
                //     CountingSet.bumpCounters rstate (_cache.MintermById(mintermId)) dfaState.Node
                // if flags.HasCounter then
                //     rstate.ActiveCounters |> Seq.iter (_.Value.TryReset())
                //     CountingSet.stepCounters rstate (_cache.MintermById(mintermId))

                this.TakeTransition(rstate, flags, &currentStateId, _cache.MintermId(&loc), &loc)

                loc.Position <- Location.nextPosition loc
            else
                looping <- false

        currentMax


    /// unoptimized collect all nullable positions
    member this.CollectReverseNullablePositions
        (
            acc: SharedResizeArray<int>,
            rstate: RegexState,
            loc: byref<Location>
        ) : SharedResizeArray<int> =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)
        let mutable looping = true
        let mutable currentStateId = DFA_TR_rev

        while looping do
            let dfaState = _stateArray[currentStateId]
            let flags = dfaState.Flags
#if SKIP
            if flags.CanSkip then
                _cache.TryNextStartsetLocationRightToLeft(
                    &loc,
                    dfaState.StartsetChars,
                    dfaState.StartsetIsInverted
                )
#endif
            if this.StateIsNullable(flags, rstate, &loc, dfaState) then
                acc.Add loc.Position

            if loc.Position > 0 && looping then
                this.TakeTransition(rstate, flags, &currentStateId, _cache.MintermId(&loc), &loc)
                loc.Position <- Location.nextPosition loc
            else
                looping <- false

        acc

    member this.llmatch_all(input: ReadOnlySpan<char>) : ResizeArray<MatchPosition> =

        let matches = ResizeArray(100)
        let mutable loc = Location.createReversedSpan input
        let rstate = RegexState(_cache.NumOfMinterms())
        use acc = new SharedResizeArray<int>(100)
        let allPotentialStarts = this.CollectReverseNullablePositions(acc, rstate, &loc)
        loc.Reversed <- false
        let mutable nextValidStart = 0
        let startSpans = allPotentialStarts.AsSpan()
        // last reverse nullable pos: first match start
        // for i = (allPotentialStarts.Count - 1) downto 0 do
        for i = (startSpans.Length - 1) downto 0 do
            let currStart = startSpans[i]

            if currStart >= nextValidStart then
                loc.Position <- currStart
                rstate.Clear()
                let matchEnd = this.DfaEndPosition(rstate, &loc, DFA_R)
                matches.Add({ MatchPosition.Index = currStart; Length = (matchEnd - currStart) })

                nextValidStart <- matchEnd

        matches

    member this.llmatch_all_count_only(input: ReadOnlySpan<char>) : int =

        let mutable matchCount = 0
        let mutable loc = Location.createReversedSpan input
        let rstate = RegexState(_cache.NumOfMinterms())
        use acc = new SharedResizeArray<int>(100)
        let allPotentialStarts = this.CollectReverseNullablePositions(acc, rstate, &loc)
        loc.Reversed <- false
        let mutable nextValidStart = 0
        let startSpans = allPotentialStarts.AsSpan()
        // last reverse nullable pos: first match start
        // for i = (allPotentialStarts.Count - 1) downto 0 do
        for i = (startSpans.Length - 1) downto 0 do
            let currStart = startSpans[i]

            if currStart >= nextValidStart then
                loc.Position <- currStart
                rstate.Clear()
                let matchEnd = this.DfaEndPosition(rstate, &loc, DFA_R)
                matchCount <- matchCount + 1
                nextValidStart <- matchEnd

        matchCount

    /// return just the positions of matches without allocating the result
    override this.MatchPositions(input) = this.llmatch_all (input)


    // accessors
    member this.TrueStarredPattern = trueStarredNode
    member this.ReverseTrueStarredPattern = reverseTrueStarredNode

    member this.RawPattern = initialNode

    member this.ReversePattern = reverseNode

    member this.Cache = _cache


module Helpers =
    let createMatcher
        (
            minterms: BDD array,
            charsetSolver,
            converter,
            trueStarPattern,
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
            let uintbuilder = RegexBuilder(converter, solver, charsetSolver)

            let trueStarredNode =
                (Minterms.transform uintbuilder charsetSolver solver) trueStarPattern

            let rawNode = (Minterms.transform uintbuilder charsetSolver solver) symbolicBddnode

            if not (regexTree.Root.Options.HasFlag(RegexOptions.RightToLeft)) then
                regexTree.Root.Options <- RegexOptions.RightToLeft
            // let reverseRegexRoot = RegexNode(RegexNodeKind.Capture, RegexOptions.NonBacktracking)
            // for i = regexTree.Root.ChildCount() - 1 downto 0 do
            //     reverseRegexRoot.AddChild(regexTree.Root.Child(i))
            // let optimizations = RegexFindOptimizations(regexTree.Root, RegexOptions.RightToLeft )
            // these only work left to right
            let optimizations = RegexFindOptimizations(regexTree.Root, RegexOptions.RightToLeft)
            let reverseNode = RegexNode.rev uintbuilder rawNode

            let cache =
                Sbre.RegexCache(
                    solver,
                    charsetSolver,
                    _implicitDotstarPattern = trueStarredNode,
                    _rawPattern = rawNode,
                    _reversePattern = reverseNode,
                    _builder = uintbuilder,
                    _optimizations = optimizations
                )

            let revTrueStarred = cache.Builder.mkConcat2 (cache.TrueStar, reverseNode)


            // let rev_optimizations =
            //     Optimizations.tryGetReversePrefix cache reverseNode

#if DEBUG
            Debug.debuggerSolver <- Some solver
#endif
            RegexMatcher<uint64>(trueStarredNode, revTrueStarred, rawNode, reverseNode, cache) //:> GenericRegexMatcher
        | n -> failwith $"bitvector too large, size: {n}"




[<Sealed>]
type Regex(pattern: string, [<Optional; DefaultParameterValue(false)>] experimental: bool) =
    inherit GenericRegexMatcher()
    let pattern = pattern.Replace("⊤", @"[\s\S]")
    // experimental parser!
    let regexTree =
        ExtendedRegexParser.Parse(
            pattern,
            RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking,
            CultureInfo.InvariantCulture
        )

    let charsetSolver = CharSetSolver()
    let runtimeBddBuilder = SymbolicRegexBuilder<BDD>(charsetSolver, charsetSolver)
    let converter = RegexNodeConverter(runtimeBddBuilder, null)
    let regexBuilder = RegexBuilder(converter, charsetSolver, charsetSolver)

    let symbolicBddnode: RegexNode<BDD> =
        RegexNodeConverter.convertToSymbolicRegexNode (
            charsetSolver,
            runtimeBddBuilder,
            regexBuilder,
            regexTree.Root
        )

    let implicitTrueStar = regexBuilder.trueStar
    let minterms = symbolicBddnode |> Minterms.compute runtimeBddBuilder

    let trueStarPattern: RegexNode<BDD> =
        regexBuilder.mkConcat2 (implicitTrueStar, symbolicBddnode)

    let matcher =
        Helpers.createMatcher (
            minterms,
            charsetSolver,
            converter,
            trueStarPattern,
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
#if DEBUG
    member this.TSetMatcher: RegexMatcher<TSet> = matcher :?> RegexMatcher<TSet>
    member this.UInt16Matcher: RegexMatcher<uint16> = matcher :?> RegexMatcher<uint16>
    member this.ByteMatcher: RegexMatcher<byte> = matcher :?> RegexMatcher<byte>
#endif
