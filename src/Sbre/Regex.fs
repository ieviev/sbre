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
open Sbre.Types
open Sbre.Pat
open System.Runtime.InteropServices

[<Struct>]
type MatchResult = {
    Success: bool
    Value: string
    Index: int
    Length: int
}

[<CLIMutable>]
[<Struct>]
type MatchPosition = { Index: int; Length: int }


[<AbstractClass>]
type GenericRegexMatcher() =
    abstract member IsMatch: input:ReadOnlySpan<char> -> bool
    abstract member FindMatchEnd: input:ReadOnlySpan<char> -> int voption
    abstract member Replace: input:ReadOnlySpan<char> * replacement:ReadOnlySpan<char> -> string
    abstract member Matches: input:ReadOnlySpan<char> -> MatchResult seq
    abstract member MatchPositions: input:ReadOnlySpan<char> -> MatchPosition seq
    abstract member MatchText: input:ReadOnlySpan<char> -> string option
    abstract member Match: input:ReadOnlySpan<char> -> MatchResult
    abstract member Count: input:ReadOnlySpan<char> -> int



type MatchingState(node:RegexNode<TSet>) =
    member val Id = -1 with get,set
    member val Node = node with get,set
    member val Startset : TSet = Unchecked.defaultof<_> with get,set
    member val Flags : RegexStateFlags = RegexStateFlags.None with get,set

    member this.BuildFlags(c:RegexCache<TSet>) =
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
        this.Flags <- flags




[<Sealed>]
type RegexMatcher<'t
        when 't : struct>
        // and 't :> IEquatable< 't >
        // and 't: equality
        // >
        (
        dotStarredNode:RegexNode<TSet>,
        initialNode:RegexNode<TSet>,
        reverseNode:RegexNode<TSet>,
        _cache:RegexCache<TSet>) as m =
    inherit GenericRegexMatcher()
    // INITIALIZE

    let InitialDfaStateCapacity = 1024
    let _stateCache = Dictionary<RegexNode<TSet>,MatchingState>() // TODO: dictionary with char kind
    let mutable _stateArray = Array.zeroCreate<MatchingState> InitialDfaStateCapacity
    // let mutable _stateFlagsArray = Array.zeroCreate<RegexStateFlags> InitialDfaStateCapacity
    let _minterms = _cache.Minterms()
    let _mintermsLog = BitOperations.Log2(uint64 _minterms.Length) + 1
    // let _startsetPredicate = _cache.GetInitialStartsetPredicate
    let _initialInfo = initialNode.TryGetInfo
    let mutable _dfaDelta: int[] =
        Array.init (1024 <<< _mintermsLog) (fun _ -> 0 ) // 0 : initial state

    do
        // initial state: 1
        m.GetOrCreateState(dotStarredNode) |> ignore
        // --


    override this.IsMatch(input) =
        let mutable currPos = 0
        let mutable startLocation = Location.createSpan input currPos
        let mutable _toplevelOr = _cache.False
        match RegexNode.matchEnd _cache &startLocation dotStarredNode &_toplevelOr with
        | ValueNone -> false
        | ValueSome _ -> true

    override this.FindMatchEnd(input) =
        let mutable currPos = 0
        let mutable _toplevelOr = _cache.False
        let mutable startLocation = Location.createSpan input currPos
        RegexNode.matchEnd _cache &startLocation dotStarredNode &_toplevelOr


    override this.Match(input) : MatchResult =
        let firstMatch =
            this.MatchPositions(input) |> Seq.tryHead
        match firstMatch with
        | None ->
            {
                Success = false
                Value = ""
                Index = 0
                Length = 0
            }
        | Some result ->
            {
                Success = true
                Value = input.Slice(result.Index,result.Length).ToString()
                Index = result.Index
                Length = result.Length
            }

    /// replace all occurrences in string
    override this.Replace(input,replacement) =
        let sb = System.Text.StringBuilder(input.ToString())
        let mutable offset = 0
        for result in this.MatchPositions(input) do
            let start = offset + result.Index
            sb.Remove(start, result.Length + 1).Insert(start, replacement) |> ignore
            offset <-  replacement.Length - result.Length - 1
        sb.ToString()

    /// return all matches on input
    override this.Matches(input) =
        let mr = ResizeArray()
        for result in this.MatchPositions(input) do
            mr.Add({
                Success = true
                Value = input.Slice(result.Index,result.Length).ToString()
                Index = result.Index
                Length = result.Length
            })
        mr


    /// used internally
    override this.MatchText(input) =
        let mutable startPos = 0
        let mutable location = Location.createSpan input startPos
        let mutable _toplevelOr = _cache.False

        match RegexNode.matchEnd _cache &location dotStarredNode &_toplevelOr with
        | ValueNone -> None
        | ValueSome endPos ->
            location.Position <- endPos
            location.Reversed <- true

            let startPos =
                RegexNode.matchEnd _cache &location reverseNode &_toplevelOr

            match startPos with
            | ValueNone ->
                failwith
                    $"match succeeded left to right but not right to left:\nmatch end: {endPos}\nreverse pattern: {reverseNode}"
            | ValueSome start ->
                let s = input.Slice(start,endPos - start).ToString()
                Some(s)


    /// counts the number of matches
    override this.Count(input) =
        this.DfaCount(input)
        // if true then this.DfaCount(input) else
        // let mutable location = Location.createSpan input 0
        // let mutable looping = true
        // let mutable counter = 0
        // let _cache : RegexCache<_>  = this.Cache
        // let mutable _toplevelOr = _cache.False
        // failwith "todo prefix optimizations"
        // // let chars = _cache.GetInitialSearchValues()
        // //
        // // let initialPrefix = _cache.GetInitialStartsetPrefix()
        //
        // while looping do
        //     // use prefix optimizations
        //
        //     // _cache.TryNextStartsetLocationArray(&location,initialPrefix.Span,chars)
        //     match RegexNode.matchEnd _cache &location dotStarredNode &_toplevelOr with
        //     | ValueNone -> looping <- false
        //     | ValueSome(endPos: int) ->
        //         counter <- counter + 1
        //         if endPos < input.Length then
        //             _toplevelOr <- _cache.False
        //             if endPos = location.Position then
        //                 location.Position <- location.Position + 1
        //             else
        //                 location.Position <- endPos
        //         else
        //             looping <- false
        //
        // counter

    member this.CreateStartset(state:MatchingState, initial: bool) =

            if state.Flags.ContainsLookaround then
                state.Startset <- _cache.Solver.Empty
            else
            let minterms = _cache.Minterms()
            let derivatives =
                minterms
                |> Array.map (fun minterm ->
                    let temp_location = Location.getDefault()
                    match RegexNode.getCachedTransition (minterm, state.Node.TryGetInfo) with
                    | ValueSome v -> v
                    | _ -> createDerivative (_cache, &temp_location, minterm, state.Node)

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
                    (fun (d) -> not (refEq d state.Node) && not (refEq d _cache.False) )
                else
                    (fun (d) -> not (refEq d _cache.False) )

            // TODO: check for sequences
            let startsetPredicate =
                Seq.zip minterms derivatives
                |> Seq.where (fun (_,d) -> condition d )
                |> Seq.map fst
                |> Seq.fold (|||) _cache.Solver.Empty

            // invert empty startset (nothing to skip to)
            state.Startset <- startsetPredicate

            //
            if initial && _cache.Optimizations.IsUseful then
                match _cache.Optimizations.FindMode with
                | FindNextStartingPositionMode.FixedDistanceString_LeftToRight ->
                    // _cache.Optimizations.MinRequiredLength
                    // _cache.Optimizations.MaxPossibleLength
                    // failwith "TODO!"
                    state.Flags <- state.Flags ||| RegexStateFlags.UseDotnetOptimizations
                | FindNextStartingPositionMode.FixedDistanceSets_LeftToRight
                | FindNextStartingPositionMode.LeadingString_LeftToRight
                | FindNextStartingPositionMode.FixedDistanceString_LeftToRight  ->
                    state.Flags <- state.Flags ||| RegexStateFlags.UseDotnetOptimizations
                | FindNextStartingPositionMode.FixedDistanceChar_LeftToRight
                | FindNextStartingPositionMode.LeadingSet_LeftToRight -> ()
                | _ ->
                    failwith $"todo optimizations: {_cache.Optimizations.FindMode}"






    member private this.GetOrCreateState(node: RegexNode<TSet>) : MatchingState =
        match _stateCache.TryGetValue(node) with
        | true , v -> v
        | _ ->
            let state = MatchingState(node)
            _stateCache.Add(node,state)
            state.Id <- _stateCache.Count
            // TODO: grow state space if needed
            if _stateArray.Length = state.Id then
                if _stateArray.Length > 50000 then
                    failwith "state space blowup!"
                // failwith "TODO: resize DFA"
                let newsize = _stateArray.Length * 2
                Array.Resize(&_stateArray, newsize)
                Array.Resize(&_dfaDelta, newsize <<< _mintermsLog)

            _stateArray[state.Id] <- state
            state.BuildFlags(_cache)
            let isInitial = refEq dotStarredNode node
            if isInitial then
                state.Flags <- state.Flags ||| RegexStateFlags.InitialFlag
            this.CreateStartset(state, isInitial)
            if not (_cache.Solver.IsEmpty(state.Startset) || _cache.Solver.IsFull(state.Startset)) then
                state.Flags <- state.Flags ||| RegexStateFlags.CanSkipFlag
            else
                ()
                // failwith $"can not skip: {state.Node.ToString()}"

            // initialize counter:
            // match node with
            // | CounterNode(loop) ->
            //     let debug = 1
            //     failwith "TODO"
            //     ()
            // | _ -> ()
            // | HasCounterFlag = 128uy


            state


    member private this.GetDeltaOffset (stateId:int, mintermId:int) =
        (stateId <<< _mintermsLog) ||| mintermId

    member private this.TryNextDerivative(currentState: byref<int>, mintermId: int, loc:inref<Location>) =
        let minterm = _cache.MintermById(mintermId)
        let sourceState = _stateArray[currentState]
        let targetDerivative =
            match RegexNode.getCachedTransition (minterm, sourceState.Node.TryGetInfo) with
            | ValueSome v -> v
            | _ -> createDerivative (_cache, &loc, minterm, sourceState.Node)
        let targetState = this.GetOrCreateState(targetDerivative)
        targetState.Id

#if DEBUG
    member this.GetStateAndFlagsById (stateId:int) =
        _stateArray[stateId]
#endif

    member this.TryTakeTransition(currentState: byref<int>, mintermId: int, loc:inref<Location>) : bool =
        let dfaOffset = this.GetDeltaOffset(currentState, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        // existing transition
        if nextStateId > 0 then
            currentState <- nextStateId
            true
        else

        // new transition
        let targetState = _stateArray[nextStateId]
        if obj.ReferenceEquals(null,targetState) then
            // let minterm = _cache.MintermById(mintermId)
            // let sourceState = _stateArray[currentState]
            // let targetDerivative =
            //     match RegexNode.getCachedTransition (minterm, sourceState.Node.TryGetInfo) with
            //     | ValueSome v -> v
            //     | _ -> createDerivative (_cache, &loc, minterm, sourceState.Node)
            // let targetState = this.GetOrCreateState(targetDerivative)
            let nextState = this.TryNextDerivative(&currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState
        true

    /// end position with DFA
    member this.DfaEndPosition(cache: RegexCache<TSet>,
        loc: byref<Location>,
        activeNode: inref<RegexNode<TSet>>) =
        assert (loc.Position > -1)
        let mutable foundmatch = false
        let mutable currentStateId = 1
        let mutable currentMax = -2

        while not foundmatch do

            let state = _stateArray[currentStateId]
            let flags = state.Flags

            if flags.IsInitial && currentMax > 0 then
                foundmatch <- true

            if flags.CanSkip then
                if flags &&& RegexStateFlags.UseDotnetOptimizations = RegexStateFlags.UseDotnetOptimizations then
                    cache.Optimizations.TryFindNextStartingPositionLeftToRight(loc.Input, &loc.Position, loc.Position) |> ignore
                else
                    let ss = state.Startset
                    cache.TryNextStartsetLocation(&loc, ss)

            // set max nullability after skipping
            if flags.CanBeNullable && (flags.IsAlwaysNullable || RegexNode.isNullable(_cache,&loc,state.Node)) then
                currentMax <- loc.Position

            if loc.Position < loc.Input.Length then
                let _ =  this.TryTakeTransition(&currentStateId, _cache.MintermId(&loc), &loc)
                loc.Position <- Location.nextPosition loc
            else
                foundmatch <- true

        if foundmatch then
            if loc.Position > Location.final loc then
                loc.Position <- Location.final loc
            if currentMax < loc.Position &&
               _stateArray[currentStateId].Node.IsAlwaysNullable || RegexNode.isNullable (cache, &loc, _stateArray[currentStateId].Node) then
                currentMax <- loc.Position
            // elif initialNode.IsAlwaysNullable then
            //     currentMax <- loc.Position
        currentMax

    member this.DfaMatchEnds(input:ReadOnlySpan<char>) =
        let mutable loc = Location.createSpan input 0
        let mutable toplevelOr = dotStarredNode
        let eps = ResizeArray()
        let mutable looping = true
        while looping do
            match this.DfaEndPosition(_cache, &loc, &toplevelOr) with
            | -2 ->
                looping <- false // failed match
            | ep ->
                if loc.Position = loc.Input.Length then
                    looping <- false
                eps.Add ep
                loc.Position <- ep + 1
        eps

    member this.DfaCount(input:ReadOnlySpan<char>) =
        let mutable loc = Location.createSpan input 0
        let mutable toplevelOr = dotStarredNode
        let mutable counter = 0
        let mutable looping = true
        while looping do
            match this.DfaEndPosition(_cache, &loc, &toplevelOr) with
            | -2 ->
                looping <- false // failed match
            | ep ->
                if loc.Position = loc.Input.Length then
                    looping <- false
                counter <- counter + 1
                loc.Position <- ep + 1
        counter


    /// return just the positions of matches without allocating the result
    override this.MatchPositions(input) =

        let mutable looping = true
        let mutable _toplevelOr = _cache.False
        let _startState = _stateArray[1]
        let _initialFlags = _startState.Flags
        let mutable currMatchStart = 0
        let mutable location = Location.createSpan input 0
        let matchPositions = ResizeArray()
        while looping do
            location.Position <- currMatchStart

            _cache.TryNextStartsetLocation(&location, _startState.Startset)

            match RegexNode.matchEnd _cache &location dotStarredNode &_toplevelOr with
            | ValueNone -> looping <- false
            | ValueSome(endPos: int) ->
                location.Position <- endPos
                location.Reversed <- true
                let startPos =
                    RegexNode.matchEnd
                        _cache
                        &location
                        reverseNode
                        &_toplevelOr

                location.Reversed <- false
                match startPos with
                | ValueNone ->
                    failwith
                        $"match succeeded left to right but not right to left\nthis may occur because of an unimplemented feature\nend-pos:{endPos}, pattern:{reverseNode}"
                | ValueSome start ->
                    let startIdx = max currMatchStart start
                    let response: MatchPosition = {
                        Index = startIdx
                        Length = endPos - startIdx
                    }
                    matchPositions.Add response
                // continue
                if endPos < input.Length then
                    _toplevelOr <- _cache.False
                    if endPos = currMatchStart then
                        currMatchStart <- currMatchStart + 1
                    else
                        currMatchStart <- endPos
                else
                    looping <- false
        matchPositions


    // accessors
    member this.InitialPattern = dotStarredNode

    member this.RawPattern = initialNode

    member this.ReversePattern = reverseNode

    member this.Cache = _cache


module Helpers =
    let createMatcher
        (minterms: BDD array,charsetSolver,converter,trueStarPattern,symbolicBddnode, regexTree:RegexTree) : GenericRegexMatcher =


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
            let trueStarredNode  = (Minterms.transform uintbuilder charsetSolver solver) trueStarPattern
            let rawNode = (Minterms.transform uintbuilder charsetSolver solver) symbolicBddnode
            let optimizations = RegexFindOptimizations(regexTree.Root, RegexOptions.NonBacktracking)
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

#if DEBUG
            Debug.debuggerSolver <- Some solver
#endif
            RegexMatcher<uint64>(trueStarredNode,rawNode,reverseNode,cache) //:> GenericRegexMatcher
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
    let trueStarPattern: RegexNode<BDD> =
        regexBuilder.mkConcat2(implicitTrueStar, symbolicBddnode)
    let minterms = trueStarPattern |> Minterms.compute runtimeBddBuilder
    let matcher =
        Helpers.createMatcher(minterms,charsetSolver,converter,trueStarPattern,symbolicBddnode,regexTree)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Count(input) = matcher.Count(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.FindMatchEnd(input) = matcher.FindMatchEnd(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.IsMatch(input) = matcher.IsMatch(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.MatchPositions(input) = matcher.MatchPositions(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.MatchText(input) = matcher.MatchText(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Matches(input) = matcher.Matches(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Replace (input,replacement) = matcher.Replace(input,replacement)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Match(input) = matcher.Match(input)

    member this.Matcher : GenericRegexMatcher = matcher
#if DEBUG
    member this.TSetMatcher : RegexMatcher<TSet> = matcher :?> RegexMatcher<TSet>
    member this.UInt16Matcher : RegexMatcher<uint16> = matcher :?> RegexMatcher<uint16>
    member this.ByteMatcher : RegexMatcher<byte> = matcher :?> RegexMatcher<byte>
#endif





