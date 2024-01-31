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
    with
    member this.GetText(input:ReadOnlySpan<char>) =
        input.Slice(this.Index, this.Length).ToString()


[<AbstractClass>]
type GenericRegexMatcher() =
    abstract member IsMatch: input:ReadOnlySpan<char> -> bool
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


type RegexSearchMode =
    | FirstNullable
    | MatchEnd

[<Sealed>]
type RegexMatcher<'t
        when 't : struct>
        // and 't :> IEquatable< 't >
        // and 't: equality
        // >
        (
        trueStarredNode:RegexNode<TSet>,
        initialNode:RegexNode<TSet>,
        reverseNode:RegexNode<TSet>,
        _cache:RegexCache<TSet>) =
    inherit GenericRegexMatcher()
    // INITIALIZE
    let InitialDfaStateCapacity = 1024
    let _stateCache = Dictionary<RegexNode<TSet>,MatchingState>()
    let mutable _stateArray = Array.zeroCreate<MatchingState> InitialDfaStateCapacity
    let _minterms = _cache.Minterms()
    let _mintermsLog = BitOperations.Log2(uint64 _minterms.Length) + 1
    let _initialInfo = initialNode.TryGetInfo
    let _initialIsNegation = match initialNode with | Not (_) -> true | _ -> false

    let mutable _dfaDelta: int[] =
        Array.init (1024 <<< _mintermsLog) (fun _ -> 0 ) // 0 : initial state

    let _getOrCreateState(node) =
        match _stateCache.TryGetValue(node) with
        | true , v -> v // a dfa state already exists for this regex
        | _ ->
            let state = MatchingState(node)
            _stateCache.Add(node,state)
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
            let isInitial = refEq trueStarredNode node
            if isInitial then
                state.Flags <- state.Flags ||| RegexStateFlags.InitialFlag

            // generate startset
            // this.CreateStartset(state, isInitial)
            // if not (_cache.Solver.IsEmpty(state.Startset) || _cache.Solver.IsFull(state.Startset)) then
            //     state.Flags <- state.Flags ||| RegexStateFlags.CanSkipFlag
            // else
            //     ()
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
    let DFA_TR= _getOrCreateState(trueStarredNode).Id // T*R: 1
    // R
    let DFA_R = _getOrCreateState(initialNode).Id // R: 2
    // R_rev
    let DFA_R_rev = _getOrCreateState(reverseNode).Id
    // ⊤*(R_rev)
    let DFA_TR_rev = _getOrCreateState(_cache.Builder.mkConcat2( _cache.TrueStar, reverseNode )).Id // R_rev


    override this.IsMatch(input) =
        let mutable currPos = 0
        let mutable startLocation = Location.createSpan input currPos
        let mutable _toplevelOr = _cache.False
        match this.DfaEndPosition(&startLocation, 1, RegexSearchMode.FirstNullable) with
        | -2 -> false
        | _ -> true


    member this.FindMatchEnd(input) =
        failwith "old"
        let mutable currPos = 0
        let mutable _toplevelOr = _cache.False
        let mutable startLocation = Location.createSpan input currPos
        RegexNode.matchEnd _cache &startLocation trueStarredNode &_toplevelOr



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


    member this.ThreePassMatch(
        loc: byref<Location>
        ) =
        let trueStarredId : int = this.GetOrCreateState(trueStarredNode).Id
        let reverseStateId = this.GetOrCreateState(reverseNode).Id
        match this.DfaEndPosition(&loc, trueStarredId, RegexSearchMode.FirstNullable) with
        | -2 -> None
        | firstNullable ->

        loc.Position <- firstNullable
        loc.Reversed <- true

        match this.DfaStartPosition(&loc, reverseStateId) with
        | -2 -> None
        | earliestStart ->

        loc.Position <- earliestStart
        loc.Reversed <- false

        match this.DfaEndPosition(&loc, _getOrCreateState(initialNode).Id, RegexSearchMode.MatchEnd) with
        | -2 -> None
        | matchEnd ->
            Some { MatchPosition.Index = earliestStart; Length = matchEnd - earliestStart }



    /// used internally
    override this.MatchText(input) =
        let mutable startPos = 0
        let mutable location = Location.createSpan input startPos
        let mutable _toplevelOr = _cache.False
        match this.ThreePassMatch(&location) with
        | None -> None
        | Some v ->
            Some (input.Slice(v.Index,v.Length).ToString())



    /// counts the number of matches
    override this.Count(input) = this.DfaCount(input)

    member this.CreateStartset(state:MatchingState, initial: bool) =

            if state.Flags.ContainsLookaround then
                state.Startset <- _cache.Solver.Empty
            else
            let minterms = _cache.Minterms()
            let derivatives =
                minterms
                |> Array.map (fun minterm ->
                    let temp_location = Location.getDefault()
                    let blankState = RegexState(_cache.NumOfMinterms())
                    match RegexNode.getCachedTransition (minterm, state.Node.TryGetInfo) with
                    | ValueSome v -> v
                    | _ ->
                        createDerivative (_cache, blankState, &temp_location, minterm, state.Node)
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


    /// initialize regex in DFA
    member this.GetOrCreateState(node: RegexNode<TSet>) : MatchingState = _getOrCreateState(node)

    member private this.GetDeltaOffset (stateId:int, mintermId:int) =
        (stateId <<< _mintermsLog) ||| mintermId

    member private this.TryNextDerivative(regexState: CountingSet.RegexState, currentState: byref<int>, mintermId: int, loc:inref<Location>) =
        let minterm = _cache.MintermById(mintermId)
        let sourceState = _stateArray[currentState]
        let targetDerivative = createDerivative (_cache, regexState, &loc, minterm, sourceState.Node)
        let targetState = this.GetOrCreateState(targetDerivative)
        targetState.Id

#if DEBUG
    member this.GetStateAndFlagsById (stateId:int) =
        _stateArray[stateId]
#endif

    member this.TryTakeTransition(rstate: CountingSet.RegexState, currentState: byref<int>, mintermId: int, loc:inref<Location>) : bool =
        let dfaOffset = this.GetDeltaOffset(currentState, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        // existing transition in dfa
        if nextStateId > 0 then
            currentState <- nextStateId
            true
        else

        // new transition
        let targetState = _stateArray[nextStateId]
        if obj.ReferenceEquals(null,targetState) then
            let nextState = this.TryNextDerivative(rstate, &currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState
        true

    member private this.RemoveInitialBranch (initial:RegexNode<_>,node:RegexNode<_>) =
        match node with
        | Or(nodes, info) -> _cache.Builder.mkOr(nodes.Remove(initial))
        | _ -> node


    /// end position with DFA
    member this.DfaEndPosition(
        loc: byref<Location>,
        startStateId: int,
        searchMode: RegexSearchMode,
        ?debugFn: (MatchingState * CountingSet.RegexState -> unit) ) : int32 =

        assert (loc.Position > -1)

        let mutable foundmatch = false
        let mutable currentStateId = startStateId
        let mutable currentMax = -2

        let rstate = CountingSet.RegexState(_cache.NumOfMinterms())
        // let mutable hasBeenNullable = RegexNode.isNullable(_cache,rstate,&loc,startStateId)
        // let initialIsNegated =
        //     hasBeenNullable
        //     && match startStateId with | Concat(head,Not _,info) -> true | _ -> false
        // // if initial is nullable dont add T*
        // if hasBeenNullable then
        //     currentStateId <- this.GetOrCreateState(dotStarredNode ).Id

        while not foundmatch do
            let mutable dfaState = _stateArray[currentStateId]
            let flags = dfaState.Flags
#if DEBUG
            debugFn |> Option.iter (fun fn -> fn (dfaState, rstate))
#endif
            if currentMax > 0 && flags.IsInitial then
                foundmatch <- true

            if flags.CanSkip then
                // if flags &&& RegexStateFlags.UseDotnetOptimizations = RegexStateFlags.UseDotnetOptimizations then
                //     cache.Optimizations.TryFindNextStartingPositionLeftToRight(loc.Input, &loc.Position, loc.Position) |> ignore
                // else
                let ss = dfaState.Startset
                _cache.TryNextStartsetLocation(&loc, ss)

            // set max nullability after skipping
            let isNullable = RegexNode.isNullable(_cache,rstate,&loc,dfaState.Node)
            if isNullable then
                currentMax <- loc.Position
                match searchMode with
                | RegexSearchMode.FirstNullable ->
                    foundmatch <- true
                | RegexSearchMode.MatchEnd -> ()

            // if (initialNullable && not isNullable) || (not initialNullable && isNullable) then
            //     if initialNullable then foundmatch <- true

            if loc.Position < loc.Input.Length && not foundmatch then
                let mintermId = _cache.MintermId(&loc)

                if flags.HasCounter then
                    let mintermId = _cache.MintermId(&loc)
                    CountingSet.bumpCounters rstate (_cache.MintermById(mintermId)) dfaState.Node
                if flags.HasCounter then
                    rstate.ActiveCounters |> Seq.iter (_.Value.TryReset())
                    CountingSet.stepCounters rstate (_cache.MintermById(mintermId))

                if flags.HasCounter || flags.ContainsLookaround then
                    let nextState = this.TryNextDerivative(
                        rstate, &currentStateId, mintermId, &loc)
                    currentStateId <- nextState
                else
                    let _ =  this.TryTakeTransition(rstate, &currentStateId, _cache.MintermId(&loc), &loc)

                    ()
                loc.Position <- Location.nextPosition loc
            else
                foundmatch <- true

        if foundmatch then
            if currentMax < loc.Position &&
               _stateArray[currentStateId].Node.IsAlwaysNullable || RegexNode.isNullable (_cache,rstate, &loc, _stateArray[currentStateId].Node) then
                currentMax <- loc.Position
        currentMax


    /// unoptimized collect all nullable positions
    member this.CollectReverseNullablePositions(
        loc: byref<Location>) : ResizeArray<int> =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)
        let mutable looping = true
        let mutable currentStateId = DFA_TR_rev
        let rstate = RegexState(_cache.NumOfMinterms())
        let mutable nullablePositions_rev = ResizeArray<int>()

        while looping do
            let dfaState = _stateArray[currentStateId]
            let flags = dfaState.Flags
            let isNullable = RegexNode.isNullable(_cache,rstate,&loc,dfaState.Node)
            if isNullable then
                nullablePositions_rev.Add loc.Position

            if loc.Position > 0 && looping then
                let loc_pred = _cache.MintermId(&loc)
                let nextState = this.TryNextDerivative(
                    rstate, &currentStateId, loc_pred, &loc)
                currentStateId <- nextState
                loc.Position <- Location.nextPosition loc
            else
                looping <- false

        nullablePositions_rev

    member this.DfaSweepNullableRanges(
        loc: byref<Location>,
        trueStarredReversePos: int) : ResizeArray<struct (int*int)> =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)
        let mutable foundmatch = false
        let tr_rev = _cache.Builder.mkConcat2( _cache.TrueStar, reverseNode )
        let mutable currentStateId = this.GetOrCreateState(tr_rev).Id

        let mutable currentMax = -2
        let rstate = CountingSet.RegexState(_cache.NumOfMinterms())
        let mutable nullableRanges = ResizeArray<struct (int*int)>()
        let mutable nullableStart = -2

        do
            let initialState = _stateArray[currentStateId]
            if RegexNode.isNullable(_cache,rstate,&loc,initialState.Node) then
                nullableStart <- loc.Position

        while not foundmatch do
            let dfaState = _stateArray[currentStateId]
            let flags = dfaState.Flags

            if flags.CanSkip then
                // if flags &&& RegexStateFlags.UseDotnetOptimizations = RegexStateFlags.UseDotnetOptimizations then
                //     cache.Optimizations.TryFindNextStartingPositionLeftToRight(loc.Input, &loc.Position, loc.Position) |> ignore
                // else
                let ss = dfaState.Startset
                _cache.TryNextStartsetLocation(&loc, ss)

            // set max nullability after skipping


            if loc.Position > 0 && not foundmatch then
                let loc_pred = _cache.MintermId(&loc)
                if flags.HasCounter then
                    rstate.ActiveCounters |> Seq.iter (_.Value.TryReset())
                    CountingSet.stepCounters rstate (_cache.MintermById(loc_pred))

                if flags.HasCounter || flags.ContainsLookaround then
                    let nextState = this.TryNextDerivative(
                        rstate, &currentStateId, loc_pred, &loc)
                    currentStateId <- nextState
                else
                    let _ =  this.TryTakeTransition(rstate, &currentStateId, _cache.MintermId(&loc), &loc)
                    ()
                loc.Position <- Location.nextPosition loc

                let ms = _stateArray[currentStateId]
                let isNullable = flags.IsAlwaysNullable || RegexNode.isNullable(_cache,rstate,&loc,ms.Node)
                match isNullable with
                // end nullable range
                | false when nullableStart > 0 ->
                    nullableRanges.Add(nullableStart, loc.Position)
                    nullableStart <- -2
                | true when nullableStart < 0 ->
                    nullableStart <- loc.Position
                | _ -> ()
            else
                foundmatch <- true

        nullableRanges

    member this.llmatch_all(
        input:ReadOnlySpan<char>) : ResizeArray<MatchPosition> =
        let matches = ResizeArray()
        let mutable loc = Location.createReversedSpan input
        let allPotentialStarts = this.CollectReverseNullablePositions(&loc)
        loc.Reversed <- false
        let mutable nextValidStart = 0
        let mutable lastMatchEnd = -2
        // last reverse nullable pos: first match start
        for i = (allPotentialStarts.Count - 1) downto 0 do
            let currStart = allPotentialStarts[i]
            if currStart >= nextValidStart then
                loc.Position <- currStart
                let matchEnd = this.DfaEndPosition(&loc, DFA_R, RegexSearchMode.MatchEnd)

                // 0 length matches that already end a match dont count as matches
                if not (matchEnd = currStart && currStart = lastMatchEnd) then
                    lastMatchEnd <- matchEnd
                    matches.Add( {
                          MatchPosition.Index = currStart
                          Length = (matchEnd - currStart) })


                nextValidStart <- matchEnd
                // a match till the end of the string should not start another match
                if nextValidStart = input.Length then
                    nextValidStart <- Int32.MaxValue



        matches


    /// end position with DFA
    member this.DfaStartPosition(
        loc: byref<Location>,
        reverseStateId: int) : int32 =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)
        let mutable foundmatch = false
        let mutable currentStateId = reverseStateId
        let mutable currentMax = -2
        let rstate = CountingSet.RegexState(_cache.NumOfMinterms())

        while not foundmatch do
            let dfaState = _stateArray[currentStateId]
            let flags = dfaState.Flags

            if currentMax > 0 && (flags.IsInitial)  then
                foundmatch <- true

            if flags.CanSkip then
                // if flags &&& RegexStateFlags.UseDotnetOptimizations = RegexStateFlags.UseDotnetOptimizations then
                //     cache.Optimizations.TryFindNextStartingPositionLeftToRight(loc.Input, &loc.Position, loc.Position) |> ignore
                // else
                let ss = dfaState.Startset
                _cache.TryNextStartsetLocation(&loc, ss)

            // set max nullability after skipping
            let isNullable = flags.IsAlwaysNullable || RegexNode.isNullable(_cache,rstate,&loc,dfaState.Node)
            if isNullable then
                currentMax <- loc.Position

            if loc.Position > 0 && not foundmatch then
                let loc_pred = _cache.MintermId(&loc)
                if flags.HasCounter then
                    rstate.ActiveCounters |> Seq.iter (_.Value.TryReset())
                    CountingSet.stepCounters rstate (_cache.MintermById(loc_pred))

                if flags.HasCounter || flags.ContainsLookaround then
                    let nextState = this.TryNextDerivative(
                        rstate, &currentStateId, loc_pred, &loc)
                    currentStateId <- nextState
                else
                    let _ =  this.TryTakeTransition(rstate, &currentStateId, _cache.MintermId(&loc), &loc)
                    ()
                loc.Position <- Location.nextPosition loc
            else
                foundmatch <- true


        if foundmatch then
            if currentMax > loc.Position &&
               _stateArray[currentStateId].Node.IsAlwaysNullable || RegexNode.isNullable (_cache,rstate, &loc, _stateArray[currentStateId].Node) then
                currentMax <- loc.Position
        currentMax

    member this.GetTruestarredStateId(loc:inref<Location>) =
        // let blankState = RegexState(_cache.NumOfMinterms())
        // if RegexNode.isNullable(_cache, blankState, &loc, dotStarredNode) then
        //     this.GetOrCreateState(initialNode)
        this.GetOrCreateState(trueStarredNode)

    member this.DfaFullSingleMatch(input:ReadOnlySpan<char>) : (int*int) voption =
        let mutable loc = Location.createSpan input 0
        let mutable toplevelOr = trueStarredNode
        let state = RegexState(_cache.NumOfMinterms())
        let initState = this.GetTruestarredStateId(&loc)
        let eps = ResizeArray()
        failwith "TODO matchtext"
        // let mutable looping = true
        // while looping do
        //     match this.DfaEndPosition( &loc, initState.Id) with
        //     | -2 ->
        //         looping <- false // failed match
        //     | ep ->
        //         if loc.Position = loc.Input.Length then
        //             looping <- false
        //         eps.Add ep
        //         loc.Position <- ep + 1
        // eps

    member this.DfaCount(input:ReadOnlySpan<char>) =
        let mutable loc = Location.createSpan input 0
        let mutable toplevelOr = trueStarredNode
        let state = RegexState(_cache.NumOfMinterms())
        let initState = this.GetTruestarredStateId(&loc)
        let mutable counter = 0
        let mutable looping = true
        failwith "TODO matchtext"
        // while looping do
        //     match this.DfaEndPosition( &loc, initState.Id) with
        //     | -2 ->
        //         looping <- false // failed match
        //     | ep ->
        //         if loc.Position = loc.Input.Length then
        //             looping <- false
        //         counter <- counter + 1
        //         loc.Position <- ep + 1
        // counter


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

            match RegexNode.matchEnd _cache &location trueStarredNode &_toplevelOr with
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


    member this.DfaMatchPositions(input) =

        let mutable looping = true
        let mutable _toplevelOr = _cache.False
        let _startState = _stateArray[1]
        let _initialFlags = _startState.Flags
        let mutable currMatchStart = 0
        let mutable location = Location.createSpan input 0
        let reverseStateId = this.GetOrCreateState(reverseNode).Id
        let state = RegexState(_cache.NumOfMinterms())
        let initState = this.GetTruestarredStateId(&location)
        failwith "todo2"
        // let matchPositions = ResizeArray()
        // while looping do
        //     location.Position <- currMatchStart
        //
        //     _cache.TryNextStartsetLocation(&location, _startState.Startset)
        //
        //     match this.DfaEndPosition(&location,initState.Id) with
        //     | -2 -> looping <- false
        //     | endPos ->
        //         location.Position <- endPos
        //         location.Reversed <- true
        //         let startPos = this.DfaStartPosition(&location,reverseStateId)
        //
        //         location.Reversed <- false
        //         match startPos with
        //         | -2 ->
        //             failwith
        //                 $"match succeeded left to right but not right to left\nthis may occur because of an unimplemented feature\nend-pos:{endPos}, pattern:{reverseNode}"
        //         | start ->
        //             let startIdx = max currMatchStart start
        //             let response: MatchPosition = {
        //                 Index = startIdx
        //                 Length = endPos - startIdx
        //             }
        //             matchPositions.Add response
        //         // continue
        //         if endPos < input.Length then
        //             _toplevelOr <- _cache.False
        //             if endPos = currMatchStart then
        //                 currMatchStart <- currMatchStart + 1
        //             else
        //                 currMatchStart <- endPos
        //         else
        //             looping <- false
        // matchPositions

    // accessors
    member this.InitialPattern = trueStarredNode

    member this.RawPattern = initialNode

    member this.ReversePattern = reverseNode

    member this.Cache = _cache

#if DEBUG

    /// formal match end without exit condition
    member this.llmatchend(
        cache: RegexCache<TSet>,
        rstate: CountingSet.RegexState, // unused: state for counting sets
        loc: byref<Location>,
        activeNode: inref<RegexNode<TSet>>
        )  =
        let mutable looping = true
        let mutable currentStateId =
            this.GetOrCreateState(activeNode).Id // initialize state in dfa
        let mutable currentMax = -2

        while looping do

            let state = _stateArray[currentStateId]
            let flags = state.Flags

            // no exit condition in formal llmatch
            // 1. the pattern goes back to initial
            // 2. there is a nullable position
            // if flags.IsInitial && currentMax > 0 then
            //     looping <- false

            // skipping not in formal match
            // if flags.CanSkip then
            //     _cache.TryNextStartsetLocation(&loc, state.Startset)

            // determine nullability
            if RegexNode.isNullable(_cache, rstate, &loc,state.Node) then
                currentMax <- loc.Position

            if not (Location.isFinal loc) then
                // transition dfa state to next
                // if not in already in dfa then a new derivative is computed here
                let _ =  this.TryTakeTransition(rstate, &currentStateId, _cache.MintermId(&loc), &loc)
                loc.Position <- Location.nextPosition loc
            else
                looping <- false

        currentMax

    /// formal llmatch
    member this.llmatch(startLoc, input:ReadOnlySpan<char>) =
        let state = CountingSet.RegexState(_cache.NumOfMinterms()) // unused for now
        // ⊤*(R_rev)
        let trueStarredReversePat =
            _cache.Builder.mkConcat2( _cache.TrueStar, reverseNode )
        // start reversing from the end of the string
        let mutable loc =
            { Input = input ; Position = input.Length ; Reversed = true }
        let leftmostStart = this.llmatchend(_cache, state, &loc, &trueStarredReversePat)

        // start looking for the longest match from start
        loc.Position <- leftmostStart
        loc.Reversed <- false

        let longestEnd = this.llmatchend(_cache, state, &loc, &initialNode)

        let leftmostLongest = input.Slice(
            start = leftmostStart,
            length = longestEnd - leftmostStart)

        {|
          startPos = leftmostStart
          endPos = longestEnd
          text = leftmostLongest.ToString()
          |}

#endif



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
    // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    // override this.FindMatchEnd(input) = matcher.FindMatchEnd(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.IsMatch(input) = matcher.IsMatch(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.MatchPositions(input) = matcher.MatchPositions(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.MatchText(input) = matcher.MatchText(input)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Matches(input) =
        matcher.Matches(input)
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





