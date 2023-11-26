namespace Sbre

open System
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
    abstract member IsMatch: input:string -> bool
    abstract member FindMatchEnd: input:string -> int voption
    abstract member Replace: input:string -> replacement:string -> string
    abstract member Matches: input:string -> MatchResult seq
    abstract member MatchPositions: input:string -> MatchPosition seq
    abstract member MatchText: input:string -> string option
    abstract member Match: input:string -> MatchResult
    abstract member Count: input:string -> int

[<Flags>]
type MatchingStateFlags =
    | None = 0uy
    | InitialFlag = 1uy
    | DeadendFlag = 2uy
    | AlwaysNullableFlag = 4uy
    | CanBeNullableFlag = 8uy
    | CanSkipFlag = 16uy
    | HasPrefixFlag = 32uy
    | ContainsLookaroundFlag = 64uy

[<AutoOpen>]
module MatchingStateFlagsExtensions =
    type MatchingStateFlags with
        member this.IsInitial = byte (this &&& MatchingStateFlags.InitialFlag) <> 0uy
        member this.IsDeadend = byte (this &&& MatchingStateFlags.DeadendFlag) <> 0uy
        member this.IsAlwaysNullable = byte (this &&& MatchingStateFlags.AlwaysNullableFlag) <> 0uy
        member this.CanBeNullable = byte (this &&& MatchingStateFlags.CanBeNullableFlag) <> 0uy
        member this.ContainsLookaround = byte (this &&& MatchingStateFlags.ContainsLookaroundFlag) <> 0uy
        member this.CanSkip = byte (this &&& MatchingStateFlags.CanSkipFlag) <> 0uy
        member this.HasPrefix = byte (this &&& MatchingStateFlags.HasPrefixFlag) <> 0uy


type MatchingState(node:RegexNode<TSet>) =
    member val Id = -1 with get,set
    member val Node = node with get,set

    member this.BuildFlags(c:RegexCache<TSet>) =
        let mutable flags = MatchingStateFlags.None
        if obj.ReferenceEquals(c.False, node) then
            flags <- flags ||| MatchingStateFlags.InitialFlag
        let nodeFlags = node.GetFlags()
        if nodeFlags.IsAlwaysNullable then
            flags <- flags ||| MatchingStateFlags.AlwaysNullableFlag
        if nodeFlags.CanBeNullable then
            flags <- flags ||| MatchingStateFlags.CanBeNullableFlag

        if nodeFlags.ContainsLookaround then
            flags <- flags ||| MatchingStateFlags.ContainsLookaroundFlag
        // TODO: could be optimized
        if c.InitialPatternWithoutDotstar.ContainsLookaround then
            flags <- flags ||| MatchingStateFlags.ContainsLookaroundFlag

        if nodeFlags.CanSkip && not flags.ContainsLookaround  then
            flags <- flags ||| MatchingStateFlags.CanSkipFlag
        if nodeFlags.HasPrefix then
            match c.Builder.GetInitializedPrefix(node) with
            | MintermArrayPrefix(prefix, _) ->
                if prefix.Length > 1 then
                    flags <- flags ||| MatchingStateFlags.HasPrefixFlag
            | _ -> ()

        flags


[<Sealed>]
type RegexMatcher<'t
        when 't : struct>
        // and 't :> IEquatable< 't >
        // and 't: equality
        // >
        (
        dotStarredNode:RegexNode<uint64>,
        initialNode:RegexNode<uint64>,
        reverseNode:RegexNode<uint64>,
        _cache:RegexCache<uint64>) as m =
    inherit GenericRegexMatcher()

    let InitialDfaStateCapacity = 1024
    let _stateCache = Dictionary<RegexNode<TSet>,MatchingState>() // TODO: dictionary with char kind
    let _stateArray = Array.zeroCreate<MatchingState> InitialDfaStateCapacity
    let _stateFlagsArray = Array.zeroCreate<MatchingStateFlags> InitialDfaStateCapacity
    let _minterms = _cache.Minterms()
    let _mintermsLog = BitOperations.Log2((uint64)_minterms.Length) + 1
    let _startsetPredicate = _cache.GetInitialStartsetPredicate
    let _initialInfo = initialNode.TryGetInfo
    let _dfaDelta: int[] =
        Array.init (1024 <<< _mintermsLog) (fun _ -> 0 ) // 0 : initial state

    do
        m.GetOrCreateState(_cache.False) |> ignore // 0



    override this.IsMatch(input: string) =
        let mutable currPos = 0
        let mutable startLocation = Location.create input currPos
        let mutable _toplevelOr = _cache.False
        match RegexNode.matchEnd _cache &startLocation dotStarredNode &_toplevelOr with
        | ValueNone -> false
        | ValueSome _ -> true

    override this.FindMatchEnd(input: string) =
        let mutable currPos = 0
        let mutable _toplevelOr = _cache.False
        let mutable startLocation = Location.create input currPos
        RegexNode.matchEnd _cache &startLocation dotStarredNode &_toplevelOr


    override this.Match(input: string) : MatchResult =
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
                Value = input[result.Index .. result.Index + result.Length - 1]
                Index = result.Index
                Length = result.Length
            }

    /// replace all occurrences in string
    override this.Replace(input: string) (replacement: string) =
        let sb = System.Text.StringBuilder(input)
        let mutable offset = 0
        for result in this.MatchPositions(input) do
            let start = offset + result.Index
            sb.Remove(start, result.Length + 1).Insert(start, replacement) |> ignore
            offset <-  replacement.Length - result.Length - 1
        sb.ToString()

    /// return all matches on input
    override this.Matches(input: string) =
        this.MatchPositions(input)
        |> Seq.map (fun result ->
            {
                Success = true
                Value = input[result.Index .. result.Index + result.Length - 1]
                Index = result.Index
                Length = result.Length
            }
        )

    /// used internally
    override this.MatchText(input: string) =
        let mutable startPos = 0
        let mutable location = Location.create input startPos
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
            | ValueSome start -> Some(input[start .. endPos - 1])


    /// counts the number of matches
    override this.Count(input: string) =
        let mutable currPos = 0
        let mutable location = Location.create input 0
        let mutable looping = true
        let mutable counter = 0
        let _cache : RegexCache<_>  = this.Cache
        let mutable _toplevelOr = _cache.False
        let chars = _cache.GetInitialSearchValues()
        let inputSpan = input.AsSpan()
        let initialPrefix = _cache.GetInitialStartsetPrefix()
            // match _cache.GetInitialStartsetPrefix() with
            // | InitialStartset.MintermArrayPrefix(arr, loopEnd) ->
            //     arr
            // | _ ->
            //     ([|_cache.Solver.Full|].AsMemory())

        while looping do
            // use prefix optimizations

            _cache.TryNextStartsetLocationArray(&location,initialPrefix.Span,chars)
            match RegexNode.matchEnd _cache &location dotStarredNode &_toplevelOr with
            | ValueNone -> looping <- false
            | ValueSome(endPos: int) ->
                counter <- counter + 1
                if endPos < inputSpan.Length then
                    _toplevelOr <- _cache.False
                    if endPos = location.Position then
                        location.Position <- location.Position + 1
                    else
                        location.Position <- endPos
                else
                    looping <- false

        counter


    member this.GetOrCreateState(node: RegexNode<TSet>) : MatchingState =
        match _stateCache.TryGetValue(node) with
        | true , v -> v
        | _ ->
            let state = MatchingState(node)
            _stateCache.Add(node,state)
            state.Id <- _stateCache.Count
            // TODO: grow state space if needed
            if _stateArray.Length = state.Id then
                failwith "TODO: resize DFA"
            _stateArray[state.Id] <- state
            _stateFlagsArray[state.Id] <- state.BuildFlags(_cache)
            state.Node.TryGetInfo
            |> ValueOption.iter (fun info ->
                if obj.ReferenceEquals(null,info.SkipToChars) then
                    info.SkipToChars <- _cache.MintermStartsetChars(info.Startset)
            )

            state


    member this.GetDeltaOffset (stateId:int, mintermId:int) =
        (stateId <<< _mintermsLog) ||| mintermId


    member this.TryCreateNextDerivative(
        locPred: TSet, loc:inref<Location>, activeBranch:inref<MatchingState>) : RegexNode<TSet> =

        let flags : MatchingStateFlags = _stateFlagsArray[activeBranch.Id]

        let initialDerivative =
            if
                Solver.elemOfSet _startsetPredicate locPred
                && (not flags.CanBeNullable)
            then
                let deriv =
                    match RegexNode.getCachedTransition (locPred, _initialInfo) with
                    | ValueSome v -> v
                    | _ -> createDerivative ( _cache, &loc, locPred, initialNode )
                deriv
            else _cache.False

        let activeDerivative =
            match flags with
            | MatchingStateFlags.InitialFlag -> activeBranch.Node
            | _ ->
                match RegexNode.getCachedTransition (locPred, activeBranch.Node.TryGetInfo) with
                | ValueSome v -> v
                | _ -> createDerivative (_cache, &loc, locPred, activeBranch.Node)

        let newActiveNode =
            if refEq initialNode activeDerivative then _cache.False else
            if refEq _cache.False initialDerivative then activeDerivative else
            if refEq _cache.False activeDerivative then initialDerivative else
                let isSubsumed =
                    match _cache.Builder.SubsumptionCache.TryGetValue(struct (activeDerivative, initialDerivative)) with
                    | true, subsumed -> subsumed
                    | _ -> _cache.Builder.trySubsumeTopLevelOr (activeDerivative, initialDerivative)
                if isSubsumed then activeDerivative else
                    _cache.Builder.mkOr [| activeDerivative; initialDerivative |]

        newActiveNode

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
            let minterm = _cache.MintermById(mintermId)

            let sourceState = _stateArray[currentState]

            let targetDerivative =
                this.TryCreateNextDerivative(minterm,&loc,&sourceState)
            let targetState = this.GetOrCreateState(targetDerivative)
            _dfaDelta[dfaOffset] <- targetState.Id
            currentState <- targetState.Id
        true


    member this.SkipToNextPosition(
        cache: RegexCache<TSet>,
        stateNode:RegexNode<TSet>,
        loc:byref<Location>,
        _initialSearchValues,
        _initialPrefix,
        flags:MatchingStateFlags) =

        match flags with
        | MatchingStateFlags.InitialFlag ->
            if not flags.HasPrefix then
                cache.SkipIndexOfAny(&loc,_initialSearchValues)
            else
                cache.TryNextStartsetLocationArray(&loc,_initialPrefix,_initialSearchValues)
        | MatchingStateFlags.CanSkipFlag ->
            match stateNode.TryGetInfo with
            | ValueSome i ->
                // don't skip if valid position
                if not (i.SkipToChars.Contains(loc.Input[loc.Position])) then
                    // todo: tradeoffs between these two
                    // if not flags.HasPrefix then
                    //     cache.SkipIndexOfAny(&loc,i.SkipToChars)
                    // else
                    Optimizations.tryJumpToStartset2 cache &loc stateNode
            | _ -> ()
        | _ -> ()


    /// end position with DFA
    member this.DfaEndPosition((cache: RegexCache<TSet>),
        (loc: byref<Location>),
        (initialNode: RegexNode<TSet>),
        (toplevelOr: inref<RegexNode<TSet>>)) =

        let mutable foundmatch = false
        let mutable currentStateId = 1
        let mutable currentMax = -2
        let _initialSearchValues = cache.GetInitialSearchValues()
        let _initialPrefix = cache.GetInitialStartsetPrefix().Span

        while not foundmatch do
            // let dbg = _stateArray[currentStateId].Node.ToString()
            let flags = _stateFlagsArray[currentStateId]
            let state = _stateArray[currentStateId]

            if flags.IsInitial  then
                if currentMax > 0  then
                    foundmatch <- true
                else
                    cache.TryNextStartsetLocationArray(&loc,_initialPrefix,_initialSearchValues)
             elif flags.CanSkip then

                match (state.Node).TryGetInfo with
                | ValueSome i ->
                    // todo : tradeoffs between these
                    if not flags.HasPrefix then
                        cache.SkipIndexOfAny(&loc,i.SkipToChars)
                    else
                        // has prefix of at least 2 chars
                        Optimizations.tryJumpToStartset2 cache &loc (state.Node)
                | _ -> ()


            if flags.CanBeNullable then
                // set nullability after skipping
                if flags.IsAlwaysNullable || RegexNode.isNullable(_cache,&loc,(state.Node)) then
                    currentMax <- loc.Position


            if Location.isFinal loc then
                foundmatch <- true
            else
                let mt_id = _cache.MintermId(&loc)
                // check dead end and nullability
                // ---
                let _ =  this.TryTakeTransition(&currentStateId, mt_id, &loc)

                // try take transition
                // if (pos >= length || !TStateHandler.TryTakeTransition(this, ref state, positionId))
                loc.Position <- Location.nextPosition loc

        if foundmatch then
            match currentMax with
            | n when n <> loc.Position ->
                if RegexNode.isNullable (cache, &loc, _stateArray[currentStateId].Node) then
                    currentMax <- loc.Position
            | _ ->
                if initialNode.IsAlwaysNullable then
                    currentMax <- loc.Position
        currentMax

    member this.DfaMatchEnds(input:ReadOnlySpan<char>) =
        let mutable loc = Location.createSpan input 0
        let mutable toplevelOr = _cache.False
        let eps = ResizeArray()
        let mutable ep = 0
        while (ep <- this.DfaEndPosition(_cache, &loc, initialNode, &toplevelOr); ep > -1) do
            eps.Add ep
            loc.Position <- ep + 1
        eps

    member this.DfaCount(input:ReadOnlySpan<char>) =
        let mutable loc = Location.createSpan input 0
        let mutable toplevelOr = _cache.False
        let mutable counter = 0
        let mutable ep = 0
        while (ep <- this.DfaEndPosition(_cache, &loc, initialNode, &toplevelOr); ep > -1) do
            counter <- counter + 1
            loc.Position <- ep + 1
        counter


    /// return just the positions of matches without allocating the result
    override this.MatchPositions(input: string) =

        let mutable looping = true
        let mutable _toplevelOr = _cache.False

        let initialPrefix = _cache.GetInitialStartsetPrefix()

        let mutable currMatchStart = 0
        let mutable location = Location.create input 0
        let matchPositions = ResizeArray()
        let chars = _cache.GetInitialSearchValues()
        while looping do
            location.Position <- currMatchStart
            _cache.TryNextStartsetLocationArray(&location,initialPrefix.Span,chars)
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
                        Length = (endPos) - startIdx
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
    member this.ImplicitPattern = dotStarredNode

    member this.RawPattern = initialNode

    member this.ReversePattern = reverseNode

    member this.Cache = _cache


module Helpers =
    let createMatcher
        (minterms: BDD array,charsetSolver,converter,trueStarPattern,symbolicBddnode, regexTree:RegexTree) : GenericRegexMatcher =


        match minterms.Length with
        // | n when n < 8 ->
        //     let solver = UInt8Solver(minterms, charsetSolver)
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
        //     RegexMatcher<byte>(trueStarredNode,rawNode,reverseNode,cache) :> GenericRegexMatcher
        // | n when n < 16 ->
        //     let solver = UInt16Solver(minterms, charsetSolver)
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
        //     RegexMatcher<uint16>(trueStarredNode,rawNode,reverseNode,cache) :> GenericRegexMatcher
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
        | _ -> failwith "sbre does not support bitvectors over 64"


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
    let charsetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()
    // builder from .net runtime
    let bddBuilder = SymbolicRegexBuilder<BDD>(charsetSolver, charsetSolver)
    let converter = RegexNodeConverter(bddBuilder, null)
    let bddBuilder2 = RegexBuilder(converter, charsetSolver, charsetSolver)

    let symbolicBddnode: RegexNode<BDD> =
        RegexNodeConverter.convertToSymbolicRegexNode (
            charsetSolver,
            bddBuilder,
            bddBuilder2,
            regexTree.Root
        )
    let implicitTrueStar = bddBuilder2.trueStar

    let trueStarPattern: RegexNode<BDD> =
        match symbolicBddnode with
        | Concat(head, tail, info) -> Concat(implicitTrueStar, Concat(head, tail, info), info)
        | Singleton pred as node ->
            Concat(implicitTrueStar, node, Info.defaultInfo() charsetSolver)
        | And(xs, info) as node -> Concat(implicitTrueStar, node, info)
        | Or(xs, info) as node -> Concat(implicitTrueStar, node, info)
        | Loop(xs, low, up, info) as node -> Concat(implicitTrueStar, node, info)
        | LookAround(xs, low, up) as node ->
            Concat(implicitTrueStar, node, Info.defaultInfo() charsetSolver)
        | Epsilon -> implicitTrueStar
        | Not(xs, info) as node ->
            let negflags = Info.Flags.inferNode xs
            Concat(implicitTrueStar, node, bddBuilder2.CreateInfo(negflags, charsetSolver.Full))
    let minterms = trueStarPattern |> Minterms.compute bddBuilder
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
    override this.Replace(input) (replacement) = matcher.Replace (input) replacement
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Match(input) = matcher.Match(input)

    member this.Matcher : GenericRegexMatcher = matcher
    member this.UInt64Matcher : RegexMatcher<uint64> = matcher :?> RegexMatcher<uint64>
    member this.UInt16Matcher : RegexMatcher<uint16> = matcher :?> RegexMatcher<uint16>
    member this.ByteMatcher : RegexMatcher<byte> =
        matcher :?> RegexMatcher<byte>







