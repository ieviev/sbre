namespace Sbre

open System
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open System.Text.RuntimeRegexCopy
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

type MatchingState(node:RegexNode<TSet>) =
    member val Id = -1 with get,set
    member val Node = node with get,set

    //


[<Sealed>]
type RegexMatcher<'t
        when 't : struct>
        // and 't :> IEquatable< 't >
        // and 't: equality
        // >
        (
        initialNode:RegexNode<uint64>,
        rawNode:RegexNode<uint64>,
        reverseNode:RegexNode<uint64>,
        _cache:RegexCache<uint64>) as m =
    inherit GenericRegexMatcher()

    let InitialDfaStateCapacity = 1024
    let _stateCache = Dictionary<RegexNode<TSet>,MatchingState>() // TODO: dictionary with char kind
    let _stateArray = Array.zeroCreate<MatchingState> InitialDfaStateCapacity
    let _stateFlagsArray = Array.zeroCreate<RegexNodeFlags> InitialDfaStateCapacity
    let _minterms = _cache.Minterms()
    let _mintermsLog = BitOperations.Log2((uint64)_minterms.Length) + 1
    let _dfaDelta: int[] =
        Array.init (1024 <<< _mintermsLog) (fun _ -> -1 )

    do m.GetOrCreateState(_cache.False) |> ignore // 0


    override this.IsMatch(input: string) =
        let mutable currPos = 0
        let mutable startLocation = Location.create input currPos
        let mutable _toplevelOr = _cache.False
        match RegexNode.matchEnd _cache &startLocation initialNode &_toplevelOr with
        | ValueNone -> false
        | ValueSome _ -> true

    override this.FindMatchEnd(input: string) =
        let mutable currPos = 0
        let mutable _toplevelOr = _cache.False
        let mutable startLocation = Location.create input currPos
        RegexNode.matchEnd _cache &startLocation initialNode &_toplevelOr


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

        match RegexNode.matchEnd _cache &location initialNode &_toplevelOr with
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

        let inputSpan = input.AsSpan()
        let initialPrefix = _cache.GetInitialStartsetPrefix()
            // match _cache.GetInitialStartsetPrefix() with
            // | InitialStartset.MintermArrayPrefix(arr, loopEnd) ->
            //     arr
            // | _ ->
            //     ([|_cache.Solver.Full|].AsMemory())

        while looping do
            // use prefix optimizations

            _cache.TryNextStartsetLocationArray(&location,initialPrefix.Span)
            match RegexNode.matchEnd _cache &location initialNode &_toplevelOr with
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
            _stateFlagsArray[state.Id] <- node.GetFlags()
            state


    member this.GetDeltaOffset (stateId:int, mintermId:int) =
        (stateId <<< _mintermsLog) ||| mintermId;
    member this.TryTakeTransition(stateId: int, mintermId: int) : MatchingState =
        let dfaOffset = this.GetDeltaOffset(stateId, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        match nextStateId with
        | n ->
            failwith "todo"

        // if (matcher.TryCreateNewTransition(matcher.GetState(state.DfaStateId), mintermId, dfaOffset, checkThreshold: true, out MatchingState<TSet>? nextState))
        //         {
        //             // We were able to create a new DFA transition to some state. Move to it and
        //             // return that we're still operating as a DFA and can keep going.
        //             state.DfaStateId = nextState.Id;
        //             return true;
        //         }
        //
        //         return false;

        // TSet minterm = GetMintermFromId(mintermId);
        //             uint nextCharKind = GetPositionKind(mintermId);
        //             targetState = GetOrCreateState(sourceState.Next(_builder, minterm, nextCharKind), nextCharKind);
        //             Volatile.Write(ref _dfaDelta[offset], targetState.Id);

        // f (checkThreshold && _stateCache.Count >= SymbolicRegexThresholds.NfaThreshold)
        //             {
        //                 nextState = null;
        //                 return false;
        //             }



    member this.FindEndPosition((cache: RegexCache<TSet>),
        (loc: byref<Location>),
        (initialNode: RegexNode<TSet>),
        (toplevelOr: byref<RegexNode<TSet>>)) =

        let mutable currentMax = ValueNone
        let mutable foundmatch = false

        // initial node
        let _initialWithoutDotstar =
            if _cache.IsImplicitDotStarred initialNode then
                _cache.InitialPatternWithoutDotstar
            else
                toplevelOr <- initialNode
                initialNode

        let _startsetPredicate = _cache.GetInitialStartsetPredicate
        let _builder = _cache.Builder
        let _initialInfo = _initialWithoutDotstar.TryGetInfo
        let mutable toplevelOrInfo = toplevelOr.TryGetInfo
        let mutable currStateId = 0

        while not foundmatch do
            let flags = _stateFlagsArray[currStateId]
            if Location.isFinal loc then
                foundmatch <- true
            else
                let mt_id = _cache.MintermId(loc)
                let locationPredicate = _cache.MintermById(mt_id)

                // check dead end and nullability
                // ---




                // try take transition
                // if (pos >= length || !TStateHandler.TryTakeTransition(this, ref state, positionId))


                foundmatch <- true
        let a = 1
        ()


    /// return just the positions of matches without allocating the result
    override this.MatchPositions(input: string) =

        let mutable looping = true
        let mutable _toplevelOr = _cache.False

        let initialPrefix = _cache.GetInitialStartsetPrefix()
            // match _cache.GetInitialStartsetPrefix() with
            // | InitialStartset.MintermArrayPrefix(arr, loopEnd) ->
            //     arr
            // | _ ->
            //     ([|_cache.Solver.Full|].AsMemory())

        let mutable currMatchStart = 0
        let mutable location = Location.create input 0
        let matchPositions = ResizeArray()
        let mutable inputSpan = input.AsSpan()
        while looping do
            location.Position <- currMatchStart
            _cache.TryNextStartsetLocationArray(&location,initialPrefix.Span)
            match RegexNode.matchEnd _cache &location initialNode &_toplevelOr with
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
    member this.ImplicitPattern = initialNode

    member this.RawPattern = rawNode

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







