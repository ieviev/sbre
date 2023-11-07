namespace Sbre

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Microsoft.FSharp.Core
open Sbre.Types
open Sbre.Pat
open Info

#nowarn "25" // missing patterns inference

type impl = MethodImplAttribute
type implOpts = MethodImplOptions


[<Sealed>]
// type RegexCache< ^t when ^t: struct and ^t :> IEquatable< ^t > and ^t: equality>
type RegexCache<'gh>
    (
        _solver: ISolver<uint64>,
        _charsetSolver: CharSetSolver,
        _implicitDotstarPattern: RegexNode<uint64>,
        _rawPattern: RegexNode<uint64>,
        _builder: RegexBuilder<uint64>,
        _optimizations: RegexFindOptimizations
    ) =
    let typedSolver = (_solver :?> UInt64Solver)
    let classifier = typedSolver._classifier


    let minterms: _[] = _solver.GetMinterms()

    let mintermBdds =
        lazy (minterms |> Array.map (fun v -> _solver.ConvertToBDD(v, _charsetSolver)))

    let predStartsets = lazy StartsetHelpers.startsetsFromMintermArray mintermBdds.Value

    let initialSs2 = Startset.inferStartset2 _solver _rawPattern

    let mutable _cachedStartsets: Dictionary<uint64, char[]> = Dictionary()
    let mutable _toplevelOr: ToplevelORCollection = new ToplevelORCollection()
    let mutable _startsetPredicate = Startset.inferStartset _solver _rawPattern

#if DEBUG
    let mintermsPretty =
        _solver.GetMinterms()
        |> Array.map (fun mt -> mt, _solver.PrettyPrint(mt, _charsetSolver))

    // member this.MintermsPretty = mintermsPretty
#endif

    member this.InitialSs2() = initialSs2
    member this.Minterms() = minterms
    member this.GetInitialStartsetPredicate() = _startsetPredicate

    member this.GetTopLevelOr() =
        _toplevelOr.Reset()
        _toplevelOr

    member this.MintermBdds() = mintermBdds.Value
    member this.MintermStartsets() = predStartsets.Value

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermIndexOfSpan(startset: uint64) =
        match _cachedStartsets.TryGetValue(startset) with
        | true, v -> v.AsSpan()
        | _ ->
            let newSpan =
                StartsetHelpers.getMergedIndexOfSpan (
                    predStartsets.Value,
                    minterms,
                    startset
                )

            _cachedStartsets.Add(startset, newSpan.ToArray())
            newSpan

    member this.TryNextStartsetLocation(loc: Location, set: uint64) =

        let setChars = this.MintermIndexOfSpan(set)
        let isInverted = this.IsValidPredicate(set, minterms[0])
        let currpos = loc.Position

        match loc.Reversed with
        | false ->
            let slice = loc.Input.AsSpan().Slice(currpos)

            let sharedIndex =
                if isInverted then
                    slice.IndexOfAnyExcept(setChars)
                else
                    slice.IndexOfAny(setChars)

            if sharedIndex = -1 then
                ValueNone
            else
                ValueSome(currpos + sharedIndex)
        | true ->
            let slice = loc.Input.AsSpan().Slice(0, currpos)

            let sharedIndex =
                if isInverted then
                    slice.LastIndexOfAnyExcept(setChars)
                else
                    slice.LastIndexOfAny(setChars)

            if sharedIndex = -1 then
                ValueNone
            else
                // let _ = Location.create loc.Input (sharedIndex + 1)
                ValueSome(sharedIndex + 1)

    member this.TryNextStartsetLocation2(loc: Location, set: _, set2: _) =

        let setChars = this.MintermIndexOfSpan(set)
        let isInverted = this.IsValidPredicate(set, minterms[0])
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let inputSpan = loc.Input.AsSpan()
        let mutable slice = inputSpan.Slice(currpos)
        let mutable sharedIndex = 0

        // let inline nextLocMinterm(pos: int) =
        //     if loc.Reversed then
        //         this.MintermForStringIndex(loc.Input, pos - 2)
        //     else
        //         this.MintermForStringIndex(loc.Input, pos + 1)

        match loc.Reversed, isInverted with
        | false, false ->
            while skipping do
                slice <- inputSpan.Slice(currpos)
                sharedIndex <- slice.IndexOfAny(setChars)

                if sharedIndex = -1 then
                    skipping <- false
                else
                    let potential = currpos + sharedIndex

                    if Location.posIsPreFinal (potential, loc) then
                        skipping <- false
                        result <- ValueSome(potential)
                    else
                        let nextLocMinterm =
                            this.MintermOfChar(inputSpan[potential + 1])

                        match Solver.isElemOfSetU64 (set2) (nextLocMinterm) with
                        | false -> currpos <- potential + 1
                        | true ->
                            skipping <- false
                            result <- ValueSome(potential)

            result
        | false, true ->
            while skipping do
                slice <- loc.Input.AsSpan().Slice(currpos)
                sharedIndex <- slice.IndexOfAnyExcept(setChars)

                if sharedIndex = -1 then
                    skipping <- false
                else
                    let potential = currpos + sharedIndex

                    if Location.posIsPreFinal (potential, loc) then
                        skipping <- false
                        result <- ValueSome(potential)
                    else

                    let nextLocMinterm =
                        this.MintermOfChar(inputSpan[potential + 1])

                    match Solver.isElemOfSetU64 set2 nextLocMinterm with
                    | false -> currpos <- potential + 1
                    | true ->
                        skipping <- false
                        result <- ValueSome(potential)

            result
        | true, _ ->
            while skipping do
                // let slice = loc.Input.AsSpan().Slice(0, currpos)
                let slice = loc.Input.AsSpan().Slice(0, currpos)

                let sharedIndex =
                    if not isInverted then
                        slice.LastIndexOfAny(setChars)
                    else
                        slice.LastIndexOfAnyExcept(setChars)


                if sharedIndex = -1 then
                    skipping <- false
                    result <- ValueNone
                else
                    let potential = sharedIndex + 1

                    if Location.posIsPreFinal (potential, loc) then
                        skipping <- false
                        result <- ValueSome(potential)
                    else

                    let nextLocMinterm =
                        if loc.Reversed then
                            this.MintermForStringIndex(loc.Input, potential - 1)
                        else
                            this.MintermForStringIndex(loc.Input, potential + 2)

                    match this.IsValidPredicate(set2, nextLocMinterm) with
                    | false -> currpos <- potential - 1
                    | true ->
                        skipping <- false
                        result <- ValueSome(potential)

            result


    member this.TryNextStartsetLocation2Alternate(loc: Location, set: uint64, set2: uint64) =

        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let inputSpan = loc.Input.AsSpan(currpos)

        let mutable e = inputSpan.GetEnumerator()
        let mutable counter = 0u

        while e.MoveNext() && skipping do
            counter <- counter + 1u
            let loc_pred = minterms[classifier.GetMintermID2(uint32 e.Current)]

            if not (Solver.isElemOfSetU64 (loc_pred) set) || not (e.MoveNext()) then
                ()
            else

                counter <- counter + 1u

                let loc_pred_2 = minterms[classifier.GetMintermID2(uint32 e.Current)]

                if not (Solver.isElemOfSetU64 (loc_pred_2) set2) then
                    ()
                else

                    skipping <- false
                    result <- ValueSome(loc.Position + int counter)

        result



    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForLocation(loc: Location) : _ =
        match loc.Reversed with
        | false -> minterms[classifier.GetMintermID(int loc.Input[loc.Position])]
        | true ->
            // if loc.Position = loc.Input.Length then 0uL
            minterms[classifier.GetMintermID(int loc.Input[loc.Position - 1])]


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForStringIndex(str: string, pos: int) =
        minterms[classifier.GetMintermID(int str[pos])]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermOfChar(c: char) : _ = minterms[classifier.GetMintermID(int c)]

    member val InitialPatternWithoutDotstar = _rawPattern

    member val Solver: UInt64Solver = typedSolver
    member val CharsetSolver: CharSetSolver = _charsetSolver
    member val Builder = _builder

    // cached instantiation members
    member val True: RegexNode< _ > = _builder.uniques._true
    //
    //
    member val False: RegexNode< _ > = _builder.uniques._false

    //
    member val TrueStar: RegexNode< _ > = _builder.uniques._trueStar


    member val FullMinterm: _ = _solver.Full


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsTrueStar(node: RegexNode< uint64 >) : bool =
        obj.ReferenceEquals(node, _builder.uniques._trueStar)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsFalse(node: RegexNode< uint64 >) : bool =
        obj.ReferenceEquals(node, _builder.uniques._false)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateInfo(flags, startset) : RegexNodeInfo<_> =
        _builder.CreateInfo(flags, startset)



    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsTrue(node: RegexNode< uint64 >) : bool =
        obj.ReferenceEquals(node, _builder.uniques._true)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsImplicitDotStarred(node: RegexNode<uint64>) : bool =
        obj.ReferenceEquals(node, _implicitDotstarPattern)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.GetPredicateResult(location: Location, pred: _) =
        let mterm = cache.MintermForLocation(location)
        cache.Solver.isElemOfSet (pred, mterm)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.IsValidPredicate(pred: _, locationPredicate: _) : bool =
        // (Solver.isElemOfSetU64 pred locationPredicate)
        cache.Solver.isElemOfSet (pred, locationPredicate)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.IsValidPredicateUint64(pred: uint64, locationPredicate: uint64) : bool =
        (pred &&& locationPredicate) > 0uL


#if DEBUG
    member cache.PrintNode(xs: RegexNode<_>) =

        if obj.ReferenceEquals(xs, null) then
            "null"
        else
            xs.ToStringHelper()

    member cache.PrettyPrintMinterm(xs: _) : string = cache.Solver.PrettyPrint(xs, _charsetSolver)
#endif

    member this.Optimizations = _optimizations
