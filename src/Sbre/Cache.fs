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

// #nowarn "25" // missing patterns inference

[<Sealed>]
type RegexCache< ^t when ^t: struct and ^t :> IEquatable< ^t > and ^t: equality>
    (
        _solver: ISolver<uint64>,
        _charsetSolver: CharSetSolver,
        _implicitDotstarPattern: RegexNode<uint64>,
        _rawPattern: RegexNode<uint64>,
        _reversePattern: RegexNode<uint64>,
        _builder: RegexBuilder<uint64>,
        _optimizations: RegexFindOptimizations
    ) =
    let typedSolver = (_solver :?> UInt64Solver)
    let classifier = typedSolver._classifier

    let _ascii = classifier.Ascii
    let _nonAscii = classifier.NonAscii
    let minterms: _[] = _solver.GetMinterms()

    let mintermBdds =
        lazy (minterms |> Array.map (fun v -> _solver.ConvertToBDD(v, _charsetSolver)))

    let predStartsets = lazy StartsetHelpers.startsetsFromMintermArray mintermBdds.Value
    let mutable _cachedStartsets: Dictionary<uint64, char[]> = Dictionary()
    let mutable _toplevelOr: ToplevelORCollection = ToplevelORCollection()
    let mutable _reverseToplevelOr: ToplevelORCollection = ToplevelORCollection()
    let mutable _startsetPredicate = Startset.inferStartset _solver _rawPattern
    let mutable _initialStartset = Startset.inferInitialStartset _solver _rawPattern

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Minterms() = minterms

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetInitialStartsetPrefix() = _initialStartset

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetInitialStartsetPredicate() = _startsetPredicate

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetTopLevelOr() =
        _toplevelOr.Reset()
        _toplevelOr

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetReverseTopLevelOr() =
        _toplevelOr.Reset()
        _toplevelOr

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermBdds() = mintermBdds.Value

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
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
        let isInverted = Solver.elemOfSet set minterms[0]
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
                ValueSome(sharedIndex + 1)

    /// skip till a prefix of minterms matches
    member this.TryNextStartsetLocationArray(loc: inref<Location>, prefix: _[]) =
        assert not loc.Reversed

        let inputSpan = loc.Input.AsSpan()
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let setSpan = prefix.AsSpan()

        /// vectorize the search for the first character
        let firstSetChars = this.MintermIndexOfSpan(setSpan[0])
        let isInverted = Solver.elemOfSet setSpan[0] minterms[0]
        let tailPrefixSpan = setSpan.Slice(1)

        let _limitLength = inputSpan.Length - setSpan.Length
        let _tailLength = tailPrefixSpan.Length

        while skipping do
            let sharedIndex =
                let slice = inputSpan.Slice(currpos)
                if not isInverted then
                    slice.IndexOfAny(firstSetChars)
                else
                    slice.IndexOfAnyExcept(firstSetChars)

            if sharedIndex = -1 then
                skipping <- false
            else
                let potential = currpos + sharedIndex
                let mutable couldBe = true
                // exit if too far
                if potential > _limitLength then
                    skipping <- false
                    result <- ValueSome(potential)
                    couldBe <- false

                let mutable i = 0
                while couldBe && i < _tailLength do
                    let inputMinterm = this.Classify(inputSpan[potential + 1 + i])
                    if Solver.notElemOfSetU64 inputMinterm tailPrefixSpan[i] then
                        couldBe <- false
                    i <- i + 1

                if couldBe then
                    skipping <- false
                    result <- ValueSome(potential)
                else
                    currpos <- potential + 1



        result

    member this.TryNextStartsetLocationArrayReversed(loc: inref<Location>, prefix: _[]) =
        assert loc.Reversed

        let inputSpan = loc.Input.AsSpan()
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let mutable slice: ReadOnlySpan<char> = inputSpan.Slice(0, currpos)
        let setSpan = prefix.AsSpan()

        /// vectorize the search for the first character
        let firstSetChars = this.MintermIndexOfSpan(setSpan[0])
        let isInverted = Solver.elemOfSet setSpan[0] minterms[0]
        let tailPrefixSpan = setSpan.Slice(1)
        let _tailPrefixLength = tailPrefixSpan.Length

        if tailPrefixSpan.Length = 1 then
            skipping <- false
            let sharedIndex =
                slice <- inputSpan.Slice(0, currpos)
                if not isInverted then
                    slice.LastIndexOfAny(firstSetChars)

                else
                    slice.LastIndexOfAnyExcept(firstSetChars)

            if not (sharedIndex = -1) then
                let potential = sharedIndex + 1
                result <- ValueSome(potential)

        while skipping do
            let sharedIndex =
                slice <- inputSpan.Slice(0, currpos)
                if not isInverted then
                    slice.LastIndexOfAny(firstSetChars)
                else
                    slice.LastIndexOfAnyExcept(firstSetChars)

            if sharedIndex = -1 then
                skipping <- false
            else
                let potential = sharedIndex + 1

                let mutable couldBe = true

                // exit if too far
                if potential < prefix.Length then
                    skipping <- false
                    result <- ValueSome(potential)
                    couldBe <- false

                let mutable i = 0
                while couldBe && i < tailPrefixSpan.Length do
                    let inputMinterm = this.Classify(inputSpan[potential - i - 2])

                    if Solver.notElemOfSet inputMinterm tailPrefixSpan[i] then
                        couldBe <- false
                    i <- i + 1

                if couldBe then
                    skipping <- false
                    result <- ValueSome(potential)
                else
                    currpos <- potential - 1

        result

    /// skip till a prefix of minterms matches
    member this.TryNextStartsetLocationArrayWithLoopTerminator(loc: Location, prefix: _[], termPrefix: _[]) =
        assert (termPrefix.Length > 0)
        let inputSpan = loc.Input.AsSpan()
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let mutable sharedIndex = -1
        let mutable setSpan = prefix.AsSpan()
        let mutable termSpan = termPrefix.AsSpan()

        let mergedPrefix =
            setSpan[0] ||| termSpan[0]

        /// vectorize the search for the first minterm
        let firstSetChars = this.MintermIndexOfSpan(mergedPrefix)

        /// '.' to ^\n -> it's easier to invert large sets
        let isInverted = Solver.elemOfSet mergedPrefix minterms[0]

        while skipping do
            if loc.Reversed then
                let slice = inputSpan.Slice(0, currpos)
                if not isInverted then
                    sharedIndex <- slice.LastIndexOfAny(firstSetChars)
                else
                    sharedIndex <- slice.LastIndexOfAnyExcept(firstSetChars)
            else
                let slice = inputSpan.Slice(currpos)
                if not isInverted then
                    sharedIndex <- slice.IndexOfAny(firstSetChars)
                else
                    sharedIndex <- slice.IndexOfAnyExcept(firstSetChars)


            if sharedIndex = -1 then
                skipping <- false
                result <- ValueNone
            else
                let potential =
                    if loc.Reversed then
                        sharedIndex + 1
                    else
                        currpos + sharedIndex

                let shouldExit =
                    match loc.Reversed with
                    | true -> potential < prefix.Length
                    | _ -> potential + prefix.Length > (loc.Input.Length - 1)

                if shouldExit then
                    skipping <- false
                    result <- ValueSome(potential)
                else

                let nextLocMinterm =
                    if loc.Reversed then
                        this.Classify(inputSpan[potential-1])
                    else
                        this.Classify(inputSpan[potential])

                let mutable canTerminateLoop , looping =
                    if termSpan.Length = 0 then
                        -1, true
                    else
                        let termMatch =
                            (Solver.elemOfSet termSpan[0] nextLocMinterm)
                        (if termMatch then 0 else -1), termSpan.Length <> 1

                let mutable i = 1
                let mutable stopSearch = true

                while looping && i < setSpan.Length do
                    let nextLocMinterm =
                        if loc.Reversed then
                            this.Classify(inputSpan[potential - i - 1])
                        else
                            this.Classify(inputSpan[potential + i])

                    match Solver.elemOfSet setSpan[i] nextLocMinterm with
                    | false ->
                        stopSearch <- false
                        looping <- false
                        if loc.Reversed then currpos <- potential - 1
                        else currpos <- potential + 1
                    | true ->
                        i <- i + 1

                    if canTerminateLoop >= 0 && i < termSpan.Length
                       then
                           match Solver.elemOfSet termSpan[i] nextLocMinterm with
                           | true ->
                               canTerminateLoop <- i
                               if i = termSpan.Length - 1 then
                                  looping <- false
                                  stopSearch <- true
                           | false -> canTerminateLoop <- -1

                if stopSearch then
                    skipping <- false
                    result <- ValueSome(potential )
        result


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForLocation(loc: Location) : _ =
        let mutable pos = loc.Position
        if loc.Reversed then
            pos <- pos - 1
        this.Classify(loc.Input[pos])

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Classify(c: char) : uint64 =
        minterms[
            let i = int c
            match i < 128 with
            | true -> _ascii[i]
            | false -> _nonAscii.Find(i)
        ]

    member val InitialPatternWithoutDotstar = _rawPattern
    member val Solver: UInt64Solver = typedSolver
    member val CharsetSolver: CharSetSolver = _charsetSolver
    member val Builder = _builder

    // cached instantiation members
    member val True: RegexNode< _ > = _builder.uniques._true
    member val False: RegexNode< _ > = _builder.uniques._false
    member val TrueStar: RegexNode< _ > = _builder.uniques._trueStar
    member val FullMinterm: _ = _solver.Full

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsOrigReversePattern(node: RegexNode< uint64 >) : bool =
        obj.ReferenceEquals(node, _reversePattern)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateInfo(flags, startset) : RegexNodeInfo<_> =
        _builder.CreateInfo(flags, startset)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsImplicitDotStarred(node: RegexNode<uint64>) : bool =
        obj.ReferenceEquals(node, _implicitDotstarPattern)

#if DEBUG
    member cache.PrettyPrintMinterm(xs: _) : string = cache.Solver.PrettyPrint(xs, _charsetSolver)
#endif
    member this.Optimizations = _optimizations
