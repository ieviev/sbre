namespace Sbre

open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
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
type RegexCache< ^t when ^t: struct and ^t :> IEquatable< ^t > and ^t: equality>
// type RegexCache<'tset>
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
    // let classify (c:char) =
    //
    //     ()


    let _ascii = classifier.Ascii
    let _nonAscii = classifier.NonAscii


    let minterms: _[] = _solver.GetMinterms()

    let mintermBdds =
        lazy (minterms |> Array.map (fun v -> _solver.ConvertToBDD(v, _charsetSolver)))

    let predStartsets = lazy StartsetHelpers.startsetsFromMintermArray mintermBdds.Value

    let mutable _cachedStartsets: Dictionary<uint64, char[]> = Dictionary()
    let mutable _toplevelOr: ToplevelORCollection = new ToplevelORCollection()
    let mutable _reverseToplevelOr: ToplevelORCollection = new ToplevelORCollection()
    let mutable _startsetPredicate = Startset.inferStartset _solver _rawPattern
    let mutable _initialStartset = Startset.inferInitialStartset _solver _rawPattern

    let getCachedStartsetChars (startset: uint64): Span<char> =
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

    // let keywordPrefix =
    //     match _initialStartset with
    //     | InitialStartset.MintermArrayPrefix(prefix, loopTerminator) ->
    //         let mutable isKeyword = true
    //         // let initialVec = Vector<uint16>.One
    //         // let prefixSpan = Ptr.stackalloc<char> (prefix.Length)
    //         let prefixSpan = StringBuilder(prefix.Length)
    //         // prefixSpan.Fill(UInt16.MaxValue)
    //         let prefixChars =
    //             for i = 0 to prefix.Length - 1 do
    //                 let charSpan = getCachedStartsetChars prefix[i]
    //                 if charSpan.Length <> 1 then
    //                     isKeyword <- false
    //                 else
    //                     prefixSpan.Append(charSpan[0]) |> ignore
    //                     // prefixSpan[i] <- charSpan[0]
    //
    //         if isKeyword then
    //             let str = prefixSpan.ToString()
    //             // let str = System.String(chars)
    //             Some(str)
    //         else
    //             None
    //     | _ -> None



#if DEBUG
    let mintermsPretty =
        _solver.GetMinterms()
        |> Array.map (fun mt -> mt, _solver.PrettyPrint(mt, _charsetSolver))

    // member this.MintermsPretty = mintermsPretty
#endif


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

    // member this.TryNextStartsetLocation2(loc: Location, set: _, set2: _) =
    //
    //     let setChars = this.MintermIndexOfSpan(set)
    //     let isInverted = this.IsValidPredicate(set, minterms[0])
    //     let mutable currpos = loc.Position
    //     let mutable skipping = true
    //     let mutable result = ValueNone
    //     let inputSpan = loc.Input.AsSpan()
    //     let mutable slice = inputSpan.Slice(currpos)
    //     let mutable sharedIndex = 0
    //
    //     match loc.Reversed, isInverted with
    //     | false, false ->
    //         while skipping do
    //             slice <- inputSpan.Slice(currpos)
    //             sharedIndex <- slice.IndexOfAny(setChars)
    //
    //             if sharedIndex = -1 then
    //                 skipping <- false
    //             else
    //                 let potential = currpos + sharedIndex
    //
    //                 if Location.posIsPreFinal (potential, loc) then
    //                     skipping <- false
    //                     result <- ValueSome(potential)
    //                 else
    //                     let nextLocMinterm =
    //                         this.MintermOfChar(inputSpan[potential + 1])
    //
    //                     match Solver.isElemOfSetU64 (set2) (nextLocMinterm) with
    //                     | false -> currpos <- potential + 1
    //                     | true ->
    //                         skipping <- false
    //                         result <- ValueSome(potential)
    //
    //         result
    //     | false, true ->
    //         while skipping do
    //             slice <- loc.Input.AsSpan().Slice(currpos)
    //             sharedIndex <- slice.IndexOfAnyExcept(setChars)
    //
    //             if sharedIndex = -1 then
    //                 skipping <- false
    //             else
    //                 let potential = currpos + sharedIndex
    //
    //                 if Location.posIsPreFinal (potential, loc) then
    //                     skipping <- false
    //                     result <- ValueSome(potential)
    //                 else
    //
    //                 let nextLocMinterm =
    //                     this.MintermOfChar(inputSpan[potential + 1])
    //
    //                 match Solver.isElemOfSetU64 set2 nextLocMinterm with
    //                 | false -> currpos <- potential + 1
    //                 | true ->
    //                     skipping <- false
    //                     result <- ValueSome(potential)
    //
    //         result
    //     | true, _ ->
    //         while skipping do
    //             let slice = loc.Input.AsSpan().Slice(0, currpos)
    //
    //             let sharedIndex =
    //                 if not isInverted then
    //                     slice.LastIndexOfAny(setChars)
    //                 else
    //                     slice.LastIndexOfAnyExcept(setChars)
    //
    //
    //             if sharedIndex = -1 then
    //                 skipping <- false
    //                 result <- ValueNone
    //             else
    //                 let potential = sharedIndex + 1
    //
    //                 if Location.posIsPreFinal (potential, loc) then
    //                     skipping <- false
    //                     result <- ValueSome(potential)
    //                 else
    //
    //                 let nextLocMinterm =
    //                     if loc.Reversed then
    //                         this.MintermForStringIndex(loc.Input, potential - 1)
    //                     else
    //                         this.MintermForStringIndex(loc.Input, potential + 2)
    //
    //                 match this.IsValidPredicate(set2, nextLocMinterm) with
    //                 | false -> currpos <- potential - 1
    //                 | true ->
    //                     skipping <- false
    //                     result <- ValueSome(potential)
    //
    //         result


    /// skip till a prefix of minterms matches
    // [<MethodImpl(MethodImplOptions.AggressiveOptimization)>]
    member this.TryNextStartsetLocationArray(loc: inref<Location>, prefix: _[]) =
        assert not loc.Reversed

        let inputSpan = loc.Input.AsSpan()
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let mutable slice: ReadOnlySpan<char> = inputSpan.Slice(currpos)
        let setSpan = prefix.AsSpan()

        /// vectorize the search for the first character
        let firstSetChars = this.MintermIndexOfSpan(prefix[0])
        let isInverted = Solver.elemOfSet prefix[0] minterms[0]
        let tailPrefixSpan = setSpan.Slice(1)
        let _limitLength = inputSpan.Length + setSpan.Length - 1
        let _tailPrefixLength = tailPrefixSpan.Length

        if tailPrefixSpan.Length = 1 then
            skipping <- false
            let sharedIndex =
                slice <- inputSpan.Slice(currpos)
                if not isInverted then
                    slice.IndexOfAny(firstSetChars)
                else
                    slice.IndexOfAnyExcept(firstSetChars)

            if not (sharedIndex = -1) then
                let potential =
                    currpos + sharedIndex
                result <- ValueSome(potential)


        while skipping do
            let sharedIndex =
                slice <- inputSpan.Slice(currpos)
                if not isInverted then
                    slice.IndexOfAny(firstSetChars)
                else
                    slice.IndexOfAnyExcept(firstSetChars)

            if sharedIndex = -1 then
                skipping <- false
            else
                let potential = currpos + sharedIndex

                let mutable couldBe = true
                let mutable i = 0

                // exit if too far
                if potential > _limitLength then
                    skipping <- false
                    result <- ValueSome(potential)
                    couldBe <- false


#if DEBUG
                // let dbgSpan = loc.Input.AsSpan().Slice(potential)
                // let setPretty =
                //     setSpan.ToArray()
                //     |> Array.map this.PrettyPrintMinterm
                //     |> String.concat ""
#endif

                while couldBe && i < tailPrefixSpan.Length do
                    let inputMinterm = this.Classify(inputSpan[potential + 1 + i])
                    if Solver.notElemOfSet inputMinterm tailPrefixSpan[i] then
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
        let firstSetChars = this.MintermIndexOfSpan(prefix[0])
        let isInverted = Solver.elemOfSet prefix[0] minterms[0]
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

#if DEBUG
                // let dbgSpan = loc.Input.AsSpan().Slice(0, potential)
                // let setPretty =
                //     setSpan.ToArray()
                //     |> Array.map this.PrettyPrintMinterm
                //     |> String.concat ""
                // let vs =
                //     [|
                //       this.PrettyPrintMinterm (this.Classify(dbgSpan[potential - 1]))
                //     |]
#endif
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

        let inputSpan = loc.Input.AsSpan()
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let mutable slice = inputSpan
        let mutable sharedIndex = -1
        let mutable setSpan = prefix.AsSpan()

        let mergedPrefix =
            prefix[0] ||| if termPrefix.Length = 0 then 0uL else termPrefix[0]

        /// vectorize the search for the first minterm
        let firstSetChars = this.MintermIndexOfSpan(mergedPrefix)

        /// '.' to ^\n -> it's easier to invert large sets
        let isInverted = Solver.elemOfSet mergedPrefix minterms[0]

        while skipping do
            if loc.Reversed then
                slice <- inputSpan.Slice(0, currpos)
                if not isInverted then
                    sharedIndex <- slice.LastIndexOfAny(firstSetChars)

                else
                    sharedIndex <- slice.LastIndexOfAnyExcept(firstSetChars)
            else
                slice <- inputSpan.Slice(currpos)
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


#if DEBUG
                // let dbgSpan = loc.Input.AsSpan().Slice(potential)
                // let setPretty =
                //     setSpan.ToArray()
                //     |> Array.map this.PrettyPrintMinterm
                //     |> String.concat ""
                // let vs =
                //     [|
                //       this.PrettyPrintMinterm loopTerminator
                //       this.PrettyPrintMinterm nextLocMinterm
                //     |]
#endif


                let nextLocMinterm =
                    if loc.Reversed then
                        this.Classify(inputSpan[potential-1])
                        // this.MintermForStringIndex(loc.Input, potential - 1 )
                    else
                        this.Classify(inputSpan[potential])
                        // minterms[classifier.GetMintermID2(uint32 inputSpan[potential + i])]

                let mutable canTerminateLoop , looping =
                    if termPrefix.Length = 0 then
                        -1, true
                    else
                        let termMatch =
                            (Solver.elemOfSet termPrefix[0] nextLocMinterm)
                        (if termMatch then 0 else -1), termPrefix.Length <> 1

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

                    if canTerminateLoop >= 0 && i < termPrefix.Length
                       then
                           match Solver.elemOfSet termPrefix[i] nextLocMinterm with
                           | true ->
                               canTerminateLoop <- i
                               if i = termPrefix.Length - 1 then
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


    member this.Ascii : _ = classifier.Ascii
    member this.NonAscii : _ = classifier.NonAscii

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Classify(c: char) : uint64 =
        minterms[
            let i = int c
            match i < 128 with
            | true -> _ascii[i]
            | false -> _nonAscii.Find(i)
        ]

    member this.Classify2(c: char) : uint64 =
        minterms[
            let i = int c
            match i < 128 with
            | true -> _ascii[i]
            | false -> _nonAscii.Find(i)
        ]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Classify3(c: char) : uint64 =
        minterms[classifier.GetMintermID(int c)]

    member this.MtId(c: char) : int =
        match Char.IsAscii(c) with
        | true -> this.Ascii[int c]
        | false -> this.NonAscii.Find(int c)

    member val InitialPatternWithoutDotstar = _rawPattern
    // member val ReversePattern = _reversePattern

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
    member this.IsOrigReversePattern(node: RegexNode< uint64 >) : bool =
        obj.ReferenceEquals(node, _reversePattern)

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

    // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    // member cache.IsValidPredicate(pred: _, locationPredicate: _) : bool =
    //     cache.Solver.isElemOfSet (pred, locationPredicate)

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
