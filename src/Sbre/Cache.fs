namespace Sbre

open System
open System.Buffers
open System.Collections.Generic
open System.Numerics
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Microsoft.FSharp.Core
open Sbre.Types
open Sbre.Pat
open Info


type OptimizedUnique =
    // | WordBorder
    // | Bol // beginning of line
    // | Eol // end of line
    | StartOfString
    | NotStartOfString

// #nowarn "25" // missing patterns inference

[<Sealed>]
type RegexCache< 't
    // TSet when
    //     TSet: struct
    //     and TSet :> IEquatable< TSet >
    //     and TSet: equality

        >
    (
        _solver: ISolver<TSet>,
        _charsetSolver: CharSetSolver,
        _implicitDotstarPattern: RegexNode<TSet>,
        _rawPattern: RegexNode<TSet>,
        _reversePattern: RegexNode<TSet>,
        _builder: RegexBuilder<TSet>
    ) =
    let classifier =
        if typeof<TSet> = typeof<uint64> then
            ((box _solver) :?> UInt64Solver)._classifier
        else
            failwith "todo invalid solver"


    let _ascii = classifier.Ascii
    let _nonAscii = classifier.NonAscii
    let minterms: TSet[] = _solver.GetMinterms()
    let mintermBdds =
        (minterms |> Array.map (fun v -> _solver.ConvertToBDD(v, _charsetSolver)))

    let predStartsets = StartsetHelpers.startsetsFromMintermArray mintermBdds
    let mutable _cachedStartsets: Dictionary<TSet, SearchValues<char>> = Dictionary()
    let mutable _optimizedUniques: Dictionary<RegexNode<TSet>,OptimizedUnique> = Dictionary(Common.equalityComparer)

    let initUniques() =
        // _optimizedUniques.Add(_builder.anchors._wordBorder.Value, OptimizedUnique.WordBorder)
        // _optimizedUniques.Add(_builder.anchors._caretAnchor.Value, OptimizedUnique.Bol)
        // _optimizedUniques.Add(_builder.anchors._dollarAnchor.Value, OptimizedUnique.Eol)
        let notStartOfString = _builder.mkLookaround(_builder.uniques._true,true,false)
        _optimizedUniques.Add(notStartOfString, OptimizedUnique.NotStartOfString)

    do initUniques()

    let _getMintermStartsetChars (minterm:TSet) =
        match _cachedStartsets.TryGetValue(minterm) with
        | true, v -> v
        | _ ->
            let newSpan =
                StartsetHelpers.getMergedIndexOfSpan (
                    _solver,
                    predStartsets,
                    minterms,
                    minterm
                )
            _cachedStartsets.Add(minterm, newSpan)
            newSpan

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Minterms() = minterms
    member this.NumOfMinterms() = minterms.Length


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermBdds() = mintermBdds

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermStartsets() = predStartsets

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermSearchValues(startset: TSet) = _getMintermStartsetChars startset

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermChars(startset: TSet) : Span<char> = StartsetHelpers.getMintermChars(_solver,predStartsets, minterms, startset)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.SkipIndexOfAny(loc: byref<Location>, setChars: SearchValues<char>) : unit =
        if isNull setChars then () else
        let slice = loc.Input.Slice(loc.Position)
        let sharedIndex = slice.IndexOfAny(setChars)
        loc.Position <- loc.Position + sharedIndex
        if sharedIndex = -1 then
            loc.Position <- Location.final loc


    member this.SkipIndexOfAnyPrefix(loc: byref<Location>, setChars: SearchValues<char>, setPrefix: ReadOnlySpan<TSet>, termPrefix: ReadOnlySpan<TSet>) : unit =

        let mutable skipping = true

        while skipping do
            let slice = loc.Input.Slice(loc.Position)
            let sharedIndex = slice.IndexOfAny(setChars)
            if sharedIndex = -1 then
                loc.Position <- Location.final loc
            else
                loc.Position <- loc.Position + sharedIndex
                let currpos = loc.Position
                if currpos + setPrefix.Length > (loc.Input.Length - 1) then
                    skipping <- false
                else

                let nextLocMinterm =
                    this.Classify(loc.Input[loc.Position])

                let mutable canTerminateLoop , looping =
                    if termPrefix.Length = 0 then
                        -1, true
                    else
                        let termMatch = Solver.elemOfSet termPrefix[0] nextLocMinterm
                        (if termMatch then 0 else -1), termPrefix.Length <> 1

                let mutable i = 1
                let mutable stopSearch = true

                while looping && i < setPrefix.Length do
                    let nextLocMinterm =
                        if loc.Reversed then
                            this.Classify(loc.Input[loc.Position - i - 1])
                        else
                            this.Classify(loc.Input[loc.Position + i])

                    match (Solver.elemOfSet setPrefix[i] nextLocMinterm) with
                    | false ->
                        stopSearch <- false
                        looping <- false
                        loc.Position <- loc.Position + 1
                    | true ->
                        i <- i + 1

                    if canTerminateLoop >= 0 && i < termPrefix.Length
                       then
                           match (Solver.elemOfSet termPrefix[i] nextLocMinterm) with
                           | true ->
                               canTerminateLoop <- i
                               if i = termPrefix.Length - 1 then
                                  looping <- false
                                  stopSearch <- true
                           | false -> canTerminateLoop <- -1

                if stopSearch then
                    skipping <- false
                    loc.Position <- currpos


    member this.TryNextStartsetLocationRightToLeft(loc: byref<Location>, set:  SearchValues<char>, isInverted:bool) : unit =

        let currpos = loc.Position
        let slice = loc.Input.Slice(0, currpos)

        let sharedIndex =
            if isInverted then
                slice.LastIndexOfAnyExcept(set)
            else
                slice.LastIndexOfAny(set)

        match sharedIndex with
        | -1 -> loc.Position <- Location.final loc
        | _ -> loc.Position <- sharedIndex + 1



    member this.TryNextStartsetLocation(loc: byref<Location>, set: TSet) : unit =
        assert (not (Solver.isEmpty set))
        let setChars = this.MintermSearchValues(set)
        let isInverted = _solver.isElemOfSet (set,minterms[0])
        let currpos = loc.Position

        match loc.Reversed with
        | false ->
            let slice = loc.Input.Slice(currpos)

            let sharedIndex =
                if isInverted then
                    slice.IndexOfAnyExcept(setChars)
                else
                    slice.IndexOfAny(setChars)

            if sharedIndex = -1 then
                loc.Position <- Location.final loc
            else
                loc.Position <- currpos + sharedIndex
        | true ->
            let slice = loc.Input.Slice(0, currpos)

            let sharedIndex =
                if isInverted then
                    slice.LastIndexOfAnyExcept(setChars)
                else
                    slice.LastIndexOfAny(setChars)

            if sharedIndex = -1 then
                loc.Position <- Location.final loc
            else
                loc.Position <- sharedIndex + 1



    /// skip till a prefix of minterms matches
    member this.TryNextStartsetLocationArray(loc: byref<Location>, setSpan: ReadOnlySpan<TSet>, searchValues:SearchValues<char>) =
        // assert not loc.Reversed
        let inputSpan = loc.Input
        let mutable skipping = true

        /// vectorize the search for the first character
        let firstSetChars = searchValues // this.MintermStartsetChars(setSpan[0])
        let isInverted = Solver.elemOfSet setSpan[0] minterms[0]
        let tailPrefixSpan = setSpan.Slice(1)

        let _limitLength = inputSpan.Length - setSpan.Length
        let _tailLength = tailPrefixSpan.Length

        if setSpan.Length = 1 then
            let sharedIndex =
                let slice = inputSpan.Slice(loc.Position)
                if not isInverted then
                    slice.IndexOfAny(firstSetChars)
                else
                    slice.IndexOfAnyExcept(firstSetChars)
            if sharedIndex = -1 then
                skipping <- false
            else
                loc.Position <- loc.Position + sharedIndex
        else


        while skipping do
            let sharedIndex =
                let slice = inputSpan.Slice(loc.Position)
                if not isInverted then
                    slice.IndexOfAny(firstSetChars)
                else
                    slice.IndexOfAnyExcept(firstSetChars)

            if sharedIndex = -1 then
                skipping <- false
            else
                let potential = loc.Position + sharedIndex
                let mutable couldBe = true
                // exit if too far
                if potential > _limitLength then
                    skipping <- false
                    loc.Position <- potential
                    couldBe <- false

                let mutable i = 0
                while couldBe && i < _tailLength do
                    let inputMinterm = this.Classify(inputSpan[potential + 1 + i])
                    if Solver.notElemOfSet inputMinterm tailPrefixSpan[i] then
                        couldBe <- false
                    i <- i + 1

                if couldBe then
                    skipping <- false
                    loc.Position <- potential
                else
                    loc.Position <- potential + 1



    member this.TryNextStartsetLocationSearchValuesReversed(loc: inref<Location>, setSpan: ReadOnlySpan<SearchValues<char>>) =
        assert loc.Reversed
        assert (not (setSpan.Length < 2))

        let inputSpan = loc.Input
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable resultEnd = ValueNone
        let mutable slice: ReadOnlySpan<char> = inputSpan.Slice(0, currpos)

        /// vectorize the search for the first character
        let firstSetChars = setSpan[0]
        let tailPrefixSpan = setSpan.Slice(1)

        while skipping do
            let sharedIndex =
                slice <- inputSpan.Slice(0, currpos)
                slice.LastIndexOfAny(firstSetChars)


            if sharedIndex = -1 then
                skipping <- false
            else
                let potential = sharedIndex + 1
                let mutable couldBe = true

                // exit if too far
                if potential < setSpan.Length then
                    skipping <- false
                    resultEnd <- ValueSome(potential)
                    couldBe <- false

                // l to r
                let mutable i = tailPrefixSpan.Length - 1
                while couldBe && i >= 0 do
                    if not (tailPrefixSpan[i].Contains(inputSpan[potential - i - 2])) then
                        couldBe <- false
                    i <- i - 1

                if couldBe then
                    skipping <- false
                    resultEnd <- ValueSome(potential)
                else
                    currpos <- potential - 1

        resultEnd

    member this.TryNextStartsetLocationArrayReversed(loc: inref<Location>, setSpan: ReadOnlySpan<TSet>) =
        assert loc.Reversed

        let inputSpan = loc.Input
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable resultEnd = ValueNone
        let mutable slice: ReadOnlySpan<char> = inputSpan.Slice(0, currpos)

        /// vectorize the search for the first character
        let firstSetChars = this.MintermSearchValues(setSpan[0])
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
                resultEnd <- ValueSome(potential)

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
                if potential < setSpan.Length then
                    skipping <- false
                    resultEnd <- ValueSome(potential)
                    couldBe <- false

                // r to l
                // let mutable i = 0
                // while couldBe && i < tailPrefixSpan.Length do
                //     let inputMinterm = this.Classify(inputSpan[potential - i - 2])
                //     if Solver.notElemOfSet inputMinterm tailPrefixSpan[i] then
                //         couldBe <- false
                //     i <- i + 1

                // l to r
                let mutable i = _tailPrefixLength - 1
                while couldBe && i >= 0 do
                    let inputMinterm = this.Classify(inputSpan[potential - i - 2])
                    if Solver.notElemOfSet inputMinterm tailPrefixSpan[i] then
                        couldBe <- false
                    i <- i - 1

                if couldBe then
                    skipping <- false
                    resultEnd <- ValueSome(potential)
                else
                    currpos <- potential - 1

        resultEnd

    /// skip till a prefix of minterms matches
    member this.TryNextStartsetLocationArrayWithLoopTerminator(loc: inref<Location>, setSpan: ReadOnlySpan<TSet>, termSpan: ReadOnlySpan<TSet>) =
        assert (termSpan.Length > 0)
        let inputSpan = loc.Input
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let mutable sharedIndex = -1

        let mergedPrefix =
            setSpan[0] ||| termSpan[0]


        /// vectorize the search for the first minterm
        let firstSetChars = this.MintermSearchValues(mergedPrefix)

        /// '.' to ^\n -> it's easier to invert large sets
        // let isInverted = _solver.isElemOfSet(mergedPrefix,minterms[0])
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
                    | true -> potential < setSpan.Length
                    | _ -> potential + setSpan.Length > (loc.Input.Length - 1)

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
                            // _solver.isElemOfSet(termSpan[0],nextLocMinterm)
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

                    match (Solver.elemOfSet setSpan[i] nextLocMinterm) with
                    | false ->
                        stopSearch <- false
                        looping <- false
                        if loc.Reversed then currpos <- potential - 1
                        else currpos <- potential + 1
                    | true ->
                        i <- i + 1

                    if canTerminateLoop >= 0 && i < termSpan.Length
                       then
                           match (Solver.elemOfSet termSpan[i] nextLocMinterm) with
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
    member this.CurrentChar(loc: Location) : _ =
        let mutable pos = loc.Position
        if loc.Reversed then
            pos <- pos - 1
        if pos = loc.Input.Length || pos = -1 then
            ValueNone else
        ValueSome loc.Input[pos]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.PrevChar(loc: Location) : _ =
        let mutable pos = loc.Position
        if not loc.Reversed then
            pos <- pos - 1
        if pos = loc.Input.Length || pos = -1 then
            ValueNone else
        ValueSome loc.Input[pos]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermId(loc: Location) : _ =
        let mutable pos = loc.Position
        if loc.Reversed then
            pos <- pos - 1
        let i = int (loc.Input[pos])
        match i < 128 with
        | true -> _ascii[i]
        | false -> _nonAscii.Find(i)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermIsInverted(mt: TSet) : _ =
        _solver.isElemOfSet (mt,minterms[0])


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CharToMinterm(chr: char) : _ =
        let i = int chr
        match i < 128 with
        | true -> minterms[_ascii[i]]
        | false -> minterms[_nonAscii.Find(i)]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermById(id: int) =
        minterms[ id ]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Classify(c: char) =
        minterms[
            let i = int c
            match i < 128 with
            | true -> _ascii[i]
            | false -> _nonAscii.Find(i)
        ]

    member val InitialPatternWithoutDotstar = _rawPattern
    member val Solver: ISolver<TSet> = _solver
    member val CharsetSolver: CharSetSolver = _charsetSolver
    member val Builder = _builder

    // cached instantiation members
    member val True: RegexNode< _ > = _builder.uniques._true
    member val False: RegexNode< _ > = _builder.uniques._false
    member val Eps: RegexNode< _ > = _builder.uniques._eps
    member val TrueStar: RegexNode< _ > = _builder.uniques._trueStar
    member val FullMinterm: _ = _solver.Full

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsOrigReversePattern(node: RegexNode< TSet >) : bool =
        obj.ReferenceEquals(node, _reversePattern)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsImplicitDotStarred(node: RegexNode<TSet>) : bool =
        obj.ReferenceEquals(node, _implicitDotstarPattern)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GenerateSampleInput(node: RegexNode<TSet>) : bool =
        obj.ReferenceEquals(node, _implicitDotstarPattern)





#if DEBUG
    member cache.PrettyPrintMinterm(xs: _) : string = cache.Solver.PrettyPrint(xs, _charsetSolver)
    member this.PrettyPrintNode(node: RegexNode<TSet>) : string =
        Debug.debuggerSolver <- Some this.Solver
        node.ToString()
#endif
    member this.OptimizedUniques = _optimizedUniques
