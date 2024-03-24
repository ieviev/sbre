namespace rec Sbre

open System
open System.Buffers
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open Microsoft.FSharp.Core
open Sbre.Types
open Sbre.Pat

type MintermSearchMode =
    | TSet = 0
    | SearchValues = 1
    | InvertedSearchValues = 2

[<Sealed>]
type MintermSearchValues<'t> =
    val Classifier: MintermClassifier
    val Mode: MintermSearchMode
    val Minterms: 't[]
    val Minterm: 't
    val SearchValuesUtf16: SearchValues<char>
    val SearchValuesByte: SearchValues<byte>
    val SearchValuesSize: int
    val CharactersInMinterm: Memory<char> option
    val Solver: ISolver<'t>

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Contains(chr: char) =
        match this.Mode with
        | MintermSearchMode.SearchValues -> this.SearchValuesUtf16.Contains(chr)
        | MintermSearchMode.InvertedSearchValues -> not(this.SearchValuesUtf16.Contains(chr))
        | MintermSearchMode.TSet ->
            let mtid = this.Classifier.GetMintermID(int chr)
            let charminterm = this.Minterms[mtid]
            this.Solver.elemOfSet this.Minterm charminterm
        | _ -> failwith ""

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Contains(b: byte) =
        match this.Mode with
        | MintermSearchMode.SearchValues -> this.SearchValuesByte.Contains(b)
        | MintermSearchMode.InvertedSearchValues -> not(this.SearchValuesByte.Contains(b))
        | MintermSearchMode.TSet ->
            let mtid = this.Classifier.GetMintermID(int b)
            let charminterm = this.Minterms[mtid]
            this.Solver.elemOfSet this.Minterm charminterm
        | _ -> failwith ""

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TryNextIndexRightToLeftByte
        (
            slice: inref<ReadOnlySpan<byte>>
        ) : int =
        match this.Mode with
        | MintermSearchMode.SearchValues ->
            slice.LastIndexOfAny(this.SearchValuesByte)
        | MintermSearchMode.InvertedSearchValues ->
            slice.LastIndexOfAnyExcept(this.SearchValuesByte)
        | MintermSearchMode.TSet ->
            let mutable fnd = false
            let mutable i = slice.Length - 1
            while not fnd && i >= 0 do
                if this.Contains(slice[i]) then
                    fnd <- true
                    i <- i + 1
                i <- i - 1
            i
        | _ -> failwith ""

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TryNextIndexLeftToRightByte
        (
            slice: ReadOnlySpan<byte>
        ) =
        match this.Mode with
        | MintermSearchMode.SearchValues ->
            slice.IndexOfAny(this.SearchValuesByte)
        | MintermSearchMode.InvertedSearchValues ->
            slice.IndexOfAnyExcept(this.SearchValuesByte)
        | MintermSearchMode.TSet ->
            let mutable fnd = false
            let mutable i = 0
            while not fnd && i < slice.Length do
                if this.Contains(slice[i]) then
                    fnd <- true
                    i <- i - 1
                i <- i + 1
            if not fnd then -1 else i
        | _ -> failwith "impossible"

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TryNextIndexRightToLeftChar
        (
            slice: inref<ReadOnlySpan<char>>
        ) : int =
        match this.Mode with
        | MintermSearchMode.SearchValues ->
            slice.LastIndexOfAny(this.SearchValuesUtf16)
        | MintermSearchMode.InvertedSearchValues ->
            slice.LastIndexOfAnyExcept(this.SearchValuesUtf16)
        | MintermSearchMode.TSet ->
            let mutable fnd = false
            let mutable i = slice.Length - 1
            while not fnd && i >= 0 do
                if this.Contains(slice[i]) then
                    fnd <- true
                    i <- i + 1
                i <- i - 1
            i
        | _ -> failwith ""

    override this.ToString() =
        let desc =
            match this.Mode with
            | MintermSearchMode.TSet -> $"{this.Minterm}"
            | MintermSearchMode.SearchValues -> $"%A{this.CharactersInMinterm.Value}"
            | MintermSearchMode.InvertedSearchValues -> $"%A{this.CharactersInMinterm.Value}"
            | _ -> ArgumentOutOfRangeException() |> raise
        $"{this.Mode.ToString()}: {desc}"

    new(tset: 't, minterms: 't[], classifier:MintermClassifier, solver) =
        {
            Classifier = classifier
            Mode = MintermSearchMode.TSet
            Minterms = minterms
            Minterm = tset
            SearchValuesUtf16 = Unchecked.defaultof<_>
            SearchValuesByte = Unchecked.defaultof<_>
            CharactersInMinterm = None
            Solver = solver
            SearchValuesSize = 0
        }

    new(tset: 't, characters: Memory<char>, invert: bool, solver) =
        let mode =
            if invert then
                MintermSearchMode.InvertedSearchValues
            else
                MintermSearchMode.SearchValues

        let byteSearchValues =
            match Memory.tryConvertToAscii(characters) with
            | ValueSome (chars) ->
                SearchValues.Create(chars.Span)
            | _ ->
                Unchecked.defaultof<_>
        {
            Mode = mode
            Minterm = tset
            CharactersInMinterm = Some characters
            SearchValuesUtf16 = SearchValues.Create(characters.Span)
            SearchValuesByte = byteSearchValues
            Minterms = Unchecked.defaultof<_>
            Classifier = Unchecked.defaultof<_>
            Solver = solver
            SearchValuesSize = characters.Length
        }




[<Sealed>]
type RegexCache<
    't when
        't: struct
        and 't :> IEquatable< 't >
        and 't : equality
>
    (
        _solver: ISolver<'t>,
        _charsetSolver: CharSetSolver,
        _bddMinterms: BDD[],
        _rawPattern: RegexNode<'t>,
        _builder: RegexBuilder<'t>,
        _bddbuilder: RegexBuilder<BDD>
    ) =
    // let classifier = (box _solver :?> TSolver<'t>)._classifier
    // let classifier = (box _solver :?> BitVectorSolver)._classifier
    let classifier =
        if typeof<'t> = typeof<BitVector> then
            (box _solver :?> BitVectorSolver)._classifier
        elif typeof<'t> = typeof<uint64> then
            (box _solver :?> UInt64Solver)._classifier
        else failwith "invalid solver"
    let _ascii = classifier.Ascii
    let _nonAscii = classifier.NonAscii
    let minterms: 't[] = _solver.GetMinterms()
    let mintermBdds = _bddMinterms
    let predStartsets = StartsetHelpers.startsetsFromMintermArray mintermBdds
    let mutable _cachedStartsets: Dictionary<'t, MintermSearchValues<'t>> = Dictionary()

    let _getMintermStartsetChars(tset: 't) =
        match _cachedStartsets.TryGetValue(tset) with
        | true, v -> v
        | _ ->
            let mintermCharsOpt =
                StartsetHelpers.tryGetMintermChars (_solver, predStartsets, minterms, tset)
            let searchValues =
                match mintermCharsOpt with
                // set too big
                | None -> MintermSearchValues<'t>(tset, minterms, classifier, _solver)
                | Some chars ->
                    let isInverted = _solver.isElemOfSet (tset, minterms[0])
                    MintermSearchValues(tset, chars, isInverted, _solver)
            _cachedStartsets.Add(tset, searchValues)
            searchValues




    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Minterms() : 't array = minterms

    member this.NumOfMinterms() = minterms.Length


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermBdds() = mintermBdds

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermStartsets() = predStartsets

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermSearchValues(startset: 't) = _getMintermStartsetChars startset

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermChars(startset: 't) : Memory<char> option =
        StartsetHelpers.tryGetMintermChars (_solver, predStartsets, minterms, startset)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.HasMintermPrefix(loc: byref<Location<_>>, setPrefix: ReadOnlySpan<'t>) : bool =
        let mutable couldBe = true
        let mutable i = 0
        let inputSpan = loc.Input.Slice(loc.Position)

        while couldBe && i < setPrefix.Length do
            let inputMinterm = this.Classify(inputSpan[i])

            if _solver.notElemOfSet inputMinterm (setPrefix[i]) then
                couldBe <- false

            i <- i + 1

        couldBe

    // member this.SkipIndexOfAnyPrefix
    //     (
    //         loc: byref<Location>,
    //         setChars: SearchValues<char>,
    //         setPrefix: ReadOnlySpan<TSet>,
    //         termPrefix: ReadOnlySpan<TSet>
    //     ) : unit =
    //
    //     let mutable skipping = true
    //
    //     while skipping do
    //         let slice = loc.Input.Slice(loc.Position)
    //         let sharedIndex = slice.IndexOfAny(setChars)
    //
    //         if sharedIndex = -1 then
    //             loc.Position <- Location.final loc
    //         else
    //             loc.Position <- loc.Position + sharedIndex
    //             let currpos = loc.Position
    //
    //             if currpos + setPrefix.Length > (loc.Input.Length - 1) then
    //                 skipping <- false
    //             else
    //
    //             let nextLocMinterm = this.Classify(loc.Input[loc.Position])
    //
    //             let mutable canTerminateLoop, looping =
    //                 if termPrefix.Length = 0 then
    //                     -1, true
    //                 else
    //                     let termMatch = Solver.elemOfSet termPrefix[0] nextLocMinterm
    //                     (if termMatch then 0 else -1), termPrefix.Length <> 1
    //
    //             let mutable i = 1
    //             let mutable stopSearch = true
    //
    //             while looping && i < setPrefix.Length do
    //                 let nextLocMinterm =
    //                     if loc.Reversed then
    //                         this.Classify(loc.Input[loc.Position - i - 1])
    //                     else
    //                         this.Classify(loc.Input[loc.Position + i])
    //
    //                 match (Solver.elemOfSet setPrefix[i] nextLocMinterm) with
    //                 | false ->
    //                     stopSearch <- false
    //                     looping <- false
    //                     loc.Position <- loc.Position + 1
    //                 | true -> i <- i + 1
    //
    //                 if canTerminateLoop >= 0 && i < termPrefix.Length then
    //                     match (Solver.elemOfSet termPrefix[i] nextLocMinterm) with
    //                     | true ->
    //                         canTerminateLoop <- i
    //
    //                         if i = termPrefix.Length - 1 then
    //                             looping <- false
    //                             stopSearch <- true
    //                     | false -> canTerminateLoop <- -1
    //
    //             if stopSearch then
    //                 skipping <- false
    //                 loc.Position <- currpos


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TryNextIndexRightToLeftChar
        (
            slice: inref<ReadOnlySpan<char>>,
            set: MintermSearchValues<_>
        ) : int =
        match set.Mode with
        | MintermSearchMode.SearchValues ->
            slice.LastIndexOfAny(set.SearchValuesUtf16)
        | MintermSearchMode.InvertedSearchValues ->
            slice.LastIndexOfAnyExcept(set.SearchValuesUtf16)
        | MintermSearchMode.TSet ->
            let mutable fnd = false
            let mutable i = slice.Length - 1
            while not fnd && i >= 0 do
                if set.Contains(slice[i]) then
                    fnd <- true
                    i <- i + 1
                i <- i - 1
            i
        | _ -> failwith ""


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TryNextIndexLeftToRight
        (
            slice: ReadOnlySpan<char>,
            set: MintermSearchValues<'t>
        ) =
        match set.Mode with
        | MintermSearchMode.SearchValues ->
            slice.IndexOfAny(set.SearchValuesUtf16)
        | MintermSearchMode.InvertedSearchValues ->
            slice.IndexOfAnyExcept(set.SearchValuesUtf16)
        | MintermSearchMode.TSet ->
            let mutable fnd = false
            let mutable i = 0
            while not fnd && i < slice.Length do
                if set.Contains(slice[i]) then
                    fnd <- true
                    i <- i - 1
                i <- i + 1
            if not fnd then -1 else i
        | _ -> failwith "impossible"



    member this.TryNextStartsetLocationArrayReversed
        (
            loc: inref<Location<_>>,
            setSpan: ReadOnlySpan<MintermSearchValues<'t>>
        ) =
        assert loc.Reversed

        let inputSpan = loc.Input
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable resultEnd = ValueNone
        let searchValues = setSpan[0]
        let tailPrefixSpan = setSpan.Slice(1)
        // let tailPrefixLength = tailPrefixSpan.Length

        if tailPrefixSpan.Length = 0 then
            skipping <- false
            let slice = inputSpan.Slice(0, currpos)
            let sharedIndex =
                this.TryNextIndexRightToLeftChar(&slice,searchValues)
            resultEnd <- ValueSome(sharedIndex + 1)

        while skipping do
            let slice = inputSpan.Slice(0, currpos)
            let sharedIndex =
                this.TryNextIndexRightToLeftChar(&slice,searchValues)

            if sharedIndex = -1 then
                skipping <- false
            else
                let mutable couldBe = true

                // exit if too far
                if sharedIndex < tailPrefixSpan.Length then
                    skipping <- false
                    couldBe <- false

                let mutable i = 0
                while couldBe && i < tailPrefixSpan.Length do
                    if not(tailPrefixSpan[i].Contains(inputSpan[sharedIndex - i - 1])) then
                        couldBe <- false
                    i <- i + 1

                if couldBe then
                    skipping <- false
                    resultEnd <- ValueSome(sharedIndex + 1)
                else
                    currpos <- sharedIndex

        resultEnd

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForLocation(loc: Location<_>) : _ =
        let mutable pos = loc.Position

        if loc.Reversed then
            pos <- pos - 1

        this.Classify(loc.Input[pos])

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CurrentChar(loc: Location<_>) : _ =
        let mutable pos = loc.Position

        if loc.Reversed then
            pos <- pos - 1

        if pos = loc.Input.Length || pos < 0 then
            ValueNone
        else
            ValueSome loc.Input[pos]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.PrevChar(loc: Location<_>) : _ =
        let mutable pos = loc.Position

        if not loc.Reversed then
            pos <- pos - 1

        if pos = loc.Input.Length || pos = -1 then
            ValueNone
        else
            ValueSome loc.Input[pos]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermIdChar(loc: Location<char>) : int =
        let i =
            int (if loc.Reversed then loc.Input[loc.Position - 1] else loc.Input[loc.Position])
        match i < 128 with
        | true -> _ascii[i]
        | false -> _nonAscii.Find(i)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermIdByte(loc: Location<byte>) : int =
        let i =
            int (if loc.Reversed then loc.Input[loc.Position - 1] else loc.Input[loc.Position])
        match i < 128 with
        | true -> _ascii[i]
        | false -> 0 // all other chars

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermIsInverted(mt: 't) : _ = _solver.isElemOfSet (mt, minterms[0])


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CharToMinterm(chr: char) : _ =
        let i = int chr

        match i < 128 with
        | true -> minterms[_ascii[i]]
        | false -> minterms[_nonAscii.Find(i)]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CharToMintermId(chr: char) : int =
        let i = int chr

        match i < 128 with
        | true -> _ascii[i]
        | false -> _nonAscii.Find(i)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermById(id: int) = minterms[id]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Classify(c: char) =
        minterms[let i = int c

                 match i < 128 with
                 | true -> _ascii[i]
                 | false -> _nonAscii.Find(i)]



    member val InitialPatternWithoutDotstar = _rawPattern
    member val Solver: ISolver<'t> = _solver
    member val CharsetSolver: CharSetSolver = _charsetSolver
    member val Builder = _builder
    member val BddBuilder = _bddbuilder

    // cached instantiation members
    member val True: RegexNode<_> = _builder.uniques._true
    member val False: RegexNode<_> = _builder.uniques._false
    member val Eps: RegexNode<_> = _builder.uniques._eps
    member val TrueStar: RegexNode<_> = _builder.uniques._trueStar
    member val Ascii = _ascii
    member val NonAscii = _nonAscii

    member cache.PrettyPrintMinterm(xs: _) : string = (box cache.Solver :?> UInt64Solver).PrettyPrint(xs, _charsetSolver)


#if DEBUG

#endif
