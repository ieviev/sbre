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

type MintermSearchValues<'t>
    (
        tset: 't,
        mode: MintermSearchMode,
        searchValues: SearchValues<char>,
        characters: Memory<char> option,
        minterms: 't[],
        classifier: MintermClassifier,
        solver: ISolver<'t>
    ) =
    member val Classifier: MintermClassifier = classifier with get, set
    member val Mode: MintermSearchMode = mode with get, set
    member val Minterm: 't = tset with get, set
    member val SearchValues: SearchValues<char> = searchValues with get, set
    member val CharactersInMinterm: Memory<char> option = characters with get, set
    member this.Contains(chr: char) =
        match mode with
        | MintermSearchMode.TSet ->
            let mtid = classifier.GetMintermID(int chr)
            let charminterm = minterms[mtid]
            solver.elemOfSet this.Minterm charminterm
        | MintermSearchMode.SearchValues -> this.SearchValues.Contains(chr)
        | MintermSearchMode.InvertedSearchValues -> not(this.SearchValues.Contains(chr))
        | _ -> ArgumentOutOfRangeException() |> raise

    override this.ToString() =
        let desc =
            match this.Mode with
            | MintermSearchMode.TSet -> $"{this.Minterm}"
            | MintermSearchMode.SearchValues -> $"%A{this.CharactersInMinterm.Value}"
            | MintermSearchMode.InvertedSearchValues -> $"%A{this.CharactersInMinterm.Value}"
            | _ -> ArgumentOutOfRangeException() |> raise
        $"{this.Mode.ToString()}: {desc}"

    new(tset: 't, minterms: 't[], classifier:MintermClassifier, solver) =
        MintermSearchValues<'t>(
            tset,
            MintermSearchMode.TSet,
            Unchecked.defaultof<_>,
            None,
            minterms,
            classifier,
            solver
        )

    new(tset: 't, characters: Memory<char>, invert: bool, solver) =
        let mode =
            if invert then
                MintermSearchMode.InvertedSearchValues
            else
                MintermSearchMode.SearchValues

        MintermSearchValues<'t>(
            tset,
            mode,
            SearchValues.Create(characters.Span),
            Some characters,
            Unchecked.defaultof<_>,
            Unchecked.defaultof<_>,
            solver
        )


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

    /// newer implementation with inverted sets and tsets
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermChars(startset: 't) : Memory<char> option =
        StartsetHelpers.tryGetMintermChars (_solver, predStartsets, minterms, startset)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.HasMintermPrefix(loc: byref<Location>, setPrefix: ReadOnlySpan<'t>) : bool =
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


    member this.TryNextStartsetLocationRightToLeft
        (
            loc: byref<Location>,
            set: SearchValues<char>,
            isInverted: bool
        ) : unit =

        let currpos = loc.Position

        // try
        let slice = loc.Input.Slice(0, currpos)


        let sharedIndex =
            if isInverted then
                slice.LastIndexOfAnyExcept(set)
            else
                slice.LastIndexOfAny(set)

        match sharedIndex with
        | -1 -> loc.Position <- Location.final loc
        | _ -> loc.Position <- sharedIndex + 1
        // with e ->
        //     failwith $"pos:{loc.Position}\n{loc.Input.ToString()}"





    member this.TryNextStartsetLocationArrayReversed
        (
            loc: inref<Location>,
            setSpan: ReadOnlySpan<MintermSearchValues<'t>>
        ) =
        assert loc.Reversed

        let inputSpan = loc.Input
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable resultEnd = ValueNone
        let mutable slice: ReadOnlySpan<char> = inputSpan.Slice(0, currpos)
        let searchValues = setSpan[0] //this.MintermSearchValues()
        let firstSetChars = searchValues.SearchValues
        let tailPrefixSpan = setSpan.Slice(1)

        if tailPrefixSpan.Length = 0 then
            skipping <- false

            let sharedIndex =
                slice <- inputSpan.Slice(0, currpos)
                match searchValues.Mode with
                | MintermSearchMode.SearchValues ->
                    slice.LastIndexOfAny(firstSetChars)
                | MintermSearchMode.InvertedSearchValues ->
                    slice.LastIndexOfAnyExcept(firstSetChars)
                | MintermSearchMode.TSet ->
                    let mutable fnd = false
                    let mutable i = slice.Length - 1
                    while not fnd && i >= 0 do
                        if searchValues.Contains(slice[i]) then
                            fnd <- true
                        i <- i - 1
                    if fnd then
                        i + 1
                    else -1
                | _ -> failwith "impossible"

            if not (sharedIndex = -1) then
                let potential = sharedIndex + 1
                resultEnd <- ValueSome(potential)

        while skipping do
            let sharedIndex =
                slice <- inputSpan.Slice(0, currpos)
                match searchValues.Mode with
                | MintermSearchMode.SearchValues ->
                    slice.LastIndexOfAny(firstSetChars)
                | MintermSearchMode.InvertedSearchValues ->
                    slice.LastIndexOfAnyExcept(firstSetChars)
                | MintermSearchMode.TSet ->
                    let mutable fnd = false
                    let mutable i = slice.Length - 1
                    while not fnd && i >= 0 do
                        if searchValues.Contains(slice[i]) then
                            fnd <- true
                        i <- i - 1
                    if fnd then
                        i + 1
                    else -1
                | _ -> failwith "impossible"

            if sharedIndex = -1 then
                skipping <- false
            else
                let potential = sharedIndex + 1

                let mutable couldBe = true

                // exit if too far
                if potential < setSpan.Length then
                    skipping <- false
                    resultEnd <- ValueNone //ValueSome(potential)
                    couldBe <- false

                // let pref = tailPrefixSpan.ToArray() |> Array.map this.PrettyPrintMinterm
                // r to l
                let mutable i = 0

                while couldBe && i < tailPrefixSpan.Length do
                    // let inputMinterm = this.Classify(inputSpan[potential - i - 2])
                    // let loc1 = this.PrettyPrintMinterm(inputMinterm)
                    // let loc2 = this.PrettyPrintMinterm(tailPrefixSpan[i])
                    // let c1 = tailPrefixSpan[i].Contains(inputSpan[potential - i - 2])
                    // if _solver.notElemOfSet inputMinterm tailPrefixSpan[i].Minterm then
                    if not(tailPrefixSpan[i].Contains(inputSpan[potential - i - 2])) then
                        couldBe <- false

                    i <- i + 1

                // l to r
                // let mutable i = _tailPrefixLength - 1
                // while couldBe && i >= 0 do
                //     let inputMinterm = this.Classify(inputSpan[potential - i - 2])
                //     if Solver.notElemOfSet inputMinterm tailPrefixSpan[i] then
                //         couldBe <- false
                //     i <- i - 1

                if couldBe then
                    skipping <- false
                    resultEnd <- ValueSome(potential)
                else
                    currpos <- potential - 1

        resultEnd

    // /// skip till a prefix of minterms matches
    // member this.TryNextStartsetLocationArrayWithLoopTerminator
    //     (
    //         loc: inref<Location>,
    //         setSpan: ReadOnlySpan<TSet>,
    //         termSpan: ReadOnlySpan<TSet>
    //     ) =
    //     assert (termSpan.Length > 0)
    //     let inputSpan = loc.Input
    //     let mutable currpos = loc.Position
    //     let mutable skipping = true
    //     let mutable result = ValueNone
    //     let mutable sharedIndex = -1
    //
    //     let mergedPrefix = setSpan[0] ||| termSpan[0]
    //
    //     let searchValues = this.MintermSearchValues(mergedPrefix)
    //     match searchValues.Mode with
    //     | MintermSearchMode.TSet -> ValueSome(currpos)
    //     | _ ->
    //     let firstSetChars = searchValues.SearchValues
    //     let isInverted =
    //         match searchValues.Mode with
    //         | MintermSearchMode.InvertedSearchValues -> true
    //         | _ -> false
    //
    //     while skipping do
    //         if loc.Reversed then
    //             let slice = inputSpan.Slice(0, currpos)
    //
    //             if not isInverted then
    //                 sharedIndex <- slice.LastIndexOfAny(firstSetChars)
    //             else
    //                 sharedIndex <- slice.LastIndexOfAnyExcept(firstSetChars)
    //         else
    //             let slice = inputSpan.Slice(currpos)
    //
    //             if not isInverted then
    //                 sharedIndex <- slice.IndexOfAny(firstSetChars)
    //             else
    //                 sharedIndex <- slice.IndexOfAnyExcept(firstSetChars)
    //
    //
    //         if sharedIndex = -1 then
    //             skipping <- false
    //             result <- ValueNone
    //         else
    //             let potential = if loc.Reversed then sharedIndex + 1 else currpos + sharedIndex
    //
    //             let shouldExit =
    //                 match loc.Reversed with
    //                 | true -> potential < setSpan.Length
    //                 | _ -> potential + setSpan.Length > (loc.Input.Length - 1)
    //
    //             if shouldExit then
    //                 skipping <- false
    //                 result <- ValueSome(potential)
    //             else
    //
    //             let nextLocMinterm =
    //                 if loc.Reversed then
    //                     this.Classify(inputSpan[potential - 1])
    //                 else
    //                     this.Classify(inputSpan[potential])
    //
    //             let mutable canTerminateLoop, looping =
    //                 if termSpan.Length = 0 then
    //                     -1, true
    //                 else
    //                     let termMatch =
    //                         // _solver.isElemOfSet(termSpan[0],nextLocMinterm)
    //                         (Solver.elemOfSet termSpan[0] nextLocMinterm)
    //
    //                     (if termMatch then 0 else -1), termSpan.Length <> 1
    //
    //             let mutable i = 1
    //             let mutable stopSearch = true
    //
    //             while looping && i < setSpan.Length do
    //                 let nextLocMinterm =
    //                     if loc.Reversed then
    //                         this.Classify(inputSpan[potential - i - 1])
    //                     else
    //                         this.Classify(inputSpan[potential + i])
    //
    //                 match (Solver.elemOfSet setSpan[i] nextLocMinterm) with
    //                 | false ->
    //                     stopSearch <- false
    //                     looping <- false
    //
    //                     if loc.Reversed then
    //                         currpos <- potential - 1
    //                     else
    //                         currpos <- potential + 1
    //                 | true -> i <- i + 1
    //
    //                 if canTerminateLoop >= 0 && i < termSpan.Length then
    //                     match (Solver.elemOfSet termSpan[i] nextLocMinterm) with
    //                     | true ->
    //                         canTerminateLoop <- i
    //
    //                         if i = termSpan.Length - 1 then
    //                             looping <- false
    //                             stopSearch <- true
    //                     | false -> canTerminateLoop <- -1
    //
    //             if stopSearch then
    //                 skipping <- false
    //                 result <- ValueSome(potential)
    //
    //     result


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

        if pos = loc.Input.Length || pos < 0 then
            ValueNone
        else
            ValueSome loc.Input[pos]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.PrevChar(loc: Location) : _ =
        let mutable pos = loc.Position

        if not loc.Reversed then
            pos <- pos - 1

        if pos = loc.Input.Length || pos = -1 then
            ValueNone
        else
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
    member this.MintermIsInverted(mt: 't) : _ = _solver.isElemOfSet (mt, minterms[0])


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CharToMinterm(chr: char) : _ =
        let i = int chr

        match i < 128 with
        | true -> minterms[_ascii[i]]
        | false -> minterms[_nonAscii.Find(i)]

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


    member cache.PrettyPrintMinterm(xs: _) : string = (box cache.Solver :?> UInt64Solver).PrettyPrint(xs, _charsetSolver)

    // member this.PrettyPrintNode(node: RegexNode<'t>) : string =

    //     Debug.debuggerSolver <- Some (box this.Solver :?> UInt64Solver :> ISolver<uint64>)
    //     node.ToString()



#if DEBUG

#endif
