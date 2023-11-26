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
        _builder: RegexBuilder<TSet>,
        _optimizations: RegexFindOptimizations
    ) as cache =
    let classifier =
        if typeof<TSet> = typeof<TSet> then
            ((box _solver) :?> UInt64Solver)._classifier
        elif typeof<TSet> = typeof<byte> then
            ((box _solver) :?> UInt8Solver)._classifier
        elif typeof<TSet> = typeof<uint16> then
            ((box _solver) :?> UInt16Solver).Classifier
        else
            failwith "todo"


    let _ascii = classifier.Ascii
    let _nonAscii = classifier.NonAscii
    let minterms: TSet[] = _solver.GetMinterms()





    let mintermBdds =
        (minterms |> Array.map (fun v -> _solver.ConvertToBDD(v, _charsetSolver)))

    let predStartsets = StartsetHelpers.startsetsFromMintermArray mintermBdds
    let mutable _cachedStartsets: Dictionary<TSet, SearchValues<char>> = Dictionary()
    let mutable _startsetPredicate = Startset.inferStartset _solver _rawPattern
    let mutable _initialStartset =
        match Startset.inferInitialStartset _solver _rawPattern with
        | InitialStartset.MintermArrayPrefix(arr, _) -> arr
        | _ ->
            // TODO: unoptimzed regex
            ([|_solver.Full|].AsMemory())

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
            _cachedStartsets.Add(minterm, (newSpan))
            newSpan

    let initialSearchValues = _getMintermStartsetChars _initialStartset.Span[0]


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Minterms() = minterms

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetInitialStartsetPrefix() = _initialStartset

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetInitialSearchValues() = initialSearchValues

    member this.GetInitialStartsetPredicate = _startsetPredicate

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermBdds() = mintermBdds

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermStartsets() = predStartsets

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermStartsetChars(startset: TSet) = _getMintermStartsetChars startset

    member this.TryNextStartsetLocationInitial(loc: byref<Location>, setChars: SearchValues<char>) : unit =
        // let currpos = loc.Position
        // match loc.Reversed with
        // | false ->
        let slice = loc.Input.Slice(loc.Position)
        let sharedIndex = slice.IndexOfAny(setChars)
        if sharedIndex = -1 then
            loc.Position <- Location.final loc
        else
            loc.Position <- loc.Position + sharedIndex
        // | true ->
        //     let slice = loc.Input.Slice(0, currpos)
        //
        //     let sharedIndex =
        //             slice.LastIndexOfAny(setChars)
        //
        //     if sharedIndex = -1 then
        //         loc.Position <- Location.final loc
        //     else
        //         loc.Position <- sharedIndex + 1

    member this.TryNextStartsetLocation(loc: byref<Location>, set: TSet) : unit =

        let setChars = this.MintermStartsetChars(set)
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
    member this.TryNextStartsetLocationArray(loc: byref<Location>, setSpan: ReadOnlySpan<TSet>) =
        // assert not loc.Reversed
        let inputSpan = loc.Input
        let mutable skipping = true
        // let mutable result = ValueNone

        /// vectorize the search for the first character
        let firstSetChars = this.MintermStartsetChars(setSpan[0])
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
                    // if _solver.notElemOfSet (inputMinterm,tailPrefixSpan[i]) then
                    if Solver.notElemOfSet inputMinterm tailPrefixSpan[i] then
                        couldBe <- false
                    i <- i + 1

                if couldBe then
                    skipping <- false
                    loc.Position <- potential
                else
                    loc.Position <- potential + 1



    member this.TryNextStartsetLocationArrayReversed(loc: inref<Location>, setSpan: ReadOnlySpan<TSet>) =
        assert loc.Reversed

        let inputSpan = loc.Input
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let mutable slice: ReadOnlySpan<char> = inputSpan.Slice(0, currpos)
        // let setSpan = prefix.AsSpan()

        /// vectorize the search for the first character
        let firstSetChars = this.MintermStartsetChars(setSpan[0])
        // let isInverted = _solver.isElemOfSet(setSpan[0],minterms[0])
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
                if potential < setSpan.Length then
                    skipping <- false
                    result <- ValueSome(potential)
                    couldBe <- false

                let mutable i = 0
                while couldBe && i < tailPrefixSpan.Length do
                    let inputMinterm = this.Classify(inputSpan[potential - i - 2])

                    // if _solver.notElemOfSet (inputMinterm,tailPrefixSpan[i]) then
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
        let firstSetChars = this.MintermStartsetChars(mergedPrefix)

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
    member this.MintermId(loc: Location) : _ =
        let mutable pos = loc.Position
        if loc.Reversed then
            pos <- pos - 1
        let c = loc.Input[pos]
        let i = int c
        match i < 128 with
        | true -> _ascii[i]
        | false -> _nonAscii.Find(i)

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
    member val TrueStar: RegexNode< _ > = _builder.uniques._trueStar
    member val FullMinterm: _ = _solver.Full

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsOrigReversePattern(node: RegexNode< TSet >) : bool =
        obj.ReferenceEquals(node, _reversePattern)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsImplicitDotStarred(node: RegexNode<TSet>) : bool =
        obj.ReferenceEquals(node, _implicitDotstarPattern)

#if DEBUG
    member cache.PrettyPrintMinterm(xs: _) : string = cache.Solver.PrettyPrint(xs, _charsetSolver)

    member this.PrettyPrintNode(node: RegexNode<TSet>) : string =
        let display(nodes: RegexNode<TSet>) = this.PrettyPrintNode(nodes)
        let paren str = $"({str})"

        let tostr(v: TSet) =
                if v = _solver.Full then
                    "⊤"
                elif _solver.IsEmpty(v) then
                    "⊥"
                else
                    match _solver.PrettyPrint(v, debugcharSetSolver) with
                    | @"[^\n]" -> "."
                    | c when c.Length > 12 -> "φ" // dont expand massive sets
                    | c -> c

        match node with
        | Singleton v -> tostr v
        | Or(items, _) ->
            let setItems: string list =
                if not (obj.ReferenceEquals(items, null)) then
                    items |> Seq.map (fun v -> this.PrettyPrintNode v ) |> Seq.toList
                else
                    []

            let combinedList = setItems

            combinedList |> String.concat "|" |> paren
        | And(items, _) ->
            let setItems: string list =
                if not (obj.ReferenceEquals(items, null)) then
                    items |> Seq.map display |> Seq.toList
                else
                    []

            setItems |> String.concat "&" |> paren
        | Not(items, info) ->
            let inner = this.PrettyPrintNode items

            $"~({inner})"
        | Loop(body, lower, upper, info) ->
            let inner = this.PrettyPrintNode body

            let isStar = lower = 0 && upper = Int32.MaxValue

            let inner = $"{inner}"

            let loopCount =
                if isStar then "*"
                elif lower = 1 && upper = Int32.MaxValue then "+"
                elif lower = 0 && upper = 1 then "?"
                else $"{{{lower},{upper}}}"

            match isStar with
            | true -> $"{inner}*"
            | false -> inner + loopCount

        | LookAround(body, lookBack, negate) ->
            let inner = this.PrettyPrintNode body

            match lookBack, negate with
            // | true, true when this.isFull body.Head -> "\\A"
            // | false, true when this.isFull body.Head -> "\\z"
            | false, true -> $"(?!{inner})"
            | false, false -> $"(?={inner})"
            | true, true -> $"(?<!{inner})"
            | true, false -> $"(?<={inner})"

        | Concat(h, t, info) -> this.PrettyPrintNode h + this.PrettyPrintNode t
        | Epsilon -> "ε"
#endif
    member this.Optimizations = _optimizations
