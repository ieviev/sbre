namespace Sbre

open System
open System.Buffers
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open Sbre.Types
open Sbre.Pat
open Info

#nowarn "25" // missing patterns inference

type impl = MethodImplAttribute
type implOpts = MethodImplOptions

[<Sealed>]
type RegexCache< ^t when ^t: struct and ^t :> IEquatable< ^t > and ^t: equality>
    (
        _solver: ISolver< ^t >,
        _charsetSolver: CharSetSolver,
        _implicitDotstarPattern: RegexNode<'t>,
        _rawPattern: RegexNode<'t>,
        _builder: RegexBuilder< ^t >,
        _optimizations: RegexFindOptimizations
    ) =

    let classifier =
        match box _solver with
        | :? UInt64Solver as v -> v._classifier
        | :? BitVectorSolver as v -> v._classifier
        | _ -> failwith $"unknown solver of type {_solver}"

    let _tagReader =
        match box _solver with
        | :? UInt64Solver ->
            FSharpValue.PreComputeUnionTagReader(
                typeof<RegexNode<uint64>>,
                System.Reflection.BindingFlags.Public
            )
        | :? BitVectorSolver ->
            FSharpValue.PreComputeUnionTagReader(
                typeof<RegexNode<BDD>>,
                System.Reflection.BindingFlags.Public
            )
        | _ -> failwith $"unknown solver of type {_solver}"

    let minterms: ^t[] = _solver.GetMinterms()

    let mintermBdds =
        lazy (minterms |> Array.map (fun v -> _solver.ConvertToBDD(v, _charsetSolver)))

    let predStartsets = lazy StartsetHelpers.startsetsFromMintermArray (mintermBdds.Value)

    let initialSs2 = Startset.inferStartset2 (_solver) _rawPattern

    let mutable _cachedBDDRanges: Dictionary< ^t, PredStartset > = Dictionary()
    let mutable _cachedStartsets: Dictionary< uint64, char[] > = Dictionary()
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
    member this.MintermIndexOfSpan(startset: ^t) =
        match _cachedStartsets.TryGetValue(unbox startset) with
        | true, v -> v.AsSpan()
        | _ ->
            let newSpan =
                StartsetHelpers.getMergedIndexOfSpan (
                    predStartsets.Value,
                    unbox minterms,
                    unbox startset
                )

            _cachedStartsets.Add(unbox startset, newSpan.ToArray())
            newSpan

    member this.TryNextStartsetLocation(loc: Location, set: ^t) =

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

    member this.TryNextStartsetLocation2(loc: Location, set: ^t, set2: ^t) =

        let setChars = this.MintermIndexOfSpan(set)
        let isInverted = this.IsValidPredicate(set, minterms[0])
        let mutable currpos = loc.Position
        let mutable skipping = true
        let mutable result = ValueNone
        let inputSpan = loc.Input.AsSpan()
        let mutable slice = inputSpan.Slice(currpos)
        let mutable sharedIndex = 0

        let inline nextLocMinterm(pos: int) : 't =
            if loc.Reversed then
                this.MintermForStringIndex(loc.Input, pos - 2)
            else
                this.MintermForStringIndex(loc.Input, pos + 1)

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
                        match
                            Solver.isElemOfSetU64 (unbox set2) (unbox (nextLocMinterm (potential)))
                        with
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

                    // match this.IsValidPredicate(set2, nextLocMinterm (potential)) with
                    match Solver.isElemOfSetU64 (unbox set2) (unbox (nextLocMinterm (potential))) with
                    | false -> currpos <- potential + 1
                    | true ->
                        skipping <- false
                        result <- ValueSome(potential)

            result
        | true, _ ->
            while skipping do
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

                    match this.IsValidPredicate(set2, nextLocMinterm (potential + 1)) with
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
            let loc_pred = minterms[
                classifier.GetMintermID2(uint32 e.Current)
            ]

            if not (Solver.isElemOfSetU64 (unbox loc_pred) set) || not (e.MoveNext()) then () else

            counter <- counter + 1u

            let loc_pred_2 = minterms[
                classifier.GetMintermID2(uint32 e.Current)
            ]

            if not (Solver.isElemOfSetU64 (unbox loc_pred_2) set2) then () else

            skipping <- false
            result <- ValueSome(loc.Position + int counter)

        result



    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForLocation(loc: Location) : ^t =
        match loc.Reversed with
        | false -> minterms[classifier.GetMintermID(int loc.Input[loc.Position])]
        | true -> minterms[classifier.GetMintermID(int loc.Input[loc.Position - 1])]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForStringIndex(str: string, pos: int) =
        minterms[classifier.GetMintermID(int str[pos])]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CharToMinterm(c: inref<char>) : ^t = minterms[classifier.GetMintermID(int c)]

    member val InitialPatternWithoutDotstar = _rawPattern

    member val Solver: ISolver< ^t > = _solver
    member val CharsetSolver: CharSetSolver = _charsetSolver
    member val Builder = _builder

    // cached instantiation members
    member val True: RegexNode< ^t > = _builder.uniques._true
    //
    //
    member val False: RegexNode< ^t > = _builder.uniques._false

    //
    member val TrueStar: RegexNode< ^t > = _builder.uniques._trueStar


    member val FullMinterm: ^t = _solver.Full


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsTrueStar(node: RegexNode< ^t >) : bool =
        obj.ReferenceEquals(node, _builder.uniques._trueStar)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsFalse(node: RegexNode< ^t >) : bool =
        obj.ReferenceEquals(node, _builder.uniques._false)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateInfo(flags, startset) : RegexNodeInfo<_> =
        _builder.CreateInfo(flags, startset)



    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsTrue(node: RegexNode< ^t >) : bool =
        obj.ReferenceEquals(node, _builder.uniques._true)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsImplicitDotStarred(node: RegexNode<'t>) : bool =
        obj.ReferenceEquals(node, _implicitDotstarPattern)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.GetPredicateResult(location: Location, pred: ^t) =
        let mterm = cache.MintermForLocation(location)
        cache.Solver.isElemOfSet (pred, mterm)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.IsValidPredicate(pred: ^t, locationPredicate: ^t) : bool =
        // (Solver.isElemOfSetU64 pred locationPredicate)
        cache.Solver.isElemOfSet (pred, locationPredicate)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.IsValidPredicateUint64(pred: uint64, locationPredicate: uint64) : bool =
        (pred &&& locationPredicate) > 0uL


#if DEBUG
    member cache.PrintNode(xs: RegexNode<'t>) =

        if obj.ReferenceEquals(xs, null) then
            "null"
        else
            xs.ToStringHelper()

    member cache.PrettyPrintMinterm(xs: ^t) : string = cache.Solver.PrettyPrint(xs, _charsetSolver)
#endif

    member this.Optimizations = _optimizations
