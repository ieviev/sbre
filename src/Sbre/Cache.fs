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
type RegexCache< ^t
    when ^t: struct
    and ^t :> IEquatable< ^t >
    and ^t: equality>
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

    let mutable _cachedBDDRanges: Dictionary< ^t, StartsetChars > =
        Dictionary()

#if DEBUG
    let mintermsPretty =
        _solver.GetMinterms()
        |> Array.map (fun mt -> mt, _solver.PrettyPrint(mt, _charsetSolver))

    // member this.MintermsPretty = mintermsPretty
#endif



    member this.tryCommonStartset(loc: Location, set: ^t) =
        let mutable setTooBig = false

        let ranges =
            match _cachedBDDRanges.TryGetValue(set) with
            | true, ranges -> ranges
            | _ ->


                let bdd = _solver.ConvertToBDD(set, _charsetSolver)
                let rcc = RegexCharClass()

                let mutable i = 0u

                let charArray = Array.zeroCreate<char> 20

                let mutable e =
                    BDDRangeConverter.ToRanges(bdd).GetEnumerator()

                while e.MoveNext()
                      && not setTooBig do
                    let struct (rs, re) =
                        e.Current :?> struct (uint32 * uint32)

                    if
                        (i
                         + (re - rs)) > 20u
                    then
                        setTooBig <- true
                    else
                        rcc.AddRange(char rs, char re)

                    for j = int rs to int re do
                        if i >= 20u then
                            setTooBig <- true
                        else
                            charArray[int i] <- char j
                            i <- i + 1u

                let trimmed =
                    charArray.AsSpan().Slice(0, int i).ToArray()

                let startsetchars =
                    StartsetChars.Of(StartsetFlags.None, trimmed)

                _cachedBDDRanges.TryAdd(set, startsetchars)
                |> ignore

                startsetchars

        if setTooBig then
            ValueNone
        else

            let currpos = loc.Position

            match loc.Reversed with
            | false ->
                let slice = loc.Input.AsSpan().Slice(currpos)
                let setChars = ranges.Chars.AsSpan() //.Slice(0, int i)
                let sharedIndex = slice.IndexOfAny(setChars)

                if sharedIndex = -1 then
                    ValueNone
                else
                    ValueSome(
                        currpos
                        + sharedIndex
                    )
            | true ->
                let slice = loc.Input.AsSpan().Slice(0, currpos)
                let setChars = ranges.Chars.AsSpan() //.Slice(0, int i)
                let sharedIndex = slice.LastIndexOfAny(setChars)

                if sharedIndex = -1 then
                    ValueNone
                else
                    let _ =
                        Location.create
                            loc.Input
                            (sharedIndex
                             + 1)

                    ValueSome(
                        sharedIndex
                        + 1
                    )


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForLocation(loc: Location) : ^t =
        match loc.Reversed with
        | false -> minterms[classifier.GetMintermID(int loc.Input[loc.Position])]
        | true ->
            minterms[classifier.GetMintermID(
                         int
                             loc.Input[loc.Position
                                       - 1]
                     )]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForStringIndex(str: string, pos: int) : ^t =
        minterms[classifier.GetMintermID(int str[pos])]

    member this.CharToMinterm(c: inref<char>) : ^t = minterms[classifier.GetMintermID(int c)]

    member val InitialPatternWithoutDotstar = _rawPattern

    member val Solver: ISolver< ^t > = _solver
    member val CharsetSolver: CharSetSolver = _charsetSolver
    member val Builder = _builder

    // cached instantiation members
    member val True: RegexNode< ^t > =
        _builder.uniques._true
    //
    //
    member val False: RegexNode< ^t > =
        _builder.uniques._false

    //
    member val TrueStar: RegexNode< ^t > =
        _builder.uniques._trueStar


    member val FullMinterm: ^t = _solver.Full


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsTrueStar(node: RegexNode< ^t >) : bool =
        let a = 1
        obj.ReferenceEquals(node, _builder.uniques._trueStar)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsFalse(node: RegexNode< ^t >) : bool =
        obj.ReferenceEquals(node, _builder.uniques._false)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateInfo(flags, startset) : RegexNodeInfo<_> =
        Info.ofFlagsAndStartset(flags, startset)



    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsTrue(node: RegexNode< ^t >) : bool =
        obj.ReferenceEquals(node, _builder.uniques._true)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsImplicitDotStarred(node: RegexNode<'t>) : bool = obj.ReferenceEquals(node, _implicitDotstarPattern)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.GetPredicateResult(location: Location, pred: ^t) =
        let mterm = cache.MintermForLocation(location)
        cache.Solver.isElemOfSet (pred, mterm)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.IsValidPredicate(pred: ^t, locationPredicate: ^t) =
        cache.Solver.isElemOfSet (pred, locationPredicate)



#if DEBUG
    member cache.PrintNode(xs: RegexNode<'t>) =

        if obj.ReferenceEquals(xs, null) then
            "null"
        else
            xs.ToStringHelper()

    member cache.PrettyPrintMinterm(xs: ^t) = cache.Solver.PrettyPrint(xs, _charsetSolver)
#endif

    member this.Optimizations = _optimizations
