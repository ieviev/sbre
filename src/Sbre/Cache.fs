namespace Sbre

open System
open System.Buffers
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open Sbre.Types
open Sbre.Patterns
open Info

#nowarn "25" // missing patterns inference

type impl = MethodImplAttribute
type implOpts = MethodImplOptions




type SingletonCache<^t when ^t:struct and ^t :> IComparable and ^t :> IComparable< ^t > and ^t :> IEquatable< ^t > and ^t: comparison>(
    charsetSolver:CharSetSolver,solver:ISolver<^t>)  =

    let _singletonLoopCache = Dictionary<BDD, RegexNode<BDD>>()
    let _true'  = RegexNode.Singleton(solver.Full)
    let _trueList = [_true']
    let truestarInfo= { Flags = RegexNodeFlags.IsAlwaysNullable ||| RegexNodeFlags.CanBeNullable ||| RegexNodeFlags.CanSkip; Startset = solver.Full }
    let trueplusInfo= { Flags = RegexNodeFlags.None; Startset = solver.Full }
    let _trueStar =
            RegexNode.Loop([_true'],0,Int32.MaxValue,truestarInfo)
    let _truePlus = RegexNode.Loop([_true'],1,Int32.MaxValue,trueplusInfo)
    let _trueStarList = [ _trueStar ]
    let _truePlusList = [ _truePlus ]
    let _false' = RegexNode.Singleton(solver.Empty)
    let _falseList = [ _false' ]
    member this.true'  = _true'
    member this.trueList = _trueList
    member this.trueStar = _trueStar
    member this.trueStarList = _trueStarList
    member this.truePlus = _truePlus
    member this.truePlusList = _truePlusList
    member this.false' = _false'
    member this.falseList = _falseList
    // member this.minterms =
    //     // 1 2 4 8 16
    //     // a b c d e
    //     solver.GetMinterms() |> Array.map (fun v -> v, RegexNode.Singleton(v)) |> dict

    member this.tryCache (bddNode: RegexNode<BDD>) =
        match bddNode with
        | Loop ([Singleton pred], 0, Int32.MaxValue, info) ->
            if pred = charsetSolver.Full then ValueSome this.trueStar
            else ValueNone

        | And _ -> ValueNone
        | Singleton pred ->
            let minterm = solver.ConvertFromBDD(pred, charsetSolver)
            ValueSome (RegexNode.Singleton(minterm))
        | _ ->
            ValueNone




[<Sealed>]
type RegexCache< ^t when
    ^t:struct
           and ^t :> IComparable
           and ^t :> IComparable< ^t >
           and ^t :> IEquatable< ^t >
           and ^t: comparison
    >
    (_solver: ISolver< ^t >,
     _charsetSolver: CharSetSolver,
     _implicitDotstarPattern: RegexNode<'t> list,
     _singletonCache: SingletonCache< ^t>,
     _optimizations: RegexFindOptimizations) =

    let classifier =
        match box _solver with
        | :? UInt64Solver as v -> v._classifier
        | :? BitVectorSolver as v -> v._classifier
        | _ -> failwith $"unknown solver of type {_solver}"

    let _tagReader =
        match box _solver with
        | :? UInt64Solver -> FSharpValue.PreComputeUnionTagReader(typeof<RegexNode<uint64>>, System.Reflection.BindingFlags.Public)
        | :? BitVectorSolver -> FSharpValue.PreComputeUnionTagReader(typeof<RegexNode<BDD>>, System.Reflection.BindingFlags.Public)
        | _ -> failwith $"unknown solver of type {_solver}"
    let minterms: ^t[] = _solver.GetMinterms()
    let mutable _cachedBDDRanges : Dictionary<^t,StartsetChars> = Dictionary()

#if DEBUG
    let mintermsPretty =
        _solver.GetMinterms()
        |> Array.map (fun mt -> mt, _solver.PrettyPrint(mt, _charsetSolver))
    member this.MintermsPretty = mintermsPretty
#endif



    member val initialMintermsArray : ^t[] = [||] with get, set
    member val InitialStartsetCharsArray : StartsetChars[] = [||] with get, set

    member this.tryCommonStartset (loc:Location,set: ^t) =
        let mutable setTooBig = false
        let ranges =
            match _cachedBDDRanges.TryGetValue(set) with
            | true, ranges -> ranges
            | _ ->


            let bdd = _solver.ConvertToBDD(set, _charsetSolver)
            let rcc = RegexCharClass()

            let mutable i = 0u

            let charArray = Array.zeroCreate<char> 20

            let mutable e = BDDRangeConverter.ToRanges(bdd).GetEnumerator()

            while e.MoveNext() && not setTooBig do
                let struct (rs,re) = e.Current :?> struct(uint32*uint32)

                if (i + (re - rs)) > 20u then setTooBig <- true
                else
                    rcc.AddRange(char rs, char re)
                for j = int rs to int re do
                    if i >= 20u then setTooBig <- true
                    else
                    charArray[int i] <- char j
                    i <- i + 1u
            let trimmed = charArray.AsSpan().Slice(0, int i).ToArray()
            let startsetchars = StartsetChars.Of(StartsetFlags.None,trimmed)
            _cachedBDDRanges.TryAdd(set,startsetchars) |> ignore
            startsetchars

        if setTooBig then ValueNone
        else

            let currpos = loc.Position

            match loc.Reversed with
            | false ->
                let slice = loc.Input.AsSpan().Slice(currpos)
                let setChars = ranges.Chars.AsSpan()//.Slice(0, int i)
                let sharedIndex = slice.IndexOfAny(setChars)
                if sharedIndex = -1 then ValueNone
                else
                ValueSome (currpos + sharedIndex)
            | true ->
                let slice = loc.Input.AsSpan().Slice(0, currpos)
                let setChars = ranges.Chars.AsSpan() //.Slice(0, int i)
                let sharedIndex = slice.LastIndexOfAny(setChars)
                if sharedIndex = -1 then ValueNone
                else
                    let _ =
                        Location.create loc.Input (sharedIndex + 1)
                    ValueSome (sharedIndex + 1)



    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForLocation(loc: Location) : ^t =
        match loc.Reversed with
        | false -> minterms[classifier.GetMintermID(int loc.Input[loc.Position])]
        | true -> minterms[classifier.GetMintermID(int loc.Input[loc.Position - 1])]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.MintermForStringIndex(str: string, pos:int) : ^t =
        minterms[classifier.GetMintermID(int str[pos])]

    member this.CharToMinterm(c:inref<char>) : ^t =
        minterms[classifier.GetMintermID(int c)]

    member val InitialPatternWithoutDotstar = _implicitDotstarPattern.Tail

    member val Solver: ISolver< ^t > = _solver
    member val CharsetSolver: CharSetSolver = _charsetSolver
    member val SingletonCache = _singletonCache

    // cached instantiation members
    member val True: RegexNode< ^t > = _singletonCache.true'
    member val TrueList: RegexNode< ^t > list = _singletonCache.trueList
    member val TruePlusList: RegexNode< ^t > list = _singletonCache.trueList
    member val False: RegexNode< ^t > = _singletonCache.false'
    member val FalseList: RegexNode< ^t > list = _singletonCache.falseList
    member val AnyStar: RegexNode< ^t > = _singletonCache.trueStar
    member val AnyStarList: RegexNode< ^t > list = _singletonCache.trueStarList
    member val FullMinterm: ^t = _solver.Full


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsAnyStar(node: RegexNode< ^t >) : bool = obj.ReferenceEquals(node, _singletonCache.trueStar)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsAnyStarList(node: RegexNode< ^t > list) : bool =
        match node with
        | [ single ] -> obj.ReferenceEquals(single, _singletonCache.trueStar)
        | _ -> false

        // obj.ReferenceEquals(node, _singletonCache.trueStarList)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsFalse(node: RegexNode< ^t >) : bool = obj.ReferenceEquals(node, _singletonCache.false')

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsFalseList(node: RegexNode< ^t > list) : bool =
        match node with
        | [single] -> obj.ReferenceEquals(single, _singletonCache.false')
        | _ -> false


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsTrue(node: RegexNode< ^t >) : bool = obj.ReferenceEquals(node, _singletonCache.true')

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsTrueList(node: RegexNode< ^t > list) : bool = obj.ReferenceEquals(node, _singletonCache.trueList)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.IsImplicitDotStarred(node: RegexNode<'t> list) : bool =
        obj.ReferenceEquals(node, _implicitDotstarPattern)



    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.GetPredicateResult(location: Location, pred: ^t) =
        let mterm = cache.MintermForLocation(location)
        cache.Solver.isElemOfSet (pred, mterm)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.IsValidPredicate(pred: ^t, locationPredicate: ^t) =
        cache.Solver.isElemOfSet (pred, locationPredicate)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.IsStartSetPredicate(locationPredicate: ^t) =
        cache.Solver.isElemOfSet (cache.initialMintermsArray[0], locationPredicate)


    /// proper equality check for 2 nodes
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.CheckSingleEquality(x1: RegexNode<'t>, x2: RegexNode<'t>) =
        match x1, x2 with
        | And(xs1,info), And(xs2,info2) ->
            info = info2 &&
            if not (isNull xs1) && not (isNull xs2) then Seq.zip xs1 xs2 |> Seq.forall cache.CheckEquality
            else isNull xs1 && isNull xs2

        | Or(xs1,info), Or(xs2,info2) ->
            info = info2 &&
            if not (isNull xs1) && not (isNull xs2) then Seq.zip xs1 xs2 |> Seq.forall cache.CheckEquality
            else isNull xs1 && isNull xs2

        | Not(x1,info), Not(x2,info2) -> cache.CheckEquality(x1, x2)
        | Singleton(x1), Singleton(x2) -> x1 = x2
        | Loop(x11,x12,x13,info), Loop(x21,x22,x23,info2) -> x12 = x22 && x13 = x23 && cache.CheckEquality(x11, x21)
        | LookAround(x11,x12,x13), LookAround(x21,x22,x23) -> x12 = x22 && x13 = x23 && cache.CheckEquality(x11, x21)
        | _ -> false


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member cache.CheckEquality(x1: RegexNode<'t> list, x2: RegexNode<'t> list) =
        obj.ReferenceEquals(x1, x2) ||
        match x1, x2 with
        | [], [] -> true
        | _, [] | [], _ -> false
        | head1::_, head2::_ when _tagReader head1 <> _tagReader head2 -> false


        | And(xs1,info)::tail1, And(xs2,info2)::tail2 ->
            let inline e3() =
                isNull xs1 = isNull xs2
                && Seq.zip xs1 xs2 |> Seq.forall cache.CheckEquality
                && cache.CheckEquality(tail1, tail2)
            e3()
        | head1::tail1, head2::tail2 ->
            cache.CheckSingleEquality(head1,head2) && cache.CheckEquality(tail1, tail2)

     member cache.defaultEqualityComparer =
        { new IEqualityComparer<RegexNode<'t> list> with
                member this.Equals(x, y) = cache.CheckEquality(x, y)
                // this is to always collide. could be skipped entirely since we only care about duplicates
                member this.GetHashCode(x) =
                    match x with
                    | [] -> 0 // epsilon
                    | _ -> ((_tagReader x.Head) + 1 ) * 256
            }



#if DEBUG
    member cache.PrintNodeList(xs: RegexNode<'t> list) =

        if obj.ReferenceEquals(xs, null) then
            "null"
        else
        xs
        |> List.map (fun v -> v.ToStringHelper())
        |> String.concat ""

    member cache.PrettyPrintMinterm(xs: ^t) =
        cache.Solver.PrettyPrint(xs,_charsetSolver)
#endif

    member this.Optimizations = _optimizations



