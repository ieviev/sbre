namespace Sbre

open System
open System.Collections.Generic
open System.Globalization
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Info
open Sbre.Pat
open Sbre.Types
open System.Linq


module internal StartsetHelpers =
    let bddToStartsetChars(bdd: BDD) : PredStartset =
        let rcc = RegexCharClass()
        let mutable ranges = BDDRangeConverter.ToRanges(bdd)
        let mutable e = ranges.GetEnumerator()
        let mutable setTooBig = false
        let mutable i = 0u
        let sizeLimit = 1024u
        let charArray = Array.zeroCreate<char> (int (sizeLimit))
        let totalRange = ranges |> Seq.sumBy (fun struct (rs, re) -> re - rs)

        while e.MoveNext() && not setTooBig do
            let struct (rs, re) = e.Current :?> struct (uint32 * uint32)

            if (i + (re - rs)) > sizeLimit then
                setTooBig <- true
            else
                rcc.AddRange(char rs, char re)

            for j = int rs to int re do
                if i >= sizeLimit then
                    setTooBig <- true
                else
                    charArray[int i] <- char j
                    i <- i + 1u

        let trimmed = charArray.AsSpan().Slice(0, int i).ToArray()

        let ranges2 = PredStartset.Of(StartsetFlags.None, trimmed)

        if setTooBig then
            // no optimization if startset too large
            PredStartset.Of(StartsetFlags.Inverted, [||])
        else
            ranges2

    let startsetsFromMintermArray(bdds: BDD array) : PredStartset array =
        let startsets1 = bdds[1..] |> Array.map bddToStartsetChars

        let invertedStartset0 =
            PredStartset.Of(StartsetFlags.Inverted, startsets1 |> Array.collect (fun v -> v.Chars))

        Array.append [| invertedStartset0 |] startsets1

    let static_merged_chars = Array.zeroCreate 1024 |> ResizeArray<char>

    let getMergedIndexOfSpan
        (
            predStartsetArray: Types.PredStartset array,
            uintMinterms: uint64 array,
            startset: uint64
        )
        : Span<char>
        =
        let mergedCharSpan = CollectionsMarshal.AsSpan(static_merged_chars)
        let mutable totalLen = 0

        let shouldInvert = Solver.isElemOfSetU64 startset uintMinterms[0]

        if shouldInvert then
            for i = 1 to predStartsetArray.Length - 1 do
                let pureMt = uintMinterms[i]

                match Solver.isElemOfSetU64 startset pureMt with
                | true -> ()
                | false ->
                    let targetSpan = mergedCharSpan.Slice(totalLen)
                    let pspan = predStartsetArray[i].Chars.AsSpan()
                    pspan.CopyTo(targetSpan)
                    totalLen <- totalLen + pspan.Length

            mergedCharSpan.Slice(0, totalLen)

        else
            for i = 1 to predStartsetArray.Length - 1 do
                let pureMt = uintMinterms[i]

                match Solver.isElemOfSetU64 startset pureMt with
                | true ->
#if DEBUG
                    if predStartsetArray[i].Flags.HasFlag(StartsetFlags.Inverted) then
                        failwith "TODO: optimizations"
#endif
                    let targetSpan = mergedCharSpan.Slice(totalLen)
                    let pspan = predStartsetArray[i].Chars.AsSpan()
                    pspan.CopyTo(targetSpan)
                    totalLen <- totalLen + pspan.Length
                | false -> ()

            mergedCharSpan.Slice(0, totalLen)



[<AutoOpen>]
module private BuilderHelpers =
    [<Flags>]
    type MkOrFlags =
        | None = 0uy
        | IsTrueStar = 1uy
        | ContainsEpsilon = 2uy

    [<Flags>]
    type MkAndFlags =
        | None = 0uy
        | IsFalse = 1uy
        | ContainsEpsilon = 2uy

    ()


/// reuses nodes and ensures reference equality
[<Sealed>]
type RegexBuilder<'t when ^t :> IEquatable< ^t > and ^t: equality>
    (converter: RegexNodeConverter, solver: ISolver< ^t >, bcss: CharSetSolver) as b =
    let runtimeBuilder = SymbolicRegexBuilder< ^t>(solver, bcss)

    let getDerivativeCacheComparer() : IEqualityComparer<struct (uint64 * RegexNode<uint64>)> =
        { new IEqualityComparer<struct (uint64 * RegexNode<uint64>)> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Equals(struct (x1, x2), struct (y1, y2)) = x1 = y1 && refEq x2 y2

            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.GetHashCode(struct (x, y)) =
                // LanguagePrimitives.PhysicalHash y
                int x ^^^ LanguagePrimitives.PhysicalHash y
                // int x ^^^ LanguagePrimitives.PhysicalHash y
        }

    let _andCacheComparer =
        { new IEqualityComparer<RegexNode<'t>[]> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Equals(xs, ys) =
                xs.Length = ys.Length && Array.forall2 refEq xs ys

            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.GetHashCode(x) =
                Enumerator.getSharedHash x
        }

    let _orCacheComparer =
        { new IEqualityComparer<RegexNode< ^t >[]> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Equals(xs, ys) =
                xs.Length = ys.Length && Array.forall2 refEq xs ys
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.GetHashCode(x) = Enumerator.getSharedHash x
        }


    let _refComparer =
        { new IEqualityComparer<RegexNode< ^t >> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Equals(xs, ys) = refEq xs ys
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }

    let _u64refComparer =
        { new IEqualityComparer<RegexNode<uint64>> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Equals(xs, ys) = refEq xs ys
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }

    let _singletonCache: Dictionary< ^t, RegexNode< ^t > > = Dictionary()

    let _loopCache: Dictionary<struct (RegexNode< ^t > * int * int), RegexNode< ^t >> =
        Dictionary()

    let _concatCache: Dictionary<struct (RegexNode< ^t > * RegexNode< ^t >), RegexNode< ^t >> =
        Dictionary()

    let _derivativeCache: Dictionary<struct (uint64 * RegexNode<uint64>), RegexNode<uint64>> =
        Dictionary(getDerivativeCacheComparer ())

    let _startset2Cache: Dictionary<RegexNode<uint64>, uint64> = Dictionary(_u64refComparer)


    let _subsumptionCacheComparer =
        { new IEqualityComparer<struct(RegexNode<uint64>*RegexNode<uint64>)> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Equals(struct(x1,y1), struct(x2,y2)) =
                refEq x1 x2 && refEq y1 y2
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.GetHashCode(struct(x,y)) =
                LanguagePrimitives.PhysicalHash x ^^^ LanguagePrimitives.PhysicalHash y
        }

    let _andSubsumptionCache: Dictionary<struct(RegexNode<uint64>*RegexNode<uint64>), bool> =
        Dictionary(_subsumptionCacheComparer)


    let _regexInfoCache: Dictionary<struct (RegexNodeFlags * ^t), RegexNodeInfo< ^t >> =
        Dictionary()



    let _orCache: Dictionary<RegexNode< ^t >[], RegexNode< ^t >> =
        Dictionary(_orCacheComparer)

    let _notCache: Dictionary<RegexNode< ^t >, RegexNode< ^t >> = Dictionary(_refComparer)

    let _andCache: Dictionary<RegexNode<'t>[], RegexNode<'t>> = Dictionary(_andCacheComparer)

    // singleton instances
    let _uniques = {|
        _false = RegexNode.Singleton(solver.Empty)
        _true = RegexNode.Singleton(solver.Full)
        _epsilon = RegexNode< ^t>.Epsilon
        _trueStar =
            RegexNode.Loop(
                RegexNode.Singleton(solver.Full),
                low = 0,
                up = Int32.MaxValue,
                info = {
                    Flags =
                        RegexNodeFlags.IsAlwaysNullable
                        ||| RegexNodeFlags.CanBeNullable
                        ||| RegexNodeFlags.CanSkip
                    Startset = solver.Full
                    InitialStartset = Uninitialized
                }
            )
        _truePlus =
            RegexNode.Loop(
                RegexNode.Singleton(solver.Full),
                low = 1,
                up = Int32.MaxValue,
                info = { Flags = RegexNodeFlags.None; Startset = solver.Full; InitialStartset = Uninitialized }
            )
        _wordChar = lazy b.setFromStr @"\w"
        _nonWordChar = lazy b.setFromStr @"\W"
    |}

    let _anchors =
        let __z_anchor =
            lazy RegexNode.LookAround(_uniques._true, lookBack = false, negate = true)

        let __big_a_anchor =
            lazy RegexNode.LookAround(_uniques._true, lookBack = true, negate = true)

        {|
            _zAnchor = __z_anchor
            _dollarAnchor =
                lazy
                    let info = {
                        Flags = RegexNodeFlags.CanBeNullable //||| RegexNodeFlags.ContainsLookaround
                        Startset = Unchecked.defaultof<_>
                        InitialStartset = Uninitialized
                    }

                    Or(
                        ofSeq [
                            __z_anchor.Value
                            RegexNode.LookAround(b.one '\n', lookBack = false, negate = false)
                        ],
                        info
                    )
            // \A ≡ (?<!⊤)
            _bigAAnchor = __big_a_anchor
            // (?<=\A|\A\n) ≡ \a
            _aAnchor =
                lazy
                    let info = {
                        Flags = RegexNodeFlags.CanBeNullable //||| RegexNodeFlags.ContainsLookaround
                        Startset = solver.Full
                        InitialStartset = Uninitialized
                    }

                    let seqv =
                        ofSeq [
                            __big_a_anchor.Value
                            b.mkConcat [ __big_a_anchor.Value; b.one '\n' ]
                        ]

                    RegexNode.LookAround(Or(seqv, info), lookBack = true, negate = true)


            _nonWordBorder =
                lazy
                    let info = {
                        Flags = RegexNodeFlags.CanBeNullable
                        Startset = solver.Full
                        InitialStartset = Uninitialized
                    }
                    // (?!ψ\w)
                    let c1 = [
                        RegexNode.LookAround(
                            _uniques._wordChar.Value,
                            lookBack = true,
                            negate = true
                        ) // (?<!ψ\w)
                        RegexNode.LookAround(
                            _uniques._wordChar.Value,
                            lookBack = false,
                            negate = true
                        )
                    ]
                    // (?=ψ\w)
                    let c2 = [
                        RegexNode.LookAround(
                            _uniques._wordChar.Value,
                            lookBack = true,
                            negate = false
                        ) // (?<=ψ\w)
                        RegexNode.LookAround(
                            _uniques._wordChar.Value,
                            lookBack = false,
                            negate = false
                        )
                    ]

                    Or(ofSeq [ b.mkConcat c1; b.mkConcat c2 ], info)

            _wordBorder =
                lazy
                    let info = {
                        Flags = RegexNodeFlags.CanBeNullable ||| RegexNodeFlags.ContainsLookaround
                        Startset = Unchecked.defaultof<_>
                        InitialStartset = Uninitialized
                    }

                    Or(
                        ofSeq [
                            b.mkConcat [
                                RegexNode.LookAround(
                                    _uniques._wordChar.Value,
                                    lookBack = true,
                                    negate = false
                                ) // (?<=ψ\w)
                                RegexNode.LookAround(
                                    _uniques._wordChar.Value,
                                    lookBack = false,
                                    negate = true
                                )
                            ]
                            b.mkConcat [
                                RegexNode.LookAround(
                                    _uniques._wordChar.Value,
                                    lookBack = true,
                                    negate = true
                                ) // (?<!ψ\w)
                                RegexNode.LookAround(
                                    _uniques._wordChar.Value,
                                    lookBack = false,
                                    negate = false
                                )
                            ]
                        ],
                        info
                    )

            // ^ ≡ \A|(?<=\n)
            _caretAnchor =
                lazy
                    let info = {
                        Flags =
                            RegexNodeFlags.CanBeNullable
                            ||| RegexNodeFlags.None
                        Startset = Unchecked.defaultof<_>
                        InitialStartset = Uninitialized
                    }

                    Or(
                        ofSeq [
                            __big_a_anchor.Value
                            RegexNode.LookAround(b.one '\n', lookBack = true, negate = false)
                        ],
                        info
                    )


        |}

    let mutable _prefixCache: Dictionary<RegexNode<uint64>, InitialStartset> = Dictionary(_u64refComparer)

    member this.trueStar = _uniques._trueStar
    member this.epsilon = _uniques._epsilon
    member this.uniques = _uniques
    member this.anchors = _anchors

    member this.PrefixCache = _prefixCache
    member this.DerivativeCache = _derivativeCache
    member this.Startset2Cache = _startset2Cache
    member this.AndSubsumptionCache = _andSubsumptionCache


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetPrefixCached(node: RegexNode<uint64>) =
        match this.PrefixCache.TryGetValue(node) with
        | true, v -> v
        | _ ->
            let ss2 = Startset.inferInitialStartset (solver :?> ISolver<uint64>) (node)
            this.PrefixCache.Add(node, ss2)
            ss2



    member this.one(char: char) : RegexNode< ^t > =
        let a1: BDD = bcss.CreateBDDFromChar char
        let a2 = solver.ConvertFromBDD(a1, bcss)

        if solver.IsFull(a2) then
            _uniques._true
        elif solver.IsEmpty(a2) then
            _uniques._false
        else
            match _singletonCache.TryGetValue(a2) with
            | true, v -> v
            | _ ->
                let v = RegexNode.Singleton(a2)
                _singletonCache.Add(a2, v)
                v


    member this.one(minterm: ^t) : RegexNode< ^t > =
        let a2 = minterm

        if solver.IsFull(a2) then
            _uniques._true
        elif solver.IsEmpty(a2) then
            _uniques._false
        else
            match _singletonCache.TryGetValue(a2) with
            | true, v -> v
            | _ ->
                let v = RegexNode.Singleton(a2)
                _singletonCache.Add(a2, v)
                v



    member this.notOne(char: char) =
        let a1: BDD = bcss.CreateBDDFromChar char
        let a2 = solver.Not(solver.ConvertFromBDD(a1, bcss))

        match _singletonCache.TryGetValue(a2) with
        | true, v -> v
        | _ ->
            let v = RegexNode.Singleton(a2)
            _singletonCache.Add(a2, v)
            v


    member this.trySubsumeOr(nodes: HashSet<RegexNode< ^t >>) : RegexNode< ^t >seq =

        let starLoops =
            nodes
            |> Seq.choose (fun v ->
                match v with
                | SingletonStarLoop(pred) -> Some struct(pred, v)
                | _ -> None
            )
            |> Seq.toArray

        match starLoops with
        | [||] -> nodes
        | _ ->

            let struct(largestPred,largestStarLoop) =
                starLoops
                |> Seq.reduce (fun struct(e1,e1node) struct(e2,e2node) ->
                    let conj = solver.And(e1, e2)
                    let iselem = not (solver.IsEmpty(conj))

                    if conj = e1 then (struct(e2,e2node))
                    elif conj = e2 then (struct(e1,e1node))
                    else (solver.Empty,Unchecked.defaultof<_>)
                )

            if solver.IsEmpty(largestPred) then nodes else

            let mutable e = nodes.GetEnumerator()

            while e.MoveNext() do
                let curr = e.Current
                if loopSubsumesBranch solver largestPred curr then
                    nodes.Remove(curr) |> ignore

            nodes.Add(largestStarLoop) |> ignore
            nodes



    member this.trySubsumeAnd(nodes: RegexNode< ^t >seq) : RegexNode< ^t >seq =
        nodes
        |> Seq.indexed
        |> Seq.tryPick (fun (idx, v) ->
            match v with
            | SingletonStarLoop(pred) ->
                if isSubsumedFromAnd solver pred v nodes then
                    Some(nodes |> Seq.removeAt idx)
                else None
            | _ -> None
        )
        |> Option.defaultValue nodes


    member this.trySubsumeTopLevelOr(first, deriv) : bool =

        match first, deriv with
        | Concat(_), And(nodes=nodes2) ->
            nodes2.Contains(first)

        | Or(nodes=nodes1), And(nodes=nodes2) ->
            if nodes2.Contains(first) then true else
            let mutable found = false
            use mutable n1e = nodes1.GetEnumerator()
            while not found && n1e.MoveNext()  do
                if nodes2.Contains(n1e.Current) then
                    found <- true
            found

        | And(nodes=nodes1), And(nodes=nodes2) | Or(nodes=nodes2), Or(nodes=nodes1)  ->

            let mutable found = false

            // check for subset equality
            use mutable n1e = nodes1.GetEnumerator()
            use mutable n2e = nodes2.GetEnumerator()
            let mutable allcontained = true
            while allcontained && n1e.MoveNext()  do

                let mutable currFound = false
                while not currFound && n2e.MoveNext() do
                    let currN2 = n2e.Current
                    let currN1 = n1e.Current
                    if refEq currN1 currN2 then
                        currFound <- true
                    else

                    match currN1 with
                    | Or(nodes=currN1Inner) ->
                        if currN1Inner.Contains(currN2) then
                            currFound <- true
                    | _ -> ()

                n2e.Reset()
                if not currFound then
                    allcontained <- false
            if allcontained then
                found <- true
#if OPTIMIZE
            else
                failwith "unoptimized"
#endif


            _andSubsumptionCache.TryAdd(struct(first,deriv),found) |> ignore
            found

        | _ -> false



    member this.mkAnd(nodeSet: RegexNode< ^t >[]) : RegexNode< ^t > =
        let nodeSet =
            // filter and expand!
            nodeSet
            |> Seq.collect (fun v ->
                match v with
                | _ when obj.ReferenceEquals(v, _uniques._trueStar) -> [||]
                | And(nodes, _) -> nodes |> Seq.toArray
                | n -> [| n |]
            )
            |> Seq.toArray

        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash nodeSet

        if nodeSet.Length = 0 then
            _uniques._trueStar
        else

            let createNode(nodeSet: RegexNode< ^t >[]) =
                nodeSet
                |> Array.tryFind (fun v -> obj.ReferenceEquals(v, _uniques._false))
                |> function
                    | Some falseNode -> falseNode
                    | _ ->
                        let nodes = nodeSet

                        match nodes with
                        | _ when nodes.Length = 0 -> _uniques._trueStar
                        | _ when nodes.Length = 1 -> (head nodes)
                        | twoormore ->
                            let flags = Flags.inferAnd twoormore
                            let startset = twoormore |> Startset.inferMergeStartset solver
                            let mergedInfo = { Flags = flags; Startset = startset; InitialStartset = Uninitialized }
                            let newAnd = RegexNode.And(ofSeq twoormore, mergedInfo)
                            newAnd

            let key = nodeSet

            match _andCache.TryGetValue(key) with
            | true, v -> v
            | _ ->
                let v = createNode key
                _andCache.Add(key, v)
                v


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.mkAndEnumerator
        (
            e: byref<Collections.Immutable.ImmutableHashSet<RegexNode<'t>>.Enumerator>,
            mkDer: RegexNode<'t> -> RegexNode<'t>
        ) : RegexNode<'t> =

        let mutable enumerating = true
        let mutable status = MkAndFlags.None
        let derivatives = ResizeArray()

        while e.MoveNext() = true && enumerating do
            let curr = e.Current
            let deriv = mkDer curr

            let rec handleNode (deriv) =
                match deriv with
                | _ when obj.ReferenceEquals(deriv, _uniques._trueStar) -> ()
                | _ when obj.ReferenceEquals(deriv, _uniques._false) ->
                    enumerating <- false
                    status <- MkAndFlags.IsFalse
                | And(nodes, _) -> for node in nodes do handleNode node

                // todo: careful in case of epsilon here
                // ~(.*) -> always false (?)
                | Not(node,info) when info.CanNotBeNullable && info.ContainsEpsilon ->
                    enumerating <- false
                    status <- MkAndFlags.IsFalse
                | Epsilon ->
                    status <- MkAndFlags.ContainsEpsilon
                    derivatives.Add(deriv)

                | _ -> derivatives.Add(deriv)

            handleNode deriv


        match status with
        | MkAndFlags.IsFalse -> _uniques._false
        | MkAndFlags.ContainsEpsilon when derivatives.Exists(fun v -> v.CanNotBeNullable) ->
            _uniques._false
        | _ ->
        if derivatives.Count = 1 then derivatives[0] else

        let createNode(nodes: RegexNode<_>[]) =
            match nodes with
            | _ when nodes.Length = 0 -> _uniques._trueStar
            | _ when nodes.Length = 1 -> nodes[0]
            | twoormore ->
                let flags = Flags.inferAnd twoormore
                let startset = twoormore |> Startset.inferMergeStartset (solver)
                let mergedInfo = { Flags = flags; Startset = startset ; InitialStartset = Uninitialized }
                let newAnd = RegexNode.And(ofSeq twoormore, mergedInfo)
                newAnd

        //
        let asArray =
            derivatives
            |> this.trySubsumeAnd
            |> Seq.toArray

        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash asArray

        match _andCache.TryGetValue(asArray) with
        | true, v -> v
        | _ ->
            let v = createNode (asArray)
            _andCache.Add(asArray, v)
            v

    member this.mkOr(nodeSet: RegexNode< _ >[]) : RegexNode< _ > =

        let mutable enumerating = true
        let mutable status = MkOrFlags.None
        let mutable zeroloops = 0
        let mutable singletonLoops = 0
        let mutable e = nodeSet.AsSpan().GetEnumerator()

        let derivatives = HashSet(_refComparer)

        while e.MoveNext() = true && enumerating do
            let deriv = e.Current

            let rec handleNode (deriv:RegexNode<'t>) =
                match deriv with
                | _ when obj.ReferenceEquals(deriv, _uniques._false) -> ()
                | _ when obj.ReferenceEquals(deriv, _uniques._trueStar) ->
                    enumerating <- false
                    status <- MkOrFlags.IsTrueStar
                | Or(nodes, _) -> nodes |> Seq.iter handleNode
                | Concat(head=Loop(low=0;up=upper)) when upper <> Int32.MaxValue ->
                    zeroloops <- zeroloops + 1
                    derivatives.Add(deriv) |> ignore
                | Loop(node=Singleton body; low=0; up = Int32.MaxValue) ->
                    singletonLoops <- singletonLoops + 1
                    derivatives.Add(deriv) |> ignore
                // todo: eat epsilon
                | Epsilon ->
                    status <- status ||| MkOrFlags.ContainsEpsilon
                | _ -> derivatives.Add(deriv) |> ignore

            handleNode deriv

        match status with
        | MkOrFlags.IsTrueStar -> _uniques._trueStar
        | _ ->

        if zeroloops > 1 then
            // remove loop duplicates
            // C{0,9}D  | C{0,8}D = C{0,9}D
            derivatives
            |> Seq.choose (fun v ->
                match v with
                | Concat(head=Loop(low=0;up=upper;node=body);tail=tail) when upper < Int32.MaxValue ->
                    Some(struct(body,tail),upper,v)
                | _ -> None
            )
            |> Seq.groupBy (fun (uniq,upper,v) -> uniq )
            |> Seq.iter (fun (uniq,nodes) ->
                nodes
                |> Seq.sortByDescending (fun (body,upper,v) -> upper )
                |> Seq.skip 1
                |> Seq.iter (fun (body,upper,v) -> derivatives.Remove(v) |> ignore )
            )
        // add epsilon only if no nullables yet
        if (status &&& MkOrFlags.ContainsEpsilon) <> MkOrFlags.None then
            if derivatives.Any(fun v -> v.IsAlwaysNullable) then ()
            else derivatives.Add(Epsilon) |> ignore

        let nodeSet =
            derivatives
            |> this.trySubsumeOr
            |> Seq.toArray
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash nodeSet

        if nodeSet.Length = 0 then
            _uniques._false
        else
            let createNode(nodes: RegexNode< ^t >[]) =
                match nodes with
                | _ when nodes.Length = 0 -> _uniques._false
                | _ when nodes.Length = 1 -> (head nodes)
                | twoormore ->
                    let flags = Flags.inferOr twoormore

                    let startset = twoormore |> Startset.inferMergeStartset solver

                    let mergedInfo = { Flags = flags; Startset = startset ; InitialStartset = Uninitialized }
                    RegexNode.Or(ofSeq twoormore, mergedInfo)

            let key = nodeSet

            match _orCache.TryGetValue(key) with
            | true, v -> v
            | _ ->
                let v = createNode key
                _orCache.Add(key, v)
                v


    member this.mkOrEnumerator(
        e: byref<Collections.Immutable.ImmutableHashSet<RegexNode<'t>>.Enumerator>,
        mkDer: (RegexNode<'t> -> RegexNode<'t>)

        ) : RegexNode< _ > =

        let mutable enumerating = true
        let mutable status = MkOrFlags.None
        let mutable zeroloops = 0
        let mutable singletonLoops = 0

        let derivatives = HashSet(_refComparer) //this.DerivativeSet

        while e.MoveNext() && enumerating do
            // let deriv = e.Current
            let deriv = mkDer e.Current

            let rec handleNode (deriv:RegexNode<'t>) =
                match deriv with
                | _ when obj.ReferenceEquals(deriv, _uniques._false) -> ()
                | _ when obj.ReferenceEquals(deriv, _uniques._trueStar) ->
                    enumerating <- false
                    status <- MkOrFlags.IsTrueStar
                | Or(nodes, _) -> nodes |> Seq.iter handleNode
                | Concat(head=Loop(low=0;up=upper)) when upper <> Int32.MaxValue ->
                    zeroloops <- zeroloops + 1
                    derivatives.Add(deriv) |> ignore
                | Loop(node=Singleton body; low=0; up = Int32.MaxValue) ->
                    singletonLoops <- singletonLoops + 1
                    derivatives.Add(deriv) |> ignore
                // todo: eat epsilon
                | Epsilon ->
                    status <- status ||| MkOrFlags.ContainsEpsilon
                | _ -> derivatives.Add(deriv) |> ignore

            handleNode deriv

        match status with
        | MkOrFlags.IsTrueStar -> _uniques._trueStar
        // | _ when derivatives.Count = 1 -> derivatives |> Seq.head
        | _ ->

        if zeroloops > 1 then
            // remove loop duplicates
            // C{0,9}D  | C{0,8}D = C{0,9}D
            derivatives
            |> Seq.choose (fun v ->
                match v with
                | Concat(head=Loop(low=0;up=upper;node=body);tail=tail) when upper < Int32.MaxValue ->
                    Some(struct(body,tail),upper,v)
                | _ -> None
            )
            |> Seq.groupBy (fun (uniq,upper,v) -> uniq )
            |> Seq.iter (fun (uniq,nodes) ->
                nodes
                |> Seq.sortByDescending (fun (body,upper,v) -> upper )
                |> Seq.skip 1
                |> Seq.iter (fun (body,upper,v) -> derivatives.Remove(v) |> ignore )
            )
        // add epsilon only if no nullables yet
        if (status &&& MkOrFlags.ContainsEpsilon) <> MkOrFlags.None then
            if derivatives.Any(fun v -> v.IsAlwaysNullable) then ()
            else derivatives.Add(Epsilon) |> ignore

        let nodeSet =
            derivatives
            |> this.trySubsumeOr
            |> Seq.toArray
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash nodeSet

        if nodeSet.Length = 0 then
            _uniques._false
        else
            let createNode(nodes: RegexNode< ^t >[]) =
                match nodes with
                | _ when nodes.Length = 0 -> _uniques._false
                | _ when nodes.Length = 1 -> (head nodes)
                | twoormore ->
                    let flags = Flags.inferOr twoormore

                    let startset = twoormore |> Startset.inferMergeStartset solver

                    let mergedInfo = { Flags = flags; Startset = startset ; InitialStartset = Uninitialized }
                    RegexNode.Or(ofSeq twoormore, mergedInfo)
            let key = nodeSet
            match _orCache.TryGetValue(key) with
            | true, v -> v
            | _ ->
                let v = createNode key
                _orCache.Add(key, v)
                v

    member this.mkNot(inner: RegexNode< ^t >) =
        // short-circuit
        if inner.IsAlwaysNullable && inner.ContainsEpsilon then _uniques._false else

        let createNode(inner: RegexNode< ^t >) =
            match inner with
            | _ when refEq _uniques._false inner -> _uniques._trueStar // ~(⊥) -> ⊤*
            | _ when refEq _uniques._trueStar inner -> _uniques._false // ~(⊤*) -> ⊥
            | Epsilon -> _uniques._truePlus // ~(ε) -> ⊤+
            | _ ->
                let mutable flags = Flags.inferNode inner
                if flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) then
                    removeFlag &flags RegexNodeFlags.CanBeNullable
                    removeFlag &flags RegexNodeFlags.IsAlwaysNullable
                else if not (flags.HasFlag(RegexNodeFlags.CanBeNullable)) then
                    addFlag &flags RegexNodeFlags.CanBeNullable
                    addFlag &flags RegexNodeFlags.IsAlwaysNullable

                Not(inner, this.CreateInfo(flags, inner.Startset))

        let key = inner
        match _notCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let v = createNode key
            _notCache.Add(key, v)
            v

    member this.setFromStr(setPattern: string) =
        let tree =
            RegexParser.Parse( setPattern, RegexOptions.ExplicitCapture, CultureInfo.InvariantCulture )

        let setStr = tree.Root.Child(0).Str
        let bdd = converter.CreateBDDFromSetString(setStr)
        let converted = solver.ConvertFromBDD(bdd, bcss)
        RegexNode.Singleton(converted)

    member this.bddFromSetString(setPattern: string) = converter.CreateBDDFromSetString(setPattern)
    member this.setFromNode(node: RegexNode) =
        RegexNode.Singleton(converter.CreateBDDFromSetString(node.Str))

    member this.mkConcat2(head: RegexNode< ^t >, tail: RegexNode< ^t >) : RegexNode< ^t > =
        let key = struct (head, tail)
        match _concatCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let v =
                match head with
                | Epsilon -> tail // ()R -> R
                | _ when refEq head _uniques._false -> _uniques._false // ⊥R -> ⊥
                | _ ->
                    let startset = Startset.inferConcatStartset solver head tail

                    let flags = Flags.inferConcat head tail
                    let info = this.CreateInfo(flags, startset)
                    Concat(head, tail, info)
            _concatCache.Add(key, v)
            v


    member this.mkConcat(nodesCorrectOrder: RegexNode< ^t > list) : RegexNode< ^t > =

        match nodesCorrectOrder with
        | [] -> Epsilon
        | [ x ] -> x
        | [ head; tail ] -> this.mkConcat2 (head, tail)
        | rest ->
            let combined =
                rest
                |> Seq.rev
                |> Seq.fold
                    (fun acc v ->
                        match acc with
                        | Epsilon -> v
                        | _ ->
                            let flags' = Flags.inferConcat v acc
                            let startset' = Startset.inferConcatStartset solver v acc
                            Concat(v, acc, this.CreateInfo(flags', startset'))
                    )
                    Epsilon

            combined

    member this.mkLoop(body: RegexNode< ^t >, lower: int, upper: int) =
        let createNode(struct (body: RegexNode< ^t >, lower: int, upper: int)) =
            match body, struct (lower, upper) with
            | _, LoopKind LoopKind.EmptyLoop -> _uniques._epsilon
            | _, LoopKind LoopKind.Single -> body
            | Singleton minterm, LoopKind LoopKind.Plus when solver.IsFull(minterm) ->
                _uniques._trueStar
            | Singleton minterm, LoopKind LoopKind.Star ->
                if solver.IsFull(minterm) then
                    _uniques._trueStar
                else
                    let flags = Flags.inferLoop (body, lower, upper)
                    let startset = Startset.inferLoopStartset solver (body, lower, upper)
                    RegexNode.Loop(body, lower, upper, info = b.CreateInfo(flags, startset))


            | _, (x, y) when x > 0 ->
                let flags = Flags.inferLoop (body, lower, upper)
                let startset = Startset.inferLoopStartset solver (body, lower, upper)
                let info = b.CreateInfo(flags, startset)
                RegexNode.Loop(body, lower, upper, info = info)


            | _ ->
                let flags = Flags.inferLoop (body, lower, upper)
                let startset = Startset.inferLoopStartset solver (body, lower, upper)
                let info = b.CreateInfo(flags, startset)
                RegexNode.Loop(body, lower, upper, info = info)

        let key = struct (body, lower, upper)

        match _loopCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let v = createNode key
            _loopCache.Add(key, v)
            v

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateInfo(flags, startset) : RegexNodeInfo<_> =
        match _regexInfoCache.TryGetValue(struct (flags, startset)) with
        | true, v -> v
        | _ ->
            let v = { Flags = flags; Startset = startset; InitialStartset = Uninitialized }
            _regexInfoCache.Add(struct (flags, startset), v)
            v


// trivia:
// for correct .NET semantics
// https://github.com/dotnet/runtime/blob/1fe9c0bba15e23b65be007ddf38c43d28b2f9dd2/src/libraries/System.Text.RegularExpressions/src/System/Text/RegularExpressions/Symbolic/UnicodeCategoryConditions.cs#L67
// member this.wordCharForWordBorder = this.setFromStr "[\w\u200C\u200D]"
