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


// TODO: proper startset optimization
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
            uintMinterms: 't array,
            solver: ISolver<'t>,
            startset: 't
        )
        : Span<char>
        =
        let mergedCharSpan = CollectionsMarshal.AsSpan(static_merged_chars)
        let mutable totalLen = 0

        let shouldInvert =
            solver.isElemOfSet (startset, uintMinterms[0])

        if shouldInvert then
            for i = 1 to predStartsetArray.Length - 1 do
                let pureMt = uintMinterms[i]
                match solver.isElemOfSet (startset, pureMt) with
                | true -> ()
                | false ->
                    let targetSpan = mergedCharSpan.Slice(totalLen)
                    let pspan = predStartsetArray[i].Chars.AsSpan()
                    pspan.CopyTo(targetSpan)
                    totalLen <- totalLen + pspan.Length

            mergedCharSpan.Slice(0,totalLen)

        else
            for i = 1 to predStartsetArray.Length - 1 do
                let pureMt = uintMinterms[i]
                match solver.isElemOfSet (startset, pureMt) with
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
            mergedCharSpan.Slice(0,totalLen)



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
                HashCode.Combine(x.GetHashCode(), LanguagePrimitives.PhysicalHash y)
        }

    let _andCacheComparer =
        { new IEqualityComparer<RegexNode< ^t >[]> with
            member this.Equals(xs, ys) =
                let inline eq2() = Array.forall2 refEq xs ys
                xs.Length = ys.Length && eq2 ()

            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.GetHashCode(x) =
                let hashes =
                    x
                    |> Seq.map LanguagePrimitives.PhysicalHash
                    |> Seq.reduce (fun a b -> HashCode.Combine(a, b))

                hashes
        }
    let _orCacheComparer =
        { new IEqualityComparer<RegexNode< ^t >[]> with
            member this.Equals(xs, ys) =
                xs.Length = ys.Length && Array.forall2 refEq xs ys

            member this.GetHashCode(x) =
                x
                |> Seq.map LanguagePrimitives.PhysicalHash
                |> Seq.reduce (fun a b -> HashCode.Combine(a, b))
        }


    let _refComparer =
        { new IEqualityComparer<RegexNode< ^t >> with
            member this.Equals(xs, ys) = refEq xs ys
            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }
    let _u64refComparer =
        { new IEqualityComparer<RegexNode<uint64>> with
            member this.Equals(xs, ys) = refEq xs ys
            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }

    let _singletonCache: Dictionary< ^t , RegexNode< ^t >>  = Dictionary()
    let _loopCache: Dictionary<struct (RegexNode< ^t > * int * int), RegexNode< ^t >> =
        Dictionary()
    let _concatCache: Dictionary<struct (RegexNode< ^t > * RegexNode< ^t >), RegexNode< ^t >> =
        Dictionary()
    let _derivativeCache: Dictionary<struct (uint64 * RegexNode<uint64>), RegexNode<uint64>> =
        // Dictionary(getDerivativeCacheComparer ())
        Dictionary(getDerivativeCacheComparer ())

    let _startset2Cache: Dictionary<RegexNode<uint64>, uint64> =
        // Dictionary()
        Dictionary(_u64refComparer)



    let _orCache: Dictionary<RegexNode< ^t >[], RegexNode< ^t >> =
        Dictionary(_orCacheComparer)



    let _notCache: Dictionary<RegexNode< ^t >, RegexNode< ^t >> = Dictionary(_refComparer)



    let _andCache: Dictionary<RegexNode< ^t >[], RegexNode< ^t >> =
        Dictionary(_andCacheComparer)

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
                }
            )
        _truePlus =
            RegexNode.Loop(
                RegexNode.Singleton(solver.Full),
                low = 1,
                up = Int32.MaxValue,
                info = { Flags = RegexNodeFlags.None; Startset = solver.Full }
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
                        Startset = Unchecked.defaultof<_>
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
                        Startset = Unchecked.defaultof<_>
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
                            // ||| RegexNodeFlags.ContainsLookaround
                            ||| RegexNodeFlags.None
                        Startset = Unchecked.defaultof<_>
                    }

                    Or(
                        ofSeq [
                            __big_a_anchor.Value
                            RegexNode.LookAround(b.one '\n', lookBack = true, negate = false)
                        ],
                        info
                    )


        |}



    member this.trueStar = _uniques._trueStar
    member this.epsilon = _uniques._epsilon
    // member this.true' = _uniques._true
    // member this.false' = _uniques._false
    member this.uniques = _uniques
    member this.anchors = _anchors

    member this.DerivativeCache = _derivativeCache
    member this.Startset2Cache = _startset2Cache

    // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetSs2Cached(node:RegexNode<uint64>) =
        match this.Startset2Cache.TryGetValue(node) with
        | true, v -> v
        | _ ->
            let ss2 = Startset.inferStartset2(solver :?> ISolver<uint64>)(node)
            this.Startset2Cache.Add(node,ss2)
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


    member this.trySubsumeOr(nodes: RegexNode< ^t >[]) : RegexNode< ^t >[] =

        let starLoops =
            nodes
            |> Seq.choose (fun v ->
                match v with
                | SingletonStarLoop(pred) -> Some pred
                | _ -> None
            )
            |> Seq.toArray


        match starLoops with
        | [||] -> nodes
        | _ ->

            let largestStarLoop =
                starLoops
                |> Seq.reduce (fun e1 e2 ->
                    let conj = solver.And(e1, e2)
                    let iselem = not (solver.IsEmpty(conj))

                    if conj = e1 then e2
                    elif conj = e2 then e1
                    else solver.Empty
                )

            nodes |> Seq.where (loopSubsumesBranch solver largestStarLoop >> not) |> Seq.toArray


    member this.trySubsumeAnd(nodes: RegexNode< ^t >[]) : RegexNode< ^t >[] =
        nodes
        |> Seq.indexed
        |> Seq.tryPick (fun (idx, v) ->
            match v with
            | SingletonStarLoop(pred) ->
                let canSubsume = isSubsumedFromAnd pred nodes
                Some(nodes |> Array.removeAt idx)
            | _ -> None
        )
        |> Option.defaultValue nodes


    member this.filterAndNodes(nodeSet: NodeSet< ^t >, der) =
        let nodes = ResizeArray(nodeSet.Count)
        let mutable looping = true
        let mutable enumerator = (nodeSet :> seq<_>).GetEnumerator()

        while enumerator.MoveNext() = true && looping do
            match der enumerator.Current with
            | x when refEq x _uniques._false ->
                looping <- false
                nodes.Clear()
                nodes.Add(_uniques._false)
            | x -> nodes.Add(x)

        nodes |> Seq.toArray

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
            |> Seq.distinctBy LanguagePrimitives.PhysicalHash
            // |> Seq.sortBy LanguagePrimitives.PhysicalHash
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
                        let nodes =
                            nodeSet
                            |> Array.tryFind (fun v -> obj.ReferenceEquals(v, _uniques._trueStar))
                            |> function
                                | None -> nodeSet
                                | _ ->
                                    nodeSet
                                    |> Seq.collect (fun v ->
                                        match v with
                                        | _ when obj.ReferenceEquals(v, _uniques._trueStar) -> [||]
                                        | n -> [| n |]
                                    )
                                    |> Seq.toArray

                        let nodes = nodes |> this.trySubsumeAnd


                        match nodes with
                        | _ when nodes.Length = 0 -> _uniques._trueStar
                        | _ when nodes.Length = 1 -> (head nodes)
                        | twoormore ->
                            let flags = Flags.inferAnd twoormore

                            let startset =
                                twoormore |> Startset.inferMergeStartset solver

                            let mergedInfo = { Flags = flags; Startset = startset }

                            let newAnd = RegexNode.And(ofSeq twoormore, mergedInfo)

                            newAnd

            // _andCache.GetOrAdd(nodeSet, valueFactory = createNode)
            let key = nodeSet

            match _andCache.TryGetValue(key) with
            | true, v -> v
            | _ ->
                let v = createNode key
                _andCache.Add(key, v)
                v


    member this.mkOr(nodeSet: RegexNode< ^t >[]) : RegexNode< ^t > =
        let nodeSet =
            // filter and expand!
            nodeSet
            |> Seq.collect (fun v ->
                match v with
                | _ when obj.ReferenceEquals(v, _uniques._false) -> [||]
                | Or(nodes, _) -> nodes |> Seq.toArray
                | n -> [| n |]
            )
            |> Seq.distinctBy LanguagePrimitives.PhysicalHash
            |> Seq.toArray

        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash nodeSet

        if nodeSet.Length = 0 then
            _uniques._false
        else

            let createNode(nodeSet: RegexNode< ^t >[]) =
                nodeSet
                |> Array.tryFind (fun v -> obj.ReferenceEquals(v, _uniques._trueStar))
                |> function
                    | Some falseNode -> falseNode
                    | _ ->

                        let nodes =
                            match
                                nodeSet
                                |> Seq.exists (fun v ->
                                    obj.ReferenceEquals(v, _uniques._false)
                                    || (function
                                    | Or _ -> true
                                    | _ -> false)
                                        v
                                )
                            with
                            | false -> nodeSet
                            | _ ->
                                nodeSet
                                |> Seq.collect (fun v ->
                                    match v with
                                    | _ when obj.ReferenceEquals(v, _uniques._false) -> [||]
                                    | Or(nodes, _) -> nodes |> Seq.toArray
                                    | n -> [| n |]
                                )
                                |> Seq.toArray

                        let nodes = nodes |> this.trySubsumeOr

                        match nodes with
                        | _ when nodes.Length = 0 -> _uniques._false
                        | _ when nodes.Length = 1 -> (head nodes)
                        | twoormore ->
                            let flags = Flags.inferOr twoormore

                            let startset = twoormore |> Startset.inferMergeStartset solver

                            let mergedInfo = { Flags = flags; Startset = startset }
                            RegexNode.Or(ofSeq twoormore, mergedInfo)

            // _orCache.GetOrAdd(nodeSet, valueFactory = createNode)
            let key = nodeSet

            match _orCache.TryGetValue(key) with
            | true, v -> v
            | _ ->
                let v = createNode key
                _orCache.Add(key, v)
                v


    member this.mkNot(inner: RegexNode< ^t >) =

        let createNode(inner: RegexNode< ^t >) =
            // ~(Derx(R))
            match inner with
            // optional rewrite, needs testing
            // ~(a) = epsilon | [^a] | a.+
            // | [ Singleton tset ] ->
            //     let ornode =
            //         Or(
            //             ofSeq [| derivative @ cache.TruePlusList
            //                      []
            //                      [ Singleton(cache.Solver.Not(tset)) ] |],
            //             singletonInfo ()
            //         )
            //     [ ornode ]
            // TODO:
            | _ when refEq _uniques._false inner -> _uniques._trueStar // ~(⊥) -> ⊤*
            | _ when refEq _uniques._trueStar inner -> _uniques._false // ~(⊤*) -> ⊥
            | Epsilon -> _uniques._truePlus // ~(ε) -> ⊤+
            // all non-epsilon zero minimum width nodes resolve to false
            // e.g. ~(_{0,_}) -> ⊥  (negation of any loop with lower bound 0 is false)
            // or containing always nullable nodes is also false
            | Concat(info = regexNodeInfo) when regexNodeInfo.IsAlwaysNullable -> _uniques._false
            | _ ->
                let mutable flags = Flags.inferNode inner

                if flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) then
                    removeFlag &flags RegexNodeFlags.CanBeNullable
                    removeFlag &flags RegexNodeFlags.IsAlwaysNullable
                else if not (flags.HasFlag(RegexNodeFlags.CanBeNullable)) then
                    addFlag &flags RegexNodeFlags.CanBeNullable
                    addFlag &flags RegexNodeFlags.IsAlwaysNullable

                let startset = Startset.inferStartset solver inner

                let info = this.CreateInfo(flags, startset)

                Not(inner, info)


        // _notCache.GetOrAdd(inner, valueFactory = createNode)
        let key = inner

        match _notCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let v = createNode key
            _notCache.Add(key, v)
            v



    member this.setFromStr(setPattern: string) =
        let tree =
            RegexParser.Parse(
                setPattern,
                RegexOptions.ExplicitCapture,
                CultureInfo.InvariantCulture
            )

        let setStr = tree.Root.Child(0).Str
        let bdd = converter.CreateBDDFromSetString(setStr)
        let converted = solver.ConvertFromBDD(bdd, bcss)
        RegexNode.Singleton(converted)

    member this.bddFromSetString(setPattern: string) =
        let bdd = converter.CreateBDDFromSetString(setPattern)
        bdd


    member this.setFromNode(node: RegexNode) =
        let bdd = converter.CreateBDDFromSetString(node.Str)
        RegexNode.Singleton(bdd)


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

                            Concat(v, acc, ofFlagsAndStartset (flags', startset'))
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
                    // .*, etc is very common, cache it

                    let flags = Flags.inferLoop (body, lower, upper)

                    let startset = Startset.inferLoopStartset solver (body, lower, upper)

                    let loop =
                        RegexNode.Loop(body, lower, upper, info = b.CreateInfo(flags, startset))

                    loop
            | _, (x, y) when x > 0 ->
                let flags = Flags.inferLoop (body, lower, upper)

                let startset = Startset.inferLoopStartset solver (body, lower, upper)

                let info = b.CreateInfo(flags, startset)

                let loop = RegexNode.Loop(body, lower, upper, info = info)

                loop

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

    member this.CreateInfo(flags, startset) : RegexNodeInfo<_> =
        ofFlagsAndStartset (flags, startset)


// trivia:
// for correct .NET semantics
// https://github.com/dotnet/runtime/blob/1fe9c0bba15e23b65be007ddf38c43d28b2f9dd2/src/libraries/System.Text.RegularExpressions/src/System/Text/RegularExpressions/Symbolic/UnicodeCategoryConditions.cs#L67
// member this.wordCharForWordBorder = this.setFromStr "[\w\u200C\u200D]"
