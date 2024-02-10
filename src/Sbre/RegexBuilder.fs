namespace Sbre

open System
open System.Buffers
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


module
#if !RELEASE
    internal
#endif
    StartsetHelpers =

    let bddSingleChar (bdd:BDD) =
        let mutable ranges = BDDRangeConverter.ToRanges(bdd)
        match ranges with
        | [| (s,e) |] when s = e -> Some (char s)
        | _ -> None
    let bddToStartsetChars(bdd: BDD) : PredStartset =
        let rcc = RegexCharClass()
        let mutable ranges = BDDRangeConverter.ToRanges(bdd)
        let mutable e = ranges.GetEnumerator()
        let mutable setTooBig = false
        let mutable i = 0u
        let sizeLimit = 1024u
        let charArray = Array.zeroCreate<char> (int (sizeLimit))

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

        let trimmed =
            charArray.AsSpan().Slice(0, int i).ToArray()

        let ranges2 = PredStartset.Of(StartsetFlags.None, trimmed)

        if setTooBig then
            // no optimization if startset too large
            PredStartset.Of(StartsetFlags.TooBig, [||])
        else
            ranges2

    let startsetsFromMintermArray(bdds: BDD array) : PredStartset array =
        let startsets1 =
            bdds[1..]
            |> Array.map bddToStartsetChars
        //
        // match startsets1 |> Array.tryFind (fun v -> v.Flags = StartsetFlags.TooBig) with
        // | Some v -> [|v|]
        // | _ ->

        let searchChars =
            startsets1
            |> Array.collect (fun v -> v.Chars)

        let invertedStartset0 =
            PredStartset.Of(StartsetFlags.Inverted, searchChars)

        Array.append
            [| invertedStartset0 |]
            startsets1

    let static_merged_chars = Array.zeroCreate 1024 |> ResizeArray<char>

    /// None means set is too big to search
    let getMintermChars
        (
            _solver: ISolver<TSet>,
            predStartsetArray: Types.PredStartset array,
            uintMinterms:TSet array, //: 't array when 't: (static member Zero: 't) and 't: (static member (&&&) : 't * 't -> 't),
            startset: TSet //: 't when 't: (static member Zero: 't) and 't: (static member (&&&) : 't * 't -> 't)
        )
        : Memory<char> option
        =
        let mergedCharSpan = CollectionsMarshal.AsSpan(static_merged_chars)
        let mutable totalLen = 0
        let mutable tooBig = false

        let shouldInvert = Solver.elemOfSet startset uintMinterms[0]
        if shouldInvert then
            for i = 1 to predStartsetArray.Length - 1 do
                let pureMt = uintMinterms[i]
                // match _solver.isElemOfSet (startset,pureMt) with
                match Solver.elemOfSet startset pureMt with
                | true -> ()
                | false ->
                    let ps = predStartsetArray[i]
                    if ps.Flags.HasFlag(StartsetFlags.TooBig) then
                        tooBig <- true
                    let targetSpan = mergedCharSpan.Slice(totalLen)
                    let pspan = predStartsetArray[i].Chars.AsSpan()
                    pspan.CopyTo(targetSpan)
                    totalLen <- totalLen + pspan.Length
            if tooBig then
                None
            else
                Some (Memory(mergedCharSpan.Slice(0, totalLen).ToArray()))

        else
            for i = 1 to predStartsetArray.Length - 1 do
                let pureMt = uintMinterms[i]


                match Solver.elemOfSet startset pureMt with
                // match _solver.isElemOfSet (startset,pureMt) with
                | true ->
                    let ps = predStartsetArray[i]
                    if ps.Flags.HasFlag(StartsetFlags.TooBig) then
                        tooBig <- true


                    let targetSpan = mergedCharSpan.Slice(totalLen)
                    let pspan = predStartsetArray[i].Chars.AsSpan()
                    pspan.CopyTo(targetSpan)
                    totalLen <- totalLen + pspan.Length
                | false -> ()

            if tooBig then
                None
            else
                Some (Memory(mergedCharSpan.Slice(0, totalLen).ToArray()))

    let getMergedIndexOfSpan
        (
            _solver: ISolver<TSet>,
            predStartsetArray: Types.PredStartset array,
            uintMinterms:TSet array, //: 't array when 't: (static member Zero: 't) and 't: (static member (&&&) : 't * 't -> 't),
            startset: TSet //: 't when 't: (static member Zero: 't) and 't: (static member (&&&) : 't * 't -> 't)
        )
        : SearchValues<char> option
        =
        let mts = (getMintermChars(_solver,predStartsetArray,uintMinterms,startset))
        match mts with
        | None -> None
        | Some v -> Some (SearchValues.Create v.Span)



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


/// reuses nodes and ensures reference equality
[<Sealed>]
type RegexBuilder<'t when 't :> IEquatable< 't > and 't: equality  >
    (converter: RegexNodeConverter, solver: ISolver< 't >, bcss: CharSetSolver) as b =
    let _createInfo flags containsMinterms =
        RegexNodeInfo<'t>(NodeFlags = flags, Minterms = containsMinterms)

    let getDerivativeCacheComparer() : IEqualityComparer<struct (TSet * RegexNode<TSet>)> =
        { new IEqualityComparer<struct (TSet * RegexNode<TSet>)> with
            member this.Equals(struct (x1, x2), struct (y1, y2)) = x1 = y1 && refEq x2 y2

            member this.GetHashCode(struct (x, y)) = int x ^^^ LanguagePrimitives.PhysicalHash y
        }

    let _andCacheComparer =
        { new IEqualityComparer<RegexNode<'t>[]> with
            member this.Equals(xs, ys) =
                xs.Length = ys.Length && Array.forall2 refEq xs ys

            member this.GetHashCode(x) = Enumerator.getSharedHash x
        }

    let _orCacheComparer =
        { new IEqualityComparer<RegexNode< 't >[]> with
            member this.Equals(xs, ys) =
                xs.Length = ys.Length && Array.forall2 refEq xs ys

            member this.GetHashCode(x) = Enumerator.getSharedHash x
        }

    let _concatCacheComparer: IEqualityComparer<struct (RegexNode<'t> * RegexNode<'t >)> =
        { new IEqualityComparer<struct (RegexNode<'t> * RegexNode<'t>)> with

            member this.Equals(struct (x1, y1), struct (x2, y2)) =
                refEq x1 x2 && refEq y1 y2


            member this.GetHashCode(struct (x, y)) =
                LanguagePrimitives.PhysicalHash x ^^^ LanguagePrimitives.PhysicalHash y
        }

    let _lookaroundComparer: IEqualityComparer<struct (RegexNode<'t> * bool * bool * int * int list)> =
        { new IEqualityComparer<struct (RegexNode<'t> * bool * bool * int * int list)> with
            member this.Equals(struct (x1, y1, z1, r1, k1), struct (x2, y2, z2, r2, k2)) =
                y1 = y2 && z1 = z2 && k1 = k2
                && refEq x1 x2

            member this.GetHashCode(struct (x, y, z, r, k)) =
                LanguagePrimitives.PhysicalHash x ^^^ r
                //^^^ LanguagePrimitives.PhysicalHash y
        }



    let _refComparer =
        { new IEqualityComparer<RegexNode< 't >> with
            member this.Equals(xs, ys) = refEq xs ys

            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }

    let _bddrefComparer =
        { new IEqualityComparer<RegexNode< BDD >> with
            member this.Equals(xs, ys) = refEq xs ys

            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }

    let _u64refComparer =
        { new IEqualityComparer<RegexNode<TSet>> with
            member this.Equals(xs, ys) = refEq xs ys

            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }

    let _singletonCache: Dictionary< 't, RegexNode< 't > > = Dictionary()

    let _loopCache: Dictionary<struct (RegexNode< 't > * int * int), RegexNode< 't >> =
        Dictionary()

    let _concatCache: Dictionary<struct (RegexNode< 't > * RegexNode< 't >), RegexNode< 't >> =
        Dictionary(_concatCacheComparer)

    let _subsumptionCacheComparer: IEqualityComparer<struct (RegexNode<TSet> * RegexNode<TSet >)> =
        { new IEqualityComparer<struct (RegexNode<TSet> * RegexNode<TSet>)> with
            member this.Equals(struct (x1, y1), struct (x2, y2)) = refEq x1 x2 && refEq y1 y2
            member this.GetHashCode(struct (x, y)) =
                LanguagePrimitives.PhysicalHash x ^^^ LanguagePrimitives.PhysicalHash y
        }

    let _subsumptionCache: Dictionary<struct (RegexNode<TSet> * RegexNode<TSet>), bool> =
        Dictionary(_subsumptionCacheComparer)

    let _orCache: Dictionary<RegexNode< 't >[], RegexNode< 't >> =
        Dictionary(_orCacheComparer)

    let _notCache: Dictionary<RegexNode< 't >, RegexNode< 't >> = Dictionary(_refComparer)
    let _lookaroundCache: Dictionary<struct (RegexNode< 't >*bool*bool*int*int list), RegexNode< 't >> = Dictionary(_lookaroundComparer)

    let _andCache: Dictionary<RegexNode<'t>[], RegexNode<'t>> = Dictionary(_andCacheComparer)

    let _true = RegexNode.Singleton(solver.Full)
    let _false = RegexNode.Singleton(solver.Empty)
    // singleton instances
    let _uniques = {|
        _eps = RegexNode<'t>.Epsilon
        _false = _false
        _true = _true
        _trueStar =
            RegexNode.Loop(
                _true,
                low = 0,
                up = Int32.MaxValue,
                info = _createInfo (RegexNodeFlags.IsAlwaysNullableFlag ||| RegexNodeFlags.CanBeNullableFlag) solver.Full

            )
        _truePlus =
            RegexNode.Loop(
                _true,
                low = 1,
                up = Int32.MaxValue,
                info = _createInfo (RegexNodeFlags.None) solver.Full
            )
        _wordChar = lazy b.setFromStr @"\w"
        _nonWordChar = lazy b.setFromStr @"\W"
    |}

    do _loopCache.Add(struct(_true, 0, Int32.MaxValue), _uniques._trueStar)
    do _loopCache.Add(struct(_true, 1, Int32.MaxValue), _uniques._truePlus)
    do _singletonCache.Add(solver.Full,_true)
    do _singletonCache.Add(solver.Empty,_false)

    let _anchors =
        let __z_anchor =
            lazy b.mkLookaround(_true,false,true)
        let __big_a_anchor =
            lazy b.mkLookaround(_true,true,true)

        {|
            _endZAnchor = lazy b.mkOr([
                b.mkLookaround(_true,false,true)
                b.mkLookaround(b.mkConcat2(b.one '\n',_true),false,true)
            ])
            _zAnchor = RegexNode<'t>.Anchor End

            // \A ≡ (?<!⊤)
            _bigAAnchor = RegexNode<'t>.Anchor Begin
                // __big_a_anchor
            // (?<=\A|\A\n) ≡ \a
            _aAnchor =
                lazy
                    let seqv =
                        ofSeq [
                            __big_a_anchor.Value
                            b.mkConcat2(__big_a_anchor.Value, b.one '\n')
                        ]
                    let node = b.mkOr(seqv)
                    b.mkLookaround(node,true,true)


            _nonWordBorder =
                lazy
                    // (?!ψ\w)
                    let c1 = [
                        b.mkLookaround(_uniques._wordChar.Value,true,true)
                        // (?<!ψ\w)
                        b.mkLookaround(_uniques._wordChar.Value,false,true)
                    ]
                    // (?=ψ\w)
                    let c2 = [
                        b.mkLookaround(_uniques._wordChar.Value,true,false)
                        // (?<=ψ\w)
                        b.mkLookaround(_uniques._wordChar.Value,false,false)
                    ]
                    b.mkOr(ofSeq [ b.mkConcat c1; b.mkConcat c2 ])


            _wordBorder =
                    lazy
                        b.mkOr(
                        ofSeq [
                            b.mkConcat [
                                // (?<=ψ\w)
                                b.mkLookaround(_uniques._wordChar.Value,true,false)
                                b.mkLookaround(_uniques._wordChar.Value,false,true)
                            ]
                            b.mkConcat [
                                b.mkLookaround(_uniques._wordChar.Value,true,true)
                                b.mkLookaround(_uniques._wordChar.Value,false,false)
                            ]
                        ]
                    )
            // (?<=\W)
            // _nonWordLeft = lazy b.mkLookaround(_uniques._nonWordChar.Value,true,false)
            // proper definition (?<=\a|\W)
            _nonWordLeft =
                lazy
                    b.mkLookaround(
                    b.mkOr([
                        RegexNode<'t>.Anchor Begin
                        _uniques._nonWordChar.Value
                    ]
                    ),true,false)
            // (?=\W)
            // _nonWordRight = lazy b.mkLookaround(_uniques._nonWordChar.Value,false,false)
            // proper definition (?=\z|\W)
            _nonWordRight =
                lazy
                    b.mkOr([ RegexNode<'t>.Anchor End; b.mkLookaround( _uniques._nonWordChar.Value,true,false) ])

                    // b.mkLookaround(
                    //     b.mkOr([ RegexNode<'t>.Anchor End; _uniques._nonWordChar.Value ])
                    //     ,true,false)

            // ^ ≡ \A|(?<=\n)
            _caretAnchor =
                RegexNode<'t>.Anchor Bol
                // lazy
                //     b.mkOr(
                //         ofSeq [
                //             __big_a_anchor.Value
                //             b.mkLookaround(b.one '\n',true,false)
                //         ]
                //     )
            _dollarAnchor =
                RegexNode<'t>.Anchor Eol
                // lazy
                //
                //     let info = _createInfo (RegexNodeFlags.CanBeNullableFlag ||| RegexNodeFlags.ContainsLookaroundFlag) solver.Full
                //
                //     Or(
                //         ofSeq [
                //             __z_anchor.Value
                //             b.mkLookaround(b.one '\n',false,false)
                //         ],
                //         info
                //     )
        |}

    let mutable _prefixCache: Dictionary<RegexNode<'t>, InitialStartset<'t>> =
        Dictionary(_refComparer)

    let mutable _uniquesDict : Dictionary<RegexNode<BDD>,RegexNode<'t>> = Dictionary<RegexNode<BDD>,RegexNode<'t>>(_bddrefComparer)



    member this.trueStar = _uniques._trueStar
    member this.uniques = _uniques
    member this.anchors = _anchors
    member this.PrefixCache = _prefixCache
    // member this.DerivativeCache = _derivativeCache
    // member this.Startset2Cache = _startset2Cache
    member this.SubsumptionCache = _subsumptionCache
    member this.UniquesDict = _uniquesDict

    member this.InitializeUniqueMap(oldBuilder:RegexBuilder<BDD>) =
        _uniquesDict.Add(oldBuilder.anchors._aAnchor.Value,this.anchors._aAnchor.Value)
        _uniquesDict.Add(oldBuilder.anchors._bigAAnchor,this.anchors._bigAAnchor)
        _uniquesDict.Add(oldBuilder.anchors._caretAnchor,this.anchors._caretAnchor)
        _uniquesDict.Add(oldBuilder.anchors._dollarAnchor,this.anchors._dollarAnchor)
        _uniquesDict.Add(oldBuilder.anchors._nonWordBorder.Value,this.anchors._nonWordBorder.Value)
        _uniquesDict.Add(oldBuilder.anchors._wordBorder.Value,this.anchors._wordBorder.Value)
        _uniquesDict.Add(oldBuilder.anchors._zAnchor,this.anchors._zAnchor)
        ()

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.AddTransitionInfo
        (
            pred: 't,
            node: RegexNode<'t>,
            result: RegexNode<'t>
        ) =
        match node with
        | Or(info = info)
        | Loop(info = info)
        | And(info = info)
        | Not(info = info)
        | Concat(info = info) ->
            match info.Transitions.Count with
            | 0 ->
                if
                    refEq _uniques._true node
                    || refEq _uniques._trueStar node
                    || refEq _uniques._truePlus node
                then
                    info.Transitions.Add({ Set = solver.Full; Node = result })
                else
                    info.Transitions.Add({ Set = pred; Node = result })

            | n ->
                // use mutable e = info.Transitions.GetEnumerator()
                let mutable e =
                    CollectionsMarshal.AsSpan(info.Transitions).GetEnumerator()

                let mutable looping = true
                let mutable counter = 0

                while looping && e.MoveNext() do
                    let curr = e.Current

                    if obj.ReferenceEquals(curr.Node, result) then
                        looping <- false
                        info.Transitions[counter].Set <- solver.Or(pred, curr.Set)

                    counter <- counter + 1

                if looping then
                    info.Transitions.Add({ Set = pred; Node = result })
        | _ -> ()


    member this.setFromNode(node: RegexNode) =
        let bdd = (converter.CreateBDDFromSetString(node.Str))
        let a2 = solver.ConvertFromBDD(bdd, bcss)
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


    member this.one(char: char) : RegexNode< 't > =
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


    member this.one(minterm: 't) : RegexNode< 't > =
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


    member this.trySubsumeOr(nodes: HashSet<RegexNode< 't >>) : RegexNode< 't > seq =
        let mutable result = ValueNone
        let starLoops =
            nodes
            |> Seq.choose (fun v ->
                match v with
                | SingletonStarLoop(pred) -> Some struct (pred, v)
                | _ -> None
            )
            |> Seq.toArray

        // ((ab)*|⊤*(ab)*) ==> (ab)*
        // (⊤*~(⊤*\n\n⊤*)|~(⊤*\n\n⊤*))
        if nodes |> Seq.forall (_.IsAlwaysNullable)  then
            match tryFindV (function TrueStarredConcat solver (tail) as currNode -> ValueSome (currNode, tail) | _ -> ValueNone) nodes with
            | ValueSome (currNode,tail) ->
                if (nodes.Count = 2 && nodes.Contains(tail) ) then
                    result <- ValueSome (Seq.singleton currNode)
                else ()
            | _ -> ()

        if result.IsSome then result.Value else

        match starLoops.Length > 0 with
        | true ->

            let struct (largestPred, largestStarLoop) =
                starLoops
                |> Seq.reduce (fun struct (e1, e1node) struct (e2, e2node) ->
                    let conj = solver.And(e1, e2)
                    let iselem = not (solver.IsEmpty(conj))

                    if conj = e1 then (struct (e2, e2node))
                    elif conj = e2 then (struct (e1, e1node))
                    else (solver.Empty, Unchecked.defaultof<_>)
                )

            if solver.IsEmpty(largestPred) then
                nodes
            else

                let mutable e = nodes.GetEnumerator()

                while e.MoveNext() do
                    let curr = e.Current

                    if loopSubsumesBranch solver largestPred curr then
                        nodes.Remove(curr) |> ignore

                nodes.Add(largestStarLoop) |> ignore
                nodes
        | _ ->

            match nodes with
            | AllSameHead(shead) when typeof<'t> = typeof<TSet>  ->
                match nodes |> unbox with
                | TrySubsumeSameTail (s) -> Seq.singleton (unbox s) //:?> RegexNode<'t> seq
                | _ ->
                    let mergeTails =
                        nodes |> Seq.map (fun v ->
                            match v with
                            | Concat(regexNode, tail, regexNodeInfo) -> tail
                            | _ -> failwith "todo: subsumption bug"
                        )
                        |> this.mkOr
                    nodes.Clear()
                    nodes.Add(this.mkConcat2(shead, mergeTails)) |> ignore
                    nodes
            | _ ->

                nodes



    member this.trySubsumeAnd(nodes: RegexNode< 't > seq) : RegexNode< 't > seq =
        nodes
        |> Seq.indexed
        |> Seq.tryPick (fun (idx, v) ->
            match v with
            | SingletonStarLoop(pred) ->
                if isSubsumedFromAnd solver pred v nodes then
                    Some(nodes |> Seq.removeAt idx)
                else
                    None
            | _ -> None
        )
        |> Option.defaultValue nodes


    member this.trySubsumeTopLevelOr(struct(existing: RegexNode<TSet>, newnode: RegexNode<TSet>)) : bool =
        match struct (existing, newnode) with
        | Concat(_), And(nodes = nodes2) ->
            nodes2.Contains(existing)
            || setIsSubsumedSingle nodes2 (existing)

        | Or(nodes = nodes1), And(nodes = nodes2) ->
            if nodes2.Contains(existing) then
                true
            else
                let mutable found = false
                use mutable n1e = nodes1.GetEnumerator()

                while not found && n1e.MoveNext() do
                    if
                        nodes2.Contains(n1e.Current)

                    then
                        found <- true
                if not found then
                    if setIsSubsumedSingle nodes2 (existing) then
                        found <- true
                    else
                        let v = 1
                        ()
#if OPTIMIZE
                if not found then
                    failwith $"unoptimized:\n{existing.ToString()}\n{newnode.ToString()}"
#endif
                found

        | And(nodes = nodes1), And(nodes = nodes2)
        | Or(nodes = nodes2), Or(nodes = nodes1) ->


            let mutable found = false

            // check for subset equality
            use mutable n1e = nodes1.GetEnumerator()
            use mutable n2e = nodes2.GetEnumerator()
            let mutable allcontained = true

            while allcontained && n1e.MoveNext() do

                let mutable currFound = false

                while not currFound && n2e.MoveNext() do
                    let currN2 = n2e.Current
                    let currN1 = n1e.Current

                    if refEq currN1 currN2 then
                        currFound <- true
                    else

                    match currN1 with
                    | Or(nodes = currN1Inner) ->
                        if currN1Inner.Contains(currN2) then
                            currFound <- true
                        else
                            match isSubSequence (currN2) (currN1) with
                            | ValueSome (v) ->
                                currFound <- true
                            | _ -> ()
                    | _ ->
                        match isSubSequence (currN2) (currN1) with
                        | ValueSome (v) ->
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

            found

        | _ -> false
        |> (fun v ->
            _subsumptionCache.Add(struct (existing, newnode), v)
            v
        )


    member this.mkAnd
        (
            nodes: RegexNode<'t> seq
        ) : RegexNode<'t> =

        let key = nodes |> Seq.toArray
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash key

        match _andCache.TryGetValue(key) with
        | true, v -> v
        | _ ->

        let mutable enumerating = true
        let mutable status = MkAndFlags.None
        let derivatives = ResizeArray()
        use mutable e = nodes.GetEnumerator()
        while e.MoveNext() = true && enumerating do
            let rec handleNode(deriv) =
                match deriv with
                | _ when obj.ReferenceEquals(deriv, _uniques._trueStar) -> ()
                | _ when obj.ReferenceEquals(deriv, _uniques._false) ->
                    enumerating <- false
                    status <- MkAndFlags.IsFalse
                | And(nodes, _) ->
                    for node in nodes do
                        handleNode node
                // ~(.*) -> always false (?)
                // | Not(node, info) when info.CanNotBeNullable() && info.DoesNotContainEpsilon ->
                //     enumerating <- false
                //     status <- MkAndFlags.IsFalse
                | Epsilon ->
                    status <- MkAndFlags.ContainsEpsilon
                    // derivatives.Add(deriv)
                | _ -> derivatives.Add(deriv)

            handleNode e.Current

        if status.HasFlag(MkAndFlags.ContainsEpsilon) then
            derivatives.Add(_uniques._eps)

        match status with
        | MkAndFlags.IsFalse -> _uniques._false
        | MkAndFlags.ContainsEpsilon when derivatives.Exists(fun v -> v.CanNotBeNullable) ->
            _uniques._false
        | _ ->



            let createNode(nodes: RegexNode<_>[]) =
                match nodes with
                | _ when nodes.Length = 0 -> _uniques._trueStar
                | _ when nodes.Length = 1 -> nodes[0]
                | twoormore ->
                    let flags = Flags.inferAnd twoormore
                    let minterms2 =
                        twoormore
                        |> Seq.map (_.SubsumedByMinterm(solver))
                        |> Seq.fold (fun acc v -> solver.Or(acc,v) ) solver.Empty
                    let mergedInfo =
                        this.CreateInfo(flags, minterms2)
                    //
                    match twoormore with
                    | _ when twoormore.Length = 0 -> _uniques._trueStar
                    | _ when twoormore.Length = 1 -> twoormore[0]
                    | _ ->

                    let newAnd = RegexNode.And(ofSeq twoormore, mergedInfo)
                    newAnd

            let asArray = derivatives |> this.trySubsumeAnd |> Seq.toArray
            Array.sortInPlaceBy LanguagePrimitives.PhysicalHash asArray

            match _andCache.TryGetValue(asArray) with
            | true, v -> v
            | _ ->
                let v = createNode (asArray)

                _andCache.Add(key, v)
                v

    member this.mkOr
        (
            nodes: RegexNode<'t> seq

        ) : RegexNode<_> =

        let key = nodes |> Seq.toArray
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash key

        match _orCache.TryGetValue(key) with
        | true, v -> v
        | _ ->

        let mutable enumerating = true
        let mutable status = MkOrFlags.None
        let mutable zeroloops = 0
        let mutable singletonLoops = 0
        use mutable e = nodes.GetEnumerator()
        let derivatives = HashSet(_refComparer) //this.DerivativeSet

        while e.MoveNext() && enumerating do
            let rec handleNode(deriv: RegexNode<'t>) =
                match deriv with
                | _ when obj.ReferenceEquals(deriv, _uniques._false) -> ()
                | _ when obj.ReferenceEquals(deriv, _uniques._trueStar) ->
                    enumerating <- false
                    status <- MkOrFlags.IsTrueStar
                | Or(nodes, _) -> nodes |> Seq.iter handleNode
                | Concat(head = Loop(low = 0; up = upper)) when upper <> Int32.MaxValue ->
                    zeroloops <- zeroloops + 1
                    derivatives.Add(deriv) |> ignore
                | Loop(node = Singleton body; low = 0; up = Int32.MaxValue) ->
                    singletonLoops <- singletonLoops + 1
                    derivatives.Add(deriv) |> ignore
                // todo: eat epsilon
                | Epsilon -> status <- status ||| MkOrFlags.ContainsEpsilon
                | _ -> derivatives.Add(deriv) |> ignore

            handleNode e.Current

        match status with
        | MkOrFlags.IsTrueStar -> _uniques._trueStar
        | _ ->
        let allSingletons() =
            nodes
            |> Seq.forall (function Singleton _ -> true | _ -> false )
        if derivatives.Any() && allSingletons() then
            let merged =
                derivatives
                |> Seq.map (function Singleton (pred) -> pred | _ -> failwith "invalid case" )
                |> Seq.fold (fun v v2 -> solver.Or(v,v2) ) solver.Empty
            this.one(merged)
        else

        if zeroloops > 1 then
            // remove loop duplicates
            // C{0,9}D  | C{0,8}D = C{0,9}D
            derivatives
            |> Seq.choose (fun v ->
                match v with
                | Concat(head = Loop(low = 0; up = upper; node = body); tail = tail) when
                    upper < Int32.MaxValue
                    ->
                    Some(struct (body, tail), upper, v)
                | _ -> None
            )
            |> Seq.groupBy (fun (uniq, upper, v) -> uniq)
            |> Seq.iter (fun (uniq, nodes) ->
                nodes
                |> Seq.sortByDescending (fun (body, upper, v) -> upper)
                |> Seq.skip 1
                |> Seq.iter (fun (body, upper, v) -> derivatives.Remove(v) |> ignore)
            )
        // add epsilon only if no nullables yet
        if (status &&& MkOrFlags.ContainsEpsilon) <> MkOrFlags.None then
            if derivatives.Any(fun v -> v.IsAlwaysNullable) then
                ()
            else
                derivatives.Add(_uniques._eps) |> ignore

        if derivatives.Count = 0 then
            _uniques._false
        else
        if derivatives.Count = 1 then derivatives |> Seq.head else

        let nodeSet = derivatives |> this.trySubsumeOr |> Seq.toArray
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash nodeSet

        if nodeSet.Length = 0 then
            _uniques._false
        else
            let createNode(nodes: RegexNode< 't >[]) =
                match nodes with
                | _ when nodes.Length = 0 -> _uniques._false
                | _ when nodes.Length = 1 -> (head nodes)
                | twoormore ->
                    let flags = Flags.inferOr twoormore

                    let minterms2 =
                        twoormore
                        |> Seq.map (_.SubsumedByMinterm(solver))
                        |> Seq.fold (fun acc v -> solver.Or(acc,v) ) solver.Empty
                    let mergedInfo =
                        this.CreateInfo(flags, minterms2)

                    RegexNode.Or(ofSeq twoormore, mergedInfo)

            match _orCache.TryGetValue(nodeSet) with
            | true, v -> v
            | _ ->
                let v = createNode nodeSet
                _orCache.Add(key, v)
                v

    member this.mkNot(inner: RegexNode< 't >) =
        // short-circuit
        // if inner.IsAlwaysNullable && inner.DoesNotContainEpsilon then
        //     _uniques._false
        // else

            let createNode(inner: RegexNode< 't >) =
                match inner with
                | _ when refEq _uniques._false inner -> _uniques._trueStar // ~(⊥) -> ⊤*
                | _ when refEq _uniques._trueStar inner -> _uniques._false // ~(⊤*) -> ⊥
                | Epsilon -> _uniques._truePlus // ~(ε) -> ⊤+
                | _ ->
                    let mutable flags = Flags.inferNot inner
                    Not(inner, this.CreateInfo(flags, inner.SubsumedByMinterm(solver)))

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
        this.one converted

    member this.bddFromSetString(setPattern: string) = converter.CreateBDDFromSetString(setPattern)



    member this.mkConcat2(head: RegexNode< 't >, tail: RegexNode< 't >) : RegexNode< 't > =
        match head with
        | Epsilon -> tail // ()R -> R
        // | LookAround(node=Epsilon; lookBack=true; negate=false) -> tail
        | _ when refEq head _uniques._false -> _uniques._false // ⊥R -> ⊥
        | _ ->
            let key = struct (head, tail)

            match _concatCache.TryGetValue(key) with
            | true, v -> v
            | _ ->
                // try rewrite
                match head, tail with
                // (?<=a.*)(?<=\W)aa to (?<=⊤*a.*&⊤*\W)aa
                | LookAround(node=node1;lookBack=true;negate=false), Concat(head=LookAround(node=node2;lookBack=true;negate=false;);tail=tail2) ->
                    let combined = this.mkAnd([
                        this.mkConcat2(this.trueStar,node1)
                        this.mkConcat2(this.trueStar,node2)
                    ])
                    let v = this.mkLookaround(combined, true, false)
                    let v = this.mkConcat2(v, tail2)
                    _concatCache.Add(key, v)
                    v
                // (?=a.*)(?=\W) to (?=a.*⊤*&\W⊤*)
                | LookAround(node=node1;lookBack=false;negate=false), LookAround(node=node2;lookBack=false;negate=false) ->
                    let combined = this.mkAnd([
                        this.mkConcat2(node1,this.trueStar)
                        this.mkConcat2(node2,this.trueStar)
                    ])
                    let v = this.mkLookaround(combined, false, false)
                    _concatCache.Add(key, v)
                    v
                // \b(?=.*c)
                | Or(nodes,_), LookAround(node=node2;lookBack=false;negate=false) ->
                    let v = nodes |> Seq.map (fun v -> b.mkConcat2(v,tail) ) |> this.mkOr
                    _concatCache.Add(key, v)
                    v
                | LookAround(node=Epsilon;lookBack=false;negate=false;pendingNullables = nullables), tail when not tail.IsAlwaysNullable ->
                    // Experimental
                    let v =
                        let flags = Flags.inferConcat head tail
                        let mergedMinterms = solver.Or(head.SubsumedByMinterm(solver),tail.SubsumedByMinterm(solver))
                        let info = this.CreateInfo(flags, mergedMinterms)
                        Concat(head, tail, info)
                    _concatCache.Add(key, v)
                    v
                | LookAround(lookBack=false;negate=false), tail when not tail.IsAlwaysNullable ->
                    failwith "inner lookarounds are not supported!"
                | _ ->
                    let v =
                        let flags = Flags.inferConcat head tail
                        let mergedMinterms = solver.Or(head.SubsumedByMinterm(solver),tail.SubsumedByMinterm(solver))
                        let info = this.CreateInfo(flags, mergedMinterms)
                        Concat(head, tail, info)

                    _concatCache.Add(key, v)
                    v


    member this.mkLookaround(body: RegexNode< 't >, lookBack:bool, negate:bool, ?pendingNullable) : RegexNode< 't > =
        let (rel,pending) = defaultArg pendingNullable (0,[])
        let key = struct (body, lookBack, negate, rel, pending)
        match _lookaroundCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let newNode =
                match body, lookBack, negate with
                | Epsilon, true, false -> _uniques._eps
                | _ -> LookAround(body, lookBack = lookBack, negate = negate, pendingNullables=(rel,pending))
            _lookaroundCache.Add(key, newNode)
            newNode



    member this.mkConcat(nodesCorrectOrder: RegexNode< 't > list) : RegexNode< 't > =

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
                            // let flags' = Flags.inferConcat v acc
                            this.mkConcat2 (v, acc)
                            // Concat(v, acc, this.CreateInfo(flags'))
                    )
                    Epsilon

            combined

    member this.mkLoop(struct(body: RegexNode< 't >, lower: int, upper: int)) =
        let createNode(struct (body: RegexNode< 't >, lower: int, upper: int)) =
            match body, struct (lower, upper) with
            | _, LoopKind LoopKind.EmptyLoop -> Epsilon
            | _, LoopKind LoopKind.Single -> body
            | Singleton minterm, LoopKind LoopKind.Plus when solver.IsFull(minterm) ->
                _uniques._trueStar
            | Singleton minterm, LoopKind LoopKind.Star ->
                if solver.IsFull(minterm) then
                    _uniques._trueStar
                else
                    let flags = Flags.inferLoop (body, lower, upper)

                    RegexNode.Loop(body, lower, upper, info = b.CreateInfo(flags, body.SubsumedByMinterm(solver)))
            | _, (x, y) when x > 0 ->
                let flags = Flags.inferLoop (body, lower, upper)
                let info = b.CreateInfo(flags, body.SubsumedByMinterm(solver))
                RegexNode.Loop(body, lower, upper, info = info)
            | _ ->
                let flags = Flags.inferLoop (body, lower, upper)
                let info = b.CreateInfo(flags, body.SubsumedByMinterm(solver))
                RegexNode.Loop(body, lower, upper, info = info)

        let key = struct (body, lower, upper)

        match _loopCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let v = createNode key
            _loopCache.Add(key, v)
            v

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateInfo(flags: RegexNodeFlags, containsMinterms:'t) : RegexNodeInfo<_> =
        _createInfo flags containsMinterms



// trivia:
// for correct .NET semantics
// https://github.com/dotnet/runtime/blob/1fe9c0bba15e23b65be007ddf38c43d28b2f9dd2/src/libraries/System.Text.RegularExpressions/src/System/Text/RegularExpressions/Symbolic/UnicodeCategoryConditions.cs#L67
// member this.wordCharForWordBorder = this.setFromStr "[\w\u200C\u200D]"
