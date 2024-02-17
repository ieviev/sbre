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
        let charArray = Array.zeroCreate<char> (int sizeLimit)

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
        let mts = getMintermChars(_solver,predStartsetArray,uintMinterms,startset)
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
    let _createInfo flags containsMinterms pendingNullables =
        RegexNodeInfo<'t>(NodeFlags = flags, Minterms = containsMinterms, PendingNullables=pendingNullables)

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
        { new IEqualityComparer<RegexNode< 't >Memory> with
            member this.Equals(xs, ys) =
                xs.Length = ys.Length &&
                xs.Span.SequenceEqual(ys.Span)
                // Seq.forall2 refEq xs.Span ys.Span
            member this.GetHashCode(x) = Enumerator.getSharedHash2 x
        }

    let _concatCacheComparer: IEqualityComparer<struct (RegexNode<'t> * RegexNode<'t >)> =
        { new IEqualityComparer<struct (RegexNode<'t> * RegexNode<'t>)> with

            member this.Equals(struct (x1, y1), struct (x2, y2)) =
                refEq x1 x2 && refEq y1 y2

            member this.GetHashCode(struct (x, y)) =
                LanguagePrimitives.PhysicalHash x ^^^ LanguagePrimitives.PhysicalHash y
        }

    let _lookaroundComparer: IEqualityComparer<struct (RegexNode<'t> * bool * int * Set<int>)> =
        { new IEqualityComparer<struct (RegexNode<'t> * bool * int * Set<int>)> with
            member this.Equals(struct (x1, y1, r1, k1), struct (x2, y2,  r2, k2)) =
                y1 = y2
                && r1 = r2
                && k1 = k2
                && refEq x1 x2

            member this.GetHashCode(struct (x, _, r, k)) =
                LanguagePrimitives.PhysicalHash x ^^^ r ^^^ LanguagePrimitives.PhysicalHash k
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

    let _combineLanguageComparer: IEqualityComparer<struct (RegexNode<'t> * RegexNode<'t >)> =
        { new IEqualityComparer<struct (RegexNode<'t> * RegexNode<'t>)> with
            member this.Equals(struct (x1, y1), struct (x2, y2)) = refEq x1 x2 && refEq y1 y2
            member this.GetHashCode(struct (x, y)) =
                LanguagePrimitives.PhysicalHash x ^^^ LanguagePrimitives.PhysicalHash y
        }

    let _canonicalCacheComparer: IEqualityComparer<struct(bool* bool * Memory<RegexNode<'t>>)> =
        { new IEqualityComparer<struct (bool*bool*Memory<RegexNode<'t>>)> with
            member this.Equals((dep1,n1,x1), (dep2,n2,x2)) =
                dep1=dep2 && n1 = n2 && x1.Span.SequenceEqual(x2.Span)
            member this.GetHashCode((_,_,x)) = Enumerator.getSharedHash3 x.Span
        }

    let _combineLanguageCache: Dictionary<struct (RegexNode<'t> * RegexNode<'t>), (RegexNode<'t> * RegexNode<'t>) option> = Dictionary(_combineLanguageComparer)

    let _canonicalCache: Dictionary<struct(bool*bool*Memory<RegexNode<'t>>), RegexNode<'t>> = Dictionary(_canonicalCacheComparer)

    let _orCache: Dictionary<RegexNode< 't >Memory, RegexNode< 't >> =
        Dictionary(_orCacheComparer)

    let _notCache: Dictionary<RegexNode< 't >, RegexNode< 't >> = Dictionary(_refComparer)
    let _lookaroundCache: Dictionary<struct (RegexNode< 't >*bool*int*Set<int>), RegexNode< 't >> = Dictionary(_lookaroundComparer)

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
                info =
                    _createInfo
                        (RegexNodeFlags.IsAlwaysNullableFlag ||| RegexNodeFlags.CanBeNullableFlag ||| RegexNodeFlags.HasZerowidthHeadFlag)
                        solver.Full
                        Set.empty

            )
        _truePlus =
            RegexNode.Loop(
                _true,
                low = 1,
                up = Int32.MaxValue,
                info = _createInfo RegexNodeFlags.None solver.Full Set.empty
            )
        _wordChar = lazy b.setFromStr @"\w"
        _nonWordChar = lazy b.setFromStr @"\W"
        _zAnchor = RegexNode<'t>.End
        _aAnchor = RegexNode<'t>.Begin
    |}

    do _loopCache.Add(struct(_true, 0, Int32.MaxValue), _uniques._trueStar)
    do _loopCache.Add(struct(_true, 1, Int32.MaxValue), _uniques._truePlus)
    do _singletonCache.Add(solver.Full,_true)
    do _singletonCache.Add(solver.Empty,_false)

    let _anchors =
        let nonWordLeft =
            lazy
                b.mkOrSeq([|
                    RegexNode<'t>.Begin;
                    b.mkLookaround( _uniques._nonWordChar.Value ,true, 0, Set.empty)
                |])
        let wordLeft =
            lazy
                b.mkOrSeq([|
                    RegexNode<'t>.Begin
                    b.mkLookaround( _uniques._wordChar.Value ,true, 0, Set.empty)
                |])
        let nonWordRight =
            lazy
                b.mkOrSeq(
                    [|RegexNode<'t>.End
                      b.mkLookaround( _uniques._nonWordChar.Value,false, 0, Set.empty) |]
                )
        let wordRight =
            lazy
                b.mkOrSeq(
                    [|RegexNode<'t>.End
                      b.mkLookaround( _uniques._wordChar.Value,false, 0, Set.empty) |]
                )

        {|
            _endZAnchor = lazy (failwith "todo: \Z anchor is not defined" : RegexNode<'t>)
            //                        b.mkOr([
            //     b.mkLookaround(_true,false,true)
            //     b.mkLookaround(b.mkConcat2(b.one '\n',_true),false,true)
            // ])
            _zAnchor = _uniques._zAnchor

            // \A ≡ (?<!⊤)
            _bigAAnchor = _uniques._aAnchor
                // __big_a_anchor
            // (?<=\A|\A\n) ≡ \a
            _aAnchor =
                lazy
                    let seqv =
                        [|
                            _uniques._aAnchor
                            b.mkConcat2(_uniques._aAnchor, b.one '\n')
                        |]
                    let node = b.mkOrSeq(seqv)
                    b.mkLookaround(node,true, 0, Set.empty)


            _nonWordBorder =
                lazy (failwith "nonwordborder"  : RegexNode<'t>)
                    // (?!ψ\w)
                    // let c1 = [
                    //     b.mkLookaround(_uniques._wordChar.Value,true,true)
                    //     // (?<!ψ\w)
                    //     b.mkLookaround(_uniques._wordChar.Value,false,true)
                    // ]
                    // // (?=ψ\w)
                    // let c2 = [
                    //     b.mkLookaround(_uniques._wordChar.Value,true,false)
                    //     // (?<=ψ\w)
                    //     b.mkLookaround(_uniques._wordChar.Value,false,false)
                    // ]
                    // b.mkOr(ofSeq [ b.mkConcat c1; b.mkConcat c2 ])


            _wordBorder =
                    lazy
                        b.mkOrSeq(
                        [|
                            b.mkConcat2(nonWordLeft.Value, wordRight.Value)
                            b.mkConcat2(wordLeft.Value, nonWordRight.Value)
                        |]
                    )
            // (?<=\W)
            // _nonWordLeft = lazy b.mkLookaround(_uniques._nonWordChar.Value,true,false)
            // proper definition (?<=\a|\W)
            _nonWordLeft =
                lazy
                    b.mkOrSeq([|
                        RegexNode<'t>.Begin
                        b.mkLookaround( _uniques._nonWordChar.Value ,true, 0, Set.empty)
                    |])
            // (?<=\W)
            _wordLeft =
                lazy
                    b.mkOrSeq([|
                        RegexNode<'t>.Begin
                        b.mkLookaround( _uniques._wordChar.Value ,true, 0, Set.empty)
                    |])
            // (?=\W)
            // _nonWordRight = lazy b.mkLookaround(_uniques._nonWordChar.Value)
            // proper definition (?=\z|\W)
            _nonWordRight =
                lazy
                    b.mkOrSeq(
                        [|RegexNode<'t>.End
                          b.mkLookaround( _uniques._nonWordChar.Value,false, 0, Set.empty) |]
                    )

                    // b.mkLookaround(
                    //     b.mkOr([ RegexNode<'t>.Anchor End; _uniques._nonWordChar.Value ])
                    //     ,true,false)
            _wordRight =
                lazy
                    b.mkOrSeq(
                        [|RegexNode<'t>.End
                          b.mkLookaround( _uniques._wordChar.Value,false, 0, Set.empty) |]
                    )

            // ^ ≡ \A|(?<=\n)
            _caretAnchor =
                // RegexNode<'t>.Anchor Bol
                lazy
                    b.mkOrSeq(
                        [|
                            _uniques._aAnchor
                            b.mkLookaround(b.one '\n',true, 0, Set.empty)
                        |]
                    )
            // ^ ≡ \z|(?=\n)
            _dollarAnchor =
                // RegexNode<'t>.Anchor Eol
                lazy
                    b.mkOrSeq(
                        [|
                            _uniques._zAnchor
                            b.mkLookaround(b.one '\n',false, 0, Set.empty)
                        |]
                    )

        |}

    let mutable _prefixCache: Dictionary<RegexNode<'t>, InitialStartset<'t>> =
        Dictionary(_refComparer)

    let mutable _uniquesDict : Dictionary<RegexNode<BDD>,RegexNode<'t>> = Dictionary<RegexNode<BDD>,RegexNode<'t>>(_bddrefComparer)



    member this.trueStar = _uniques._trueStar
    member this.uniques = _uniques
    member this.anchors = _anchors
    member this.PrefixCache = _prefixCache
    member this.LanguageCache = _combineLanguageCache

    member this.CanonicalCache = _canonicalCache
    member this.UniquesDict = _uniquesDict
    member val CanonicalizeCallback : (RegexNode<'t> -> RegexNode<'t>)  option = None with get, set

    member this.GetCanonical(oldNode:RegexNode<'t>, ders:Memory<RegexNode<'t>>, mknode: unit -> RegexNode<'t>) =
        // if refEq oldNode node then node else
        let key = struct(oldNode.DependsOnAnchor,oldNode.CanBeNullable , ders)

        match this.CanonicalCache.TryGetValue(key) with
        | true, v ->
            oldNode.TryGetInfo |> ValueOption.iter (fun inf ->
                inf.HasCanonicalForm <- Some v
            )
            v
        | _ ->
            let node = mknode()
            node.TryGetInfo
            |> ValueOption.iter (fun v -> v.IsCanonical <- true )
            this.CanonicalCache.Add(key, node)
            node


    member this.InitCanonical(minterms: 't array) =
        // if refEq oldNode node then node else
        let falseLang = minterms |> Array.map (fun v -> _uniques._false ) |> Memory
        this.CanonicalCache.Add(struct(false,false, falseLang),_uniques._false)
        this.CanonicalCache.Add(struct(false,true, falseLang),_uniques._eps)
        // --


    member this.TryGetCanonical(oldNode:RegexNode<'t>, ders:Memory<RegexNode<'t>>) =
        // if refEq oldNode node then node else
        let key = struct(oldNode.DependsOnAnchor,oldNode.CanBeNullable , ders)
        match this.CanonicalCache.TryGetValue(key) with
        | true, v ->
            oldNode.TryGetInfo |> ValueOption.iter (fun inf ->
                inf.HasCanonicalForm <- Some v
            )
            Some v
        | _ ->
            None

    member this.InitializeUniqueMap(oldBuilder:RegexBuilder<BDD>) =
        // _uniquesDict.Add(oldBuilder.anchors._aAnchor.Value,this.anchors._aAnchor.Value)
        // _uniquesDict.Add(oldBuilder.anchors._bigAAnchor,this.anchors._bigAAnchor)
        // _uniquesDict.Add(oldBuilder.anchors._caretAnchor.Value,this.anchors._caretAnchor.Value)
        // _uniquesDict.Add(oldBuilder.anchors._dollarAnchor.Value,this.anchors._dollarAnchor.Value)
        // _uniquesDict.Add(oldBuilder.anchors._wordBorder.Value,this.anchors._wordBorder.Value)
        // _uniquesDict.Add(oldBuilder.anchors._zAnchor,this.anchors._zAnchor)
        ()

    member this.setFromNode(node: RegexNode) =
        let bdd = converter.CreateBDDFromSetString(node.Str)
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
        let minterm = solver.ConvertFromBDD(a1, bcss)
        this.one(minterm)

    member this.one(minterm: 't) : RegexNode< 't > =
        let mt = minterm

        if solver.IsFull(mt) then
            _uniques._true
        elif solver.IsEmpty(mt) then
            _uniques._false
        else
            match _singletonCache.TryGetValue(mt) with
            | true, v -> v
            | _ ->
                let v = RegexNode.Singleton(mt)
                _singletonCache.Add(mt, v)
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
        // if nodes |> Seq.forall (_.IsAlwaysNullable)  then
        //     match tryFindV (function TrueStarredConcat solver tail as currNode -> ValueSome (currNode, tail) | _ -> ValueNone) nodes with
        //     | ValueSome (currNode,tail) ->
        //         if (nodes.Count = 2 && nodes.Contains(tail) ) then
        //             result <- ValueSome (Seq.singleton currNode)
        //         else ()
        //     | _ -> ()

        if result.IsSome then result.Value else

        match starLoops.Length > 0 with
        | true ->
            let struct (largestPred, largestStarLoop) =
                starLoops
                |> Seq.reduce (fun struct (e1, e1node) struct (e2, e2node) ->
                    let conj = solver.And(e1, e2)
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
                | TrySubsumeSameTail s -> Seq.singleton (unbox s) //:?> RegexNode<'t> seq
                | _ ->
                    let mergeTails =
                        nodes |> Seq.map (fun v ->
                            match v with
                            | Concat(regexNode, tail, regexNodeInfo) -> tail
                            | _ -> failwith "todo: subsumption bug"
                        )
                        |> this.mkOrSeq
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


    /// returns: Some (intersect lang, union lang) / None: no overlap
    // member this.combineLanguage(node1: RegexNode<'t>, node2: RegexNode<'t>) : (RegexNode<'t>*RegexNode<'t>) option =
    //     let hash1 = LanguagePrimitives.PhysicalHash node1
    //     let hash2 = LanguagePrimitives.PhysicalHash node2
    //     let key = if hash1 < hash2 then struct(node1,node2) else struct(node2,node1)
    //
    //     match this.LanguageCache.TryGetValue(key) with
    //     | true, v -> v
    //     | _ ->
    //
    //     let mergeUnion() = this.mkOr2Direct(node1,node2)
    //     let mergeIntersection() = this.mkAnd2Direct(node1,node2)
    //
    //     if refEq node1 node2 then Some (node1,node2) else
    //
    //     let result =
    //         match node1, node2 with
    //         // R&⊥ -> ⊥
    //         | other, falseNode
    //         | falseNode, other when refEq _uniques._false falseNode -> Some(falseNode,other)
    //         // R&⊤* -> R
    //         | other, tsnode
    //         | tsnode, other when refEq _uniques._trueStar tsnode -> Some(other,tsnode)
    //         // (T*.*&.*) -> .*
    //         | Concat(head=TrueStar(solver);tail=catTail) as catnode, other
    //         | other, (Concat(head=TrueStar(solver);tail=catTail) as catnode) when refEq catTail other ->
    //             Some (other, catnode)
    //         // ~(.*)&.*
    //         | n2,Not(node=n1)
    //         | Not(node=n1), n2 when refEq n1 n2 -> Some (_uniques._false, _uniques._trueStar)
    //         // \d\d⊤*&\d⊤* -> \d\d⊤*
    //         | Concat(head=cathead1;tail=TrueStar(solver)) as cat1, (Concat(head=cathead2;tail=TrueStar(solver)) as cat2)
    //         | (Concat(head=cathead2;tail=TrueStar(solver)) as cat2), (Concat(head=cathead1;tail=TrueStar(solver)) as cat1) ->
    //             match cathead1, cathead2 with
    //             | Singleton cat1pred, othernode ->
    //                 if solver.isElemOfSet(cat1pred,othernode.SubsumedByMinterm(solver)) then
    //                     Some(cat2,cat1)
    //                 else
    //                     None
    //             | othernode,Singleton cat2pred  ->
    //                 if solver.isElemOfSet(cat2pred,othernode.SubsumedByMinterm(solver)) then
    //                     Some(cat1,cat2)
    //                 else None
    //             | _ ->
    //                 failwith $"_subsume: {node1.ToString()} ;;; {node2.ToString()}"
    //
    //         // ε&.* -> ε
    //         | other,Epsilon
    //         | Epsilon, other ->
    //             match other.IsAlwaysNullable with
    //             | true -> Some(Epsilon, other)
    //             | false ->
    //                 if other.CanNotBeNullable then
    //                     Some(_uniques._false, this.mkOr2Direct(other,Epsilon))
    //                 else
    //                     None
    //         // (a*&.*) -> a*
    //         | SingletonStarLoop(p2) as p2node, (SingletonStarLoop(p1) as p1node)
    //         | (SingletonStarLoop(p1) as p1node),(SingletonStarLoop(p2) as p2node) ->
    //             if Solver.containsS solver p1 p2  then Some(p2node, p1node)
    //             elif Solver.containsS solver p2 p1  then Some(p1node, p2node)
    //             else None
    //         // (and .*|.* and .*)&.* -> (and .*|.* and .*)
    //         | other, (SingletonStarLoop(p1) as p1node)
    //         | (SingletonStarLoop(p1) as p1node),other ->
    //             let submt = other.SubsumedByMinterm(solver)
    //             // let w = solver.PrettyPrint(submt, Debug.debugcharSetSolver)
    //             if Solver.containsS solver p1 submt then
    //                 Some(other,p1node)
    //             else Some(mergeIntersection(),mergeUnion())
    //         // (.*hat.*|.*t.*hat.*) -> .*hat.*
    //         | other, (SingletonStarLoop(p1) as p1node)
    //         | (SingletonStarLoop(p1) as p1node),other ->
    //             if solver.isElemOfSet(p1,other.SubsumedByMinterm(solver)) then
    //                 Some(other,p1node)
    //             else Some(mergeIntersection(),mergeUnion())
    //
    //
    //
    //         // [a-z]&a -> a
    //         | Singleton p1 as p1node, (Singleton p2 as p2node) ->
    //             if Solver.containsS solver p1 p2  then Some(p2node, p1node)
    //             elif Solver.containsS solver p2 p1  then Some(p1node, p2node)
    //             else None
    //         // (s.*&.*) -> s.*
    //         | Concat(head=cathead;tail=cattail) as catnode, (Loop(node=_;low=0;up=Int32.MaxValue) as loopnode)
    //         | (Loop(node=_;low=0;up=Int32.MaxValue) as loopnode), (Concat(head=cathead;tail=cattail) as catnode) when refEq loopnode cattail ->
    //             Some (catnode, loopnode)
    //         // (.*&.*s) -> .*s
    //         | Concat(head=cathead;tail=_) as catnode, (Loop(node=_;low=0;up=Int32.MaxValue) as loopnode)
    //         | (Loop(node=_;low=0;up=Int32.MaxValue) as loopnode), (Concat(head=cathead;tail=_) as catnode) when refEq loopnode cathead ->
    //             Some (catnode, loopnode)
    //         // [a-z]&.*a -> a
    //         // a&.*b -> ⊥
    //         | Concat(head=StarLoop(Singleton p1 as p1head); tail = p1tail) as p1node, (Singleton p2 as p2node)
    //         | (Singleton p2 as p2node), (Concat(head=StarLoop(Singleton p1 as p1head); tail = p1tail) as p1node) ->
    //             // .* > [a-z]
    //             if Solver.containsS solver p1 p2  then
    //                 match this.combineLanguage(p1tail,p2node) with
    //                 | Some (smaller, larger) -> Some(smaller,this.mkOr2Direct(larger, p1node))
    //                 | _ -> None
    //             elif Solver.containsS solver p2 p1 then Some(p2node, p1node)
    //             else None
    //         // a&b.* -> ⊥
    //         // a&[a-z].* -> a
    //         | Concat(head=p1head; tail = StarLoop(Singleton p1 as p1tail)) as p1node, (Singleton p2 as p2node)
    //         | (Singleton p2 as p2node), (Concat(head=p1head; tail = StarLoop(Singleton p1 as p1tail)) as p1node) ->
    //             // .* > [a-z]
    //             if Solver.containsS solver p1 p2  then
    //                 match this.combineLanguage(p1head,p2node) with
    //                 | Some (smaller, larger) -> Some(smaller,this.mkOr2Direct(larger, p1node))
    //                 | _ -> None
    //             elif Solver.containsS solver p2 p1 then Some(p2node, p1node)
    //             else None
    //         // complex and subsumption
    //         | (And(nodes=nodes2) as and2),  (And(nodes=nodes1) as and1)  ->
    //             let diff1 = nodes1.Except(nodes2)
    //             let diff2 = nodes2.Except(nodes1)
    //             let inter = this.mkAnd(nodes2.Intersect(nodes1))
    //             if diff1.Count = 1 && diff2.Count = 1 then
    //                 let diffLang = this.combineLanguage(diff1 |> Seq.head,diff2 |> Seq.head)
    //                 match diffLang with
    //                 | Some (small,big) -> Some (this.mkAnd2(small,inter),this.mkAnd2(big,inter))
    //                 | _ -> Some(mergeIntersection(),mergeUnion())
    //             else
    //                 // let d2in1 = diff2.Count > 1 && diff2 |> Seq.forall  (Node.containsRecursive nodes1)
    //                 // let d1in2 = diff1.Count > 1 && diff1 |> Seq.forall  (Node.containsRecursive nodes2)
    //                 // if d2in1 then Some(and2,and1)
    //                 // elif d1in2 then Some(and1,and2) else
    //                 Some(mergeIntersection(),mergeUnion())
    //         | (Or(nodes=nodes2) as p2node),  (Or(nodes=nodes1) as p1node)  ->
    //             // let mutable found = false
    //             // check for subset equality
    //             let diff1 = nodes1.Except(nodes2)
    //             let diff2 = nodes2.Except(nodes1)
    //             if diff1.Count = 1 && diff2.Count = 1 then
    //                 let inter = this.mkOrSeq(nodes2.Intersect(nodes1))
    //                 let diffLang = this.combineLanguage(diff1 |> Seq.head,diff2 |> Seq.head)
    //                 match diffLang with
    //                 | Some (small,big) -> Some (this.mkOr2(small,inter),this.mkOr2(big,inter))
    //                 | _ -> Some(mergeIntersection(),mergeUnion())
    //             else
    //                 Some(mergeIntersection(),mergeUnion())
    //
    //         // (⊤*A⊤*&⊤*B⊤*)&⊤*B⊤* -> ⊤*A⊤*&⊤*B⊤*
    //         | other, (And(nodes=nodes) as andnode)
    //         | (And(nodes=nodes) as andnode), other ->
    //             match nodes.Contains(other) with
    //             | true -> Some(andnode, other)
    //             | false -> Some(mergeIntersection(),mergeUnion())
    //         // ((.*s|ε)&.*s) -> .*s
    //         // ([a-z]|ε)&a -> a
    //         | other, (Or(nodes=nodes) as ornode)
    //         | (Or(nodes=nodes) as ornode), other ->
    //             match nodes.Contains(other) with
    //             | true -> Some(other, ornode)
    //             | false ->
    //                 let removedEps =
    //                     if other.CanNotBeNullable && nodes.Contains(_uniques._eps) then
    //                         this.mkOrSeq((nodes.Remove(_uniques._eps)))
    //                     else ornode
    //                 Some(this.mkAnd2Direct(removedEps,other),this.mkOr2Direct(ornode,other))
    //
    //         // ⊤*s.*&.* -> s.*
    //         // (⊤*B⊤*&⊤*A⊤*) -> both
    //         // TODO: BUG
    //         // | (Concat(head=TrueStar(solver);tail=catTail) as catnode), other
    //         // | other, (Concat(head=TrueStar(solver);tail=catTail) as catnode) ->
    //         //     if refEq catTail other then Some (other, catnode) else // already covered
    //         //     // (⊤*a&.)
    //         //     match catTail, other with
    //         //     | Singleton catTailPred, othernode ->
    //         //         if solver.isElemOfSet(catTailPred,othernode.SubsumedByMinterm(solver)) then
    //         //             Some(catnode, this.mkOr2Direct(node1,node2))
    //         //             // Some(catTail, this.mkOr2Direct(node1,node2))
    //         //         else
    //         //             Some(this.mkAnd2Direct(node1,node2),this.mkOr2Direct(node1,node2))
    //         //     | _ ->
    //         //
    //         //         Some(this.mkAnd2Direct(node1,node2),this.mkOr2Direct(node1,node2))
    //
    //         // concat same head merge
    //         | Concat(head=chead1;tail=ctail1) as c1, (Concat(head=chead2;tail=ctail2) as c2)
    //         | (Concat(head=chead2;tail=ctail2) as c2),(Concat(head=chead1;tail=ctail1) as c1) when
    //             refEq chead1 chead2 ->
    //             Some(mergeIntersection(), this.mkConcat2(chead1,this.mkOr2Direct(ctail1,ctail2)) )
    //         // node1 = (?<=(⊤*,}.*&(⊤*φ|ε)))
    //         //         (?<=((⊤*,}.*|.*)&(⊤*φ|ε)))
    //         | LookAround(node=lbody1;lookBack= true) as l1, (LookAround(node=lbody2;lookBack= true) as l2) ->
    //             match this.combineLanguage(lbody1, lbody2) with
    //             | Some (small,big) ->
    //                 Some (this.mkLookaround(small, true, 0, Set.empty),
    //                       this.mkLookaround(big, true, 0, Set.empty))
    //             | _ ->
    //                 Some(mergeIntersection(),mergeUnion())
    //                 // failwith "merge look"
    //
    //         // unoptimized cases ------------------------------
    //         | _, (Begin|End)
    //         | (Begin|End), _ -> Some(mergeIntersection(),mergeUnion())
    //         // | Not _, _ | _, Not _ -> None
    //         | n1,n2 ->
    //             // derivative-based subsumption
    //             let fixlen1 = Node.getFixedLength n1
    //             let fixlen2 = Node.getFixedLength n2
    //             match (n1,fixlen1), (n2,fixlen2) with
    //             | (_,Some n1len), (_,Some n2len) when n1len <> n2len -> None
    //             | (n2,None),(n1,Some n1len)
    //             | (n1,Some n1len), (n2,None) ->
    //                 let minlen = getMinLength n2
    //                 match minlen with
    //                 | Some ml when n1len < ml -> None
    //                 | _ ->
    //                     Some(mergeIntersection(),mergeUnion())
    //                     // failwith $"_subsume: {node1.ToString()} ;;; {node2.ToString()}"
    //             | _ ->
    //                 // (A⊤*&⊤*B⊤*)
    //                 Some(mergeIntersection(),mergeUnion())
    //                 // failwith $"_subsume: {node1.ToString()} ;;; {node2.ToString()}"
    //
    //
    //     this.LanguageCache.Add(key, result)
    //     result



    member this.mkOr2Direct (node1: RegexNode<'t>, node2: RegexNode<'t>) : RegexNode<'t> =
        let key = [|node1;node2|]
        key.AsSpan().Sort(physComparison)
        let keymem = key.AsMemory()
        // Array.sortInPlaceBy LanguagePrimitives.PhysicalHash key
        match _orCache.TryGetValue(keymem) with
        | true, v -> v
        | _ ->
            // actually creates it without trying to subsume
            match key with
            | _ when key.Length = 0 -> _uniques._false
            | _ when key.Length = 1 -> (key[0])
            | twoormore ->
                let twoormore = twoormore.ToArray()
                let flags = Flags.inferOr (twoormore)
                let minterms2 =
                    twoormore
                    |> Seq.map (_.SubsumedByMinterm(solver))
                    |> Seq.fold (fun acc v -> solver.Or(acc,v) ) solver.Empty
                let inner = twoormore |> Seq.map (_.PendingNullables) |> Set.unionMany
                let mergedInfo =
                    this.CreateInfo(flags, minterms2, inner)
                let n = RegexNode.Or(ofSeq twoormore, mergedInfo)
                _orCache.Add(key,n)
                n

    member this.mkAnd2Direct (node1: RegexNode<'t>, node2: RegexNode<'t>) : RegexNode<'t> =
        let key = [|node1;node2|]
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash key
        match _andCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            match key with
            | _ when key.Length = 0 -> _uniques._trueStar
            | _ when key.Length = 1 -> key[0]
            | twoormore ->
                let flags = Flags.inferAnd twoormore
                let minterms2 =
                    twoormore
                    |> Seq.map (_.SubsumedByMinterm(solver))
                    |> Seq.fold (fun acc v -> solver.Or(acc,v) ) solver.Empty
                let inner = twoormore |> Seq.map (fun v -> v.PendingNullables) |> Set.unionMany
                let mergedInfo = this.CreateInfo(flags, minterms2,inner)
                match twoormore with
                | _ when twoormore.Length = 0 -> _uniques._trueStar
                | _ when twoormore.Length = 1 -> twoormore[0]
                | _ ->
                let newAnd = RegexNode.And(ofSeq twoormore, mergedInfo)
                _andCache.Add(key,newAnd)
                newAnd


    // member this.trySubsumeHead(singletonLoop: RegexNode<'t>, tail: RegexNode<'t>) : RegexNode<'t> =
    //     match tail with
    //     | Concat(head=Or(nodes=nodes) as head;tail=ctail) ->
    //         match head.IsAlwaysNullable with
    //         | true ->
    //             let langs =
    //                 nodes
    //                 |> Seq.map (fun v -> this.combineLanguage(singletonLoop, v) )
    //                 |> Seq.toArray
    //             let allcontained =
    //                 langs |> Seq.forall (fun v -> v.IsSome && refEq (snd v.Value) singletonLoop )
    //             if allcontained then
    //                 this.trySubsumeHead(singletonLoop, ctail)
    //             else
    //                 this.mkConcat2(singletonLoop, tail)
    //         | _ ->
    //             this.mkConcat2(singletonLoop, tail)
    //     | _ ->
    //         this.mkConcat2(singletonLoop, tail)


    member this.mkOr2 (node1: RegexNode<'t>, node2: RegexNode<'t>) : RegexNode<'t> =
        let key = [|node1;node2|]
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash key

        // actually creates it without trying to subsume
        let createCached(nodes: RegexNode< 't >[]) =
            let flags = Flags.inferOr nodes
            let minterms2 =
                nodes
                |> Seq.map (_.SubsumedByMinterm(solver))
                |> Seq.fold (fun acc v -> solver.Or(acc,v) ) solver.Empty
            let inner = nodes |> Seq.map (fun v -> v.PendingNullables) |> Set.unionMany
            let mergedInfo =
                this.CreateInfo(flags, minterms2,inner)
            let n = RegexNode.Or(ofSeq nodes, mergedInfo)
            _orCache.Add(key,n)
            n

        let addToCache(result: RegexNode< 't >) =
            _orCache.TryAdd(key,result) |> ignore
            result

        match _orCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            // bdd solver cannot be used here
            if typeof<'t> = typeof<BDD> then createCached(key) else
            match node1, node2 with
            | n, falseNode | falseNode, n when refEq falseNode _false -> n
            | n, trueStarNode | trueStarNode, n when refEq _uniques._trueStar trueStarNode -> trueStarNode
            | n, n2 when refEq n n2 -> n
            | Epsilon, n | n, Epsilon -> this.mkLoop(n, 0, 1)
            | Loop(node=node;low=2;up=2), other | other, Loop(node=node;low=2;up=2) when refEq node other ->
                this.mkLoop(node, 1, 2)
            | Or(nodes=nodes) as orNode, other | other, (Or(nodes=nodes) as orNode) ->
                if nodes.Contains(other) then orNode else
                let arr = seq {yield! nodes;other}
                let merged = this.mkOrSeq(arr)
                merged

            // merge head
            | Concat(head=chead1;tail=ctail1) as c1, (Concat(head=chead2;tail=ctail2) as c2)
            | (Concat(head=chead2;tail=ctail2) as c2),(Concat(head=chead1;tail=ctail1) as c1) when
                refEq chead1 chead2 ->
                let newtail = this.mkOr2(ctail1,ctail2)
                let v = this.mkConcat2(chead1, newtail)
                addToCache v
            // ⊤*.*t(ε|.*t) ->
            | Concat(head=chead1;tail=ctail1) as c1, (Concat(head=chead2;tail=ctail2) as c2)
            | (Concat(head=chead2;tail=ctail2) as c2),(Concat(head=chead1;tail=ctail1) as c1) when
                refEq chead1 chead2 ->
                let newtail = this.mkOr2(ctail1,ctail2)
                let v = this.mkConcat2(chead1, newtail)
                addToCache v
            // Huck[a-zA-Z]+|Saw[a-zA-Z]+ -> (Huck|Saw)([A-Za-z])+
            // | Concat(head=h1;tail=t1) as node1, (Concat(head=h2;tail=t2) as node2) ->
            //     let list1 = (this.getConcatList node1) |> Seq.toArray
            //     let list2 = (this.getConcatList node2) |> Seq.toArray
            //     let list1rev = list1 |> Array.rev
            //     let list2rev = list2 |> Array.rev
            //     let mergedSuffix =
            //         Seq.zip list1rev list2rev
            //         |> Seq.takeWhile (fun (v1,v2) ->
            //             refEq v1 v2
            //         )
            //         |> Seq.toArray
            //     let remainingHeadNodes1 = list1[0..(list1.Length - (mergedSuffix.Length + 1))]
            //     let remainingHeadNodes2 = list2[0..(list2.Length - (mergedSuffix.Length + 1))]
            //     match mergedSuffix with
            //     | [| |] -> createCached(key)
            //     | suff ->
            //         let conc1 = this.mkConcat(List.ofSeq remainingHeadNodes1)
            //         let conc2 = this.mkConcat(List.ofSeq remainingHeadNodes2)
            //         let sufflist =
            //             suff
            //             |> Seq.rev
            //             |> Seq.map fst
            //             |> Seq.toList
            //             |> this.mkConcat
            //         let newNode =
            //             this.mkConcat2(
            //                 this.mkOr2(conc1,conc2),
            //                 sufflist
            //             )
            //         let v = newNode
            //         addToCache v
            | _ ->
                // match this.combineLanguage(node1, node2) with
                // | Some (smaller,larger) -> addToCache larger
                // | _ ->
                createCached(key)


    member this.mkAnd2 (node1: RegexNode<'t>, node2: RegexNode<'t>) : RegexNode<'t> =
        let key = [|node1;node2|] |> Seq.toArray
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash key

        let addToCache(result: RegexNode< 't >) =
            _andCache.TryAdd(key,result) |> ignore
            result

        let createCached(nodes: RegexNode<_>[]) =
            match nodes with
            | _ when nodes.Length = 0 -> _uniques._trueStar
            | _ when nodes.Length = 1 -> nodes[0]
            | twoormore ->
                let flags = Flags.inferAnd twoormore
                let minterms2 =
                    twoormore
                    |> Seq.map (_.SubsumedByMinterm(solver))
                    |> Seq.fold (fun acc v -> solver.Or(acc,v) ) solver.Empty
                let inner = twoormore |> Seq.map (fun v -> v.PendingNullables) |> Set.unionMany
                let mergedInfo =
                    this.CreateInfo(flags, minterms2, inner)
                //
                match twoormore with
                | _ when twoormore.Length = 0 -> _uniques._trueStar
                | _ when twoormore.Length = 1 -> twoormore[0]
                | _ ->
                let newAnd = RegexNode.And(ofSeq twoormore, mergedInfo)
#if CANONICAL
                let newAnd =
                    this.CanonicalizeCallback
                    |> Option.map (fun fn -> fn newAnd )
                    |> Option.defaultValue newAnd
#endif
                _andCache.TryAdd(key,newAnd) |> ignore
                newAnd

        match _andCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            if typeof<'t> = typeof<BDD> then createCached(key) else

            match node1, node2 with
            | n, falseNode | falseNode, n when refEq falseNode _false -> falseNode
            | n, trueStarNode | trueStarNode, n when refEq _uniques._trueStar trueStarNode -> n

            // Lookaround semantics hack - bad
            // | LookAround(lookBack=true) as look , other
            // | other , (LookAround(lookBack=true) as look) ->
            //     this.mkConcat2(look, other)
            // | Concat(head=LookAround(lookBack=true); tail=looktail) as look , other
            // | other, (Concat(head=LookAround(lookBack=true); tail=looktail) as look) ->
            //     this.mkConcat2(look, this.mkAnd2(looktail,other))

            // | (And(nodes=nodes2) as p2node),  (And(nodes=nodes1) as p1node)  ->
                // let mutable found = false
                // check for subset equality
                // failwith "todo and sub"
                // let diff1 = nodes1.Except(nodes2)
                // let diff2 = nodes2.Except(nodes1)
                // let inter = this.mkAnd(nodes2.Intersect(nodes1))
                // if diff1.Count = 1 && diff2.Count = 1 then
                //     let diffLang = this.combineLanguage(diff1 |> Seq.head,diff2 |> Seq.head)
                //     match diffLang with
                //     | Some (small,big) -> Some (this.mkAnd2(small,inter),this.mkAnd2(big,inter))
                //     | _ -> Some(mergeIntersection(),mergeUnion())
                // else
                // // TODO: optimization potential
                // Some(mergeIntersection(),mergeUnion())
            | And(nodes=nodes) as andNode, other | other, (And(nodes=nodes) as andNode) ->
                if nodes.Contains(other) then andNode else
                let merged = this.mkAnd([|yield! nodes;other|])
                merged
            | _ ->
                createCached(key)
            // match this.combineLanguage(node1, node2) with
            // | Some (smaller,larger) -> addToCache smaller
            // | _ -> _uniques._false


    member this.mkAndDirect
        (
            nodes: RegexNode<'t> seq
        ) : RegexNode<'t> =

        let key = nodes |> Seq.toArray
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash key

        match _andCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let flags = Flags.inferAnd key
            let minterms2 =
                key
                |> Seq.map (_.SubsumedByMinterm(solver))
                |> Seq.fold (fun acc v -> solver.Or(acc,v) ) solver.Empty
            let inner = key |> Seq.map (fun v -> v.PendingNullables) |> Set.unionMany
            let mergedInfo =
                this.CreateInfo(flags, minterms2,inner)
            //
            match key with
            | _ when key.Length = 0 -> _uniques._trueStar
            | _ when key.Length = 1 -> key[0]
            | _ ->
            let newAnd = RegexNode.And(ofSeq key, mergedInfo)
            _andCache.Add(key,newAnd)
            newAnd

    member this.mkAnd
        (
            nodes: RegexNode<'t> seq
        ) : RegexNode<'t> =

        let key = nodes |> Seq.toArray
        Array.sortInPlaceBy LanguagePrimitives.PhysicalHash key

        match _andCache.TryGetValue(key) with
        | true, v -> v
        | _ ->


#if SUBSUME
        if key.Length = 2 then this.mkAnd2(key[0],key[1]) else
#endif

        let mutable enumerating = true
        let mutable status = MkAndFlags.None
        let derivatives = ResizeArray()
        // let prefixes = ResizeArray()
        // let suffixes = ResizeArray()
        use mutable e = nodes.GetEnumerator()
        while e.MoveNext() = true && enumerating do
            let rec handleNode deriv =
                match deriv with
                | _ when obj.ReferenceEquals(deriv, _uniques._trueStar) -> ()
                | _ when obj.ReferenceEquals(deriv, _uniques._false) ->
                    enumerating <- false
                    status <- MkAndFlags.IsFalse
                | And(nodes, _) ->
                    for node in nodes do
                        handleNode node
                | Epsilon -> status <- MkAndFlags.ContainsEpsilon
                | _ -> derivatives.Add(deriv)
            handleNode e.Current

        if status.HasFlag(MkAndFlags.ContainsEpsilon) then
            derivatives.Add(_uniques._eps)

        match status with
        | MkAndFlags.IsFalse -> _uniques._false
        | MkAndFlags.ContainsEpsilon when derivatives.Exists(fun v -> v.CanNotBeNullable) ->
            _uniques._false
        | _ ->
            // if prefixes.Count > 0 || suffixes.Count > 0 then
            //     let ps = this.mkConcat(Seq.toList prefixes)
            //     let ss = this.mkConcat(Seq.toList suffixes)
            //     let inner = this.mkAnd(derivatives)
            //     let combined = this.mkConcat([ps;inner;ss])
            //     _andCache.Add(key, combined)
            //     combined
            // else

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
                    let inner = twoormore |> Seq.map (fun v -> v.PendingNullables) |> Set.unionMany
                    let mergedInfo =
                        this.CreateInfo(flags, minterms2,inner)
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

                let v = createNode asArray

                _andCache.Add(key, v)
                v

    member this.mkOrSeq(
            nodes: RegexNode<'t> seq
        ) : RegexNode<_> =
        let mutable pool = ArrayPool<RegexNode<'t>>.Shared
        let mutable limit = 16
        let mutable rentedArray = pool.Rent(limit)
        use mutable e = nodes.GetEnumerator()
        let mutable i = 0
        while e.MoveNext() do
            if i >= limit then
                let newLimit = limit * 2
                let newRentedArray = pool.Rent(newLimit)
                rentedArray.AsSpan().CopyTo(destination=newRentedArray.AsSpan())
                pool.Return(rentedArray)
                rentedArray <- newRentedArray
                limit <- newLimit
            rentedArray[i] <- e.Current
            i <- i + 1
        if i = 0 then _uniques._false else
        let mem = rentedArray.AsMemory(0,i)
        mem.Span.Sort(physComparison)
        let res = this.mkOr(&mem)
        pool.Return(rentedArray)
        res

    member this.mkOr
        (
            nodes: inref<RegexNode<'t>Memory>
        ) : RegexNode<_> =
        let key = nodes
        match _orCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
#if SUBSUME
        if key.Length = 2 then this.mkOr2(key.Span[0],key.Span[1]) else
#endif

        let mutable enumerating = true
        let mutable status = MkOrFlags.None
        let mutable zeroloops = 0
        let mutable singletonLoops = 0
        let mutable e = nodes.Span.GetEnumerator()
        let derivatives = HashSet(_refComparer)

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
                | Epsilon -> status <- status ||| MkOrFlags.ContainsEpsilon
                | _ -> derivatives.Add(deriv) |> ignore

            handleNode e.Current

        match status with
        | MkOrFlags.IsTrueStar -> _uniques._trueStar
        | _ ->
        // let allSingletons() =
        //     nodes
        //     |> Seq.forall (function Singleton _ -> true | _ -> false )
        // if derivatives.Any() && allSingletons() then
        //     let merged =
        //         derivatives
        //         |> Seq.map (function Singleton pred -> pred | _ -> failwith "invalid case" )
        //         |> Seq.fold (fun v v2 -> solver.Or(v,v2) ) solver.Empty
        //     this.one(merged)
        // else

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
                    let inner = twoormore |> Seq.map (fun v -> v.PendingNullables) |> Set.unionMany

                    let mergedInfo =
                        this.CreateInfo(flags, minterms2,inner)

                    RegexNode.Or(ofSeq twoormore, mergedInfo)

            match _orCache.TryGetValue(nodeSet) with
            | true, v -> v
            | _ ->
                let v = createNode nodeSet
                let newArr = Array.zeroCreate key.Length
                for i = 0 to key.Length - 1 do
                    newArr[i] <- key.Span[i]
                _orCache.Add(nodeSet, v)
                v

    member this.mkNot(inner: RegexNode< 't >) =
            let createNode(inner: RegexNode< 't >) =
                match inner with
                | _ when refEq _uniques._false inner -> _uniques._trueStar // ~(⊥) -> ⊤*
                | StarLoop _ -> _uniques._false // ~(R*) -> ⊥
                | Epsilon -> _uniques._truePlus // ~(ε) -> ⊤+
                | _ ->
                    let mutable flags = Flags.inferNot inner
                    Not(inner, this.CreateInfo(flags, inner.SubsumedByMinterm(solver),inner.PendingNullables))

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

    member this.bddFromClass(setClass: string) =
        let bdd = converter.CreateBDDFromSetString(setClass)
        bdd

    member this.bddFromSetString(setPattern: string) = converter.CreateBDDFromSetString(setPattern)

    member this.mkConcat2(head: RegexNode< 't >, tail: RegexNode< 't >) : RegexNode< 't > =

        // wont pollute the cache with these
        match head, tail with
        | Epsilon, _ -> tail // ()R -> R
        | _, Epsilon -> head
        // redundant anchor branches
        | End, tail when tail.CanNotBeNullable -> _uniques._false
        | Begin, tail when tail.CanNotBeNullable -> _uniques._false
        | _ when refEq head _uniques._false -> _uniques._false // ⊥R -> ⊥
        | _ ->
            let key = struct (head, tail)

            let createCached(head,tail) =
                let flags = Flags.inferConcat head tail
                let mergedMinterms = solver.Or(head.SubsumedByMinterm(solver),tail.SubsumedByMinterm(solver))
                let info = this.CreateInfo(flags, mergedMinterms,Set.union head.PendingNullables tail.PendingNullables)
                let node = Concat(head, tail, info)
                _concatCache.Add(key,node)
                node

            match _concatCache.TryGetValue(key) with
            | true, v -> v
            | _ ->
                match head, tail with
                | SingletonStarLoop(p1), SingletonStarLoop(p2) ->
                    if Solver.containsS solver p1 p2 then head
                    elif Solver.containsS solver p2 p1 then tail
                    else createCached(head,tail)
                | SingletonStarLoop(p1), Concat(head=SingletonStarLoop(p2);tail=ctail) ->
                    if Solver.containsS solver p1 p2 then this.mkConcat2(head,ctail)
                    elif Solver.containsS solver p2 p1 then this.mkConcat2(tail,ctail)
                    else createCached(head,tail)
                // normalize
                | Concat(head=h1;tail=h2), tail ->
                    let merged = this.mkConcat2(h1,this.mkConcat2(h2,tail))
                    _concatCache.Add(key, merged)
                    merged
                // (?<=a.*)(?<=\W)aa to (?<=⊤*a.*&⊤*\W)aa
                | LookAround(node=node1;lookBack=true), Concat(head=LookAround(node=node2;lookBack=true;);tail=tail2) ->
                    let combined = this.mkAnd([
                        this.mkConcat2(this.trueStar,node1)
                        this.mkConcat2(this.trueStar,node2)
                    ])
                    let v = this.mkLookaround(combined, true, 0, Set.empty)
                    let v = this.mkConcat2(v, tail2)
                    _concatCache.Add(key, v)
                    v
                // (?<=.*).* to .*
                | LookAround(node=SingletonStarLoop(pred) as look;lookBack=true), other when refEq look tail ->
                    let v = tail
                    _concatCache.Add(key, v)
                    v
                // // (?<=.*).*ab to .*ab
                | LookAround(node=SingletonStarLoop(pred) as look;lookBack=true), Concat(head=chead;tail=ctail) when refEq look chead ->
                    let v = tail
                    _concatCache.Add(key, v)
                    v
                // (?=a.*)(?=\W) to (?=a.*⊤*&\W⊤*)
                | LookAround(node=node1;lookBack=false; relativeTo = rel1; pendingNullables = pending1),
                    LookAround(node=node2;lookBack=false; relativeTo = rel2; pendingNullables = pending2) ->
                    assert pending2.IsEmpty
                    let combined = this.mkAnd([
                        this.mkConcat2(node1,this.trueStar)
                        this.mkConcat2(node2,this.trueStar)
                    ])
                    let v = this.mkLookaround(combined, false, rel1, pending1) // pass pending nullables
                    _concatCache.Add(key, v)
                    v
                | LookAround(node=Epsilon;lookBack=false;pendingNullables = _), tail when not tail.IsAlwaysNullable ->
                    // Experimental
                    let v =
                        let flags = Flags.inferConcat head tail
                        let mergedMinterms = solver.Or(head.SubsumedByMinterm(solver),tail.SubsumedByMinterm(solver))
                        let info = this.CreateInfo(flags, mergedMinterms, Set.union head.PendingNullables tail.PendingNullables)
                        Concat(head, tail, info)
                    _concatCache.Add(key, v)
                    v
                // INNER LOOKAROUND
#if REWRITE_INNER
                // TODO: ⊤*(?<=aaa) not correct - this does not have the same semantics
                // a(?<=[a-z]) to (a&(⊤*[a-z])
                | head, LookAround(node=node;lookBack=true) ->
                    // only rewrite trivial examples!
                    let v = b.mkAnd([
                        head
                        b.mkConcat2(b.trueStar,node)
                    ])
                    _concatCache.Add(key, v)
                    v
                // (?=[a-z])a to (a&(⊤*[a-z])
                | LookAround(node=node;lookBack=false),tail  ->
                    // only rewrite trivial examples!
                    let v = b.mkAnd([
                        tail
                        b.mkConcat2(node,b.trueStar)
                    ])
                    _concatCache.Add(key, v)
                    v
#else
                | LookAround(node=_;lookBack=false),other | other, LookAround(node=_;lookBack=true) when not (refEq other _uniques._trueStar ) ->
                    failwith "Sbre does not support inner lookarounds"
#endif
                | _ ->
                    let v =
                        let flags = Flags.inferConcat head tail
                        let mergedMinterms = solver.Or(head.SubsumedByMinterm(solver),tail.SubsumedByMinterm(solver))
                        let info = this.CreateInfo(flags, mergedMinterms, Set.union head.PendingNullables tail.PendingNullables)
                        Concat(head, tail, info)

                    _concatCache.Add(key, v)
                    v


    member this.mkLookaround(body: RegexNode< 't >, lookBack:bool, rel:int, pendingNullable:Set<int>) : RegexNode< 't > =
        let key = struct (body, lookBack, rel, pendingNullable)
        match _lookaroundCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let newNode =
                match body, lookBack with
                | Epsilon, true -> _uniques._eps
                | _, true when refEq _uniques._trueStar body -> _uniques._eps
                | _, false when refEq _uniques._trueStar body ->
                    let flags = Flags.inferLookaround body lookBack
                    let pendingNull =
                        if body.CanBeNullable then pendingNullable |> Set.map (fun v -> v + rel)
                        else Set.empty
                    let info = this.CreateInfo(flags, solver.Full, pendingNull)
                    LookAround(_uniques._eps,lookBack, rel,pendingNullable,info)
                | _ when refEq _uniques._false body -> _uniques._false
                | _ ->
                    let flags = Flags.inferLookaround body lookBack
                    let pendingNull =
                        if body.CanBeNullable then
                            pendingNullable |> Set.map (fun v -> v + rel)
                        else Set.empty
                    let info = this.CreateInfo(flags, solver.Full, pendingNull)
                    LookAround(body,lookBack, rel,pendingNullable,info)
            _lookaroundCache.Add(key, newNode)
            newNode

    member this.stripSuffixes(node: RegexNode< 't >)  =
        match node with
        | Concat(head=LookAround(lookBack = true) as look; tail=tail) -> node, []
        | Concat(head=head; tail=LookAround(lookBack = false) as look) -> head, [look]
        | Concat(head=head; tail=Concat(head=LookAround(lookBack = false) as look1; tail=tail)) ->
            let innerHead,innerSuffixes = this.stripSuffixes tail
            assert (innerHead = Epsilon)
            head, look1 :: innerSuffixes
        | Concat(head=head; tail=tail) ->
            let hd2,suf = this.stripSuffixes tail
            this.mkConcat2(head,hd2), suf
        | LookAround(lookBack=false) -> this.uniques._eps, [node]
        | _ -> failwith "todo suffix"

    member this.getConcatList(node: RegexNode< 't >)  =
        let acc = ResizeArray()
        let rec loop node =
            match node with
            | Concat(head=head; tail=tail) ->
                acc.Add(head)
                loop tail
            | node -> acc.Add(node)
        loop node
        acc
    // member this.stripTail(node: RegexNode< 't >)  =
    //     let revNodes = this.getConcatList node
    //     this.mkConcat(revNodes |> Seq.take (revNodes.Count-2) |> Seq.toList),
    //     revNodes[revNodes.Count-1]


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
                        | _ -> this.mkConcat2 (v, acc)
                    )
                    Epsilon
            combined

    member this.mkLoop(body: RegexNode< 't >, lower: int, upper: int) =
        let createNode(struct (body: RegexNode< 't >, lower: int, upper: int)) =
            match body, lower, upper with
            | _, 0, 0 -> Epsilon
            | _, 1, 1 -> body
            | _ ->
                let flags = Flags.inferLoop (body, lower, upper)
                let info = b.CreateInfo(flags, body.SubsumedByMinterm(solver),body.PendingNullables)
                RegexNode.Loop(body, lower, upper, info = info)

        let key = struct (body, lower, upper)

        match _loopCache.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let v = createNode key
            _loopCache.Add(key, v)
            v

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateInfo(flags: RegexNodeFlags, containsMinterms:'t, nullables:Set<int>) : RegexNodeInfo<_> =
        _createInfo flags containsMinterms nullables



// trivia:
// for correct .NET semantics
// https://github.com/dotnet/runtime/blob/1fe9c0bba15e23b65be007ddf38c43d28b2f9dd2/src/libraries/System.Text.RegularExpressions/src/System/Text/RegularExpressions/Symbolic/UnicodeCategoryConditions.cs#L67
// member this.wordCharForWordBorder = this.setFromStr "[\w\u200C\u200D]"
