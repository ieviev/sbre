namespace Sbre

open System
open System.Buffers
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open System.Text.RuntimeRegexCopy
open Microsoft.FSharp.Core
open Sbre.Algorithm
open Sbre.Optimizations
open Sbre.Types
open Sbre.Pat
open System.Runtime.InteropServices
open Sbre.Common
open Sbre.Cache



[<Sealed>]
type internal MatchState<'t when 't :> IEquatable<'t> and 't: equality>(node: RegexNode<'t>) =
    member val Id = -1 with get, set
    member val Node = node with get, set
    member val Startset: 't = Unchecked.defaultof<'t> with get, set
    member val Flags: RegexStateFlags = RegexStateFlags.None with get, set

    // -- optimizations
    member val PendingNullablePositions: Memory<int> = Unchecked.defaultof<_> with get, set

    // member val PendingNullablePositions: Set<int> = Set.empty with get, set
    member val ActiveOptimizationsChar: ActiveBranchOptimizations<'t, char> =
        ActiveBranchOptimizations.NoOptimizations with get, set

    member val ActiveOptimizationsByte: ActiveBranchOptimizations<'t, byte> =
        ActiveBranchOptimizations.NoOptimizations with get, set
    member val MintermSearchValues: MintermSearchValues<'t> = Unchecked.defaultof<_> with get, set


[<Sealed>]
type RegexMatcher<'t when 't: struct and 't :> IEquatable<'t> and 't: equality>
    (uncanonicalizedNode: RegexNode<'t>, _cache: RegexCache<'t>, options: SbreOptions) =
    inherit GenericRegexMatcher()
    let InitialDfaStateCapacity = options.InitialDfaCapacity
    let _stateCache = Dictionary<RegexNode<'t>, MatchState<'t>>()
    let mutable _stateArray = Array.zeroCreate<MatchState<'t>> InitialDfaStateCapacity
    let mutable _flagsArray = Array.zeroCreate<RegexStateFlags> InitialDfaStateCapacity
    let mutable _stateMem = _stateArray.AsMemory()
    let mutable _flagsMem = _flagsArray.AsMemory()
    let _minterms: 't[] = _cache.Minterms()
    let _ascii: int[] = _cache.Ascii
    let _nonAscii: BDD = _cache.NonAscii
    let _mintermsLog = BitOperations.Log2(uint64 _minterms.Length) + 1

    let mutable _dfaDelta: int[] =
        Array.init (InitialDfaStateCapacity <<< _mintermsLog) (fun _ -> 0) // 0 : initial state

    let mutable _revStartStates: int[] = Array.init (10 <<< _mintermsLog) (fun _ -> 0) // 0 : initial state

    let _nonInitialIsNullable (node: RegexNode<'t>) =
        if node.CanNotBeNullable then false
        elif node.IsAlwaysNullable then true
        else false

    let rec _isNullable(loc: inref<Location<_>>, node: RegexNode<'t>) : bool =
        // short-circuit
        if node.CanNotBeNullable then
            false
        elif node.IsAlwaysNullable then
            true
        else
            match node with
            | Epsilon -> true
            | Singleton _ -> false
            | Or(xs, _) ->
                use mutable e = xs.GetEnumerator()
                let mutable found = false

                while not found && e.MoveNext() do
                    found <- _isNullable (&loc, e.Current)

                found
            | And(xs, _) ->
                use mutable e = xs.GetEnumerator()
                let mutable forall = true

                while forall && e.MoveNext() do
                    forall <- _isNullable (&loc, e.Current)

                forall
            | Loop(R, low, _, _) -> low = 0 || (_isNullable (&loc, R))
            | Not(inner, _) -> not (_isNullable (&loc, inner))
            | Concat(head, tail, _) -> _isNullable (&loc, head) && _isNullable (&loc, tail)
            | LookAround(body, _, _, _, _) -> _isNullable (&loc, body)
            | End -> loc.Position = loc.Input.Length
            | Begin -> loc.Position = 0


    let rec _createNonInitialDerivative
        (
            loc_pred: 't,
            node: RegexNode<'t>
        )
        : RegexNode<'t>
        =

        let cachedTransition = Algorithm.RegexNode.getCachedTransition (loc_pred, node)

        let result =
            match cachedTransition with
            | ValueSome inf -> inf
            | _ ->

            match node with
            | Epsilon -> _cache.False
            | Singleton pred ->
                if _cache.Solver.elemOfSet pred loc_pred then
                    _cache.Eps
                else
                    _cache.False
            | Loop(R, low, up, info) ->
                let inline decr x =
                    if x = Int32.MaxValue || x = 0 then x else x - 1

                let case1 = low = 0 || info.IsAlwaysNullable = true || not (_nonInitialIsNullable (R))
                let R_decr = _cache.Builder.mkLoop (R, decr low, decr up)

                match case1 with
                | true ->
                    let R' = _createNonInitialDerivative (loc_pred, R)
                    _cache.Builder.mkConcat2 (R', R_decr)
                | false -> _createNonInitialDerivative (loc_pred, _cache.Builder.mkConcat2 (R, R_decr))
            // Derx (R | S) = Derx (R) | Derx (S)
            | Or(xs, _) ->
                use mutable derivatives = new SharedResizeArrayStruct<RegexNode<'t>>(16)
                for n in xs do
                    let der = _createNonInitialDerivative (loc_pred, n)
                    if not (refEq _cache.False der) then
                        derivatives.Add(der)
                derivatives.AsSpan().Sort(physComparison)
                let mem = derivatives.RentMemory()
                let res = _cache.Builder.mkOr (&mem)
                res

            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, _) ->
                use mutable derivatives = new SharedResizeArrayStruct<RegexNode<'t>>(16)
                let mutable foundFalse = false

                for n in xs do
                    if not foundFalse then
                        let der = _createNonInitialDerivative (loc_pred, n)

                        match der with
                        | _ when refEq _cache.False der -> foundFalse <- true
                        | _ -> derivatives.Add der

                if foundFalse then
                    _cache.False
                else
                    let result = _cache.Builder.mkAnd (derivatives.AllocateArray())
                    result
            // Derx(~R) = ~Derx (R)
            | Not(inner, _) -> _cache.Builder.mkNot (_createNonInitialDerivative (loc_pred, inner))
            | Concat(head, tail, _) when head.IsAlwaysNullable ->
                let R' = _createNonInitialDerivative (loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                let S' = _createNonInitialDerivative (loc_pred, tail)

                if refEq _cache.Builder.uniques._false S' then R'S
                else if refEq R'S _cache.False then S'
                else _cache.Builder.mkOrSeq [| R'S; S' |]
            | Concat(head, tail, _) when head.HasZerowidthHead ->
                let R' = _createNonInitialDerivative (loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                let S' = _createNonInitialDerivative (loc_pred, tail)


                if not (_nonInitialIsNullable (head)) then R'S
                else if refEq R'S _cache.False then S'
                else _cache.Builder.mkOrSeq [| R'S; S' |]
            // Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, _) ->
                let R' = _createNonInitialDerivative (loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                if _nonInitialIsNullable (head) then
                    let S' = _createNonInitialDerivative (loc_pred, tail)
                    if refEq _cache.Builder.uniques._false S' then R'S
                    else if refEq R'S _cache.False then S'
                    else _cache.Builder.mkOrSeq [| R'S; S' |]
                else
                    R'S
            // Lookahead
            | LookAround(
                node = R
                lookBack = false
                relativeTo = rel
                pendingNullables = pendingNulls
                info = _) ->
                let der_R = _createNonInitialDerivative (loc_pred, R)

                match der_R with
                // start a new pending match
                | _ when pendingNulls.IsEmpty ->
                    match _nonInitialIsNullable (der_R) with
                    | true ->
                        _cache.Builder.mkLookaround (der_R, false, rel + 1, RefSet<int>.zeroList)
                    | false ->
                        match der_R with
                        // ⊤*\A special case - always known to be a match
                        | Concat(head = TrueStar _cache.Solver; tail = Begin | End) ->
                            _cache.Builder.mkLookaround (
                                _cache.Eps,
                                false,
                                rel + 1,
                                RefSet<int>.zeroList
                            )
                        | _ ->
                            // if der_R.DependsOnAnchor then failwith "anchor der"
                            // _cache.Builder.mkLookaround(der_R, false, rel, pendingNulls)
                            // this is very expensive but so be it
                            _cache.Builder.mkLookaround (
                                der_R,
                                false,
                                rel + 1,
                                RefSet<int>.zeroList
                            )

                | _ -> _cache.Builder.mkLookaround (der_R, false, rel + 1, pendingNulls)
            // Lookback
            | LookAround(node = R; lookBack = true; relativeTo = _; pendingNullables = _; info = _) ->
                let der_R = _createNonInitialDerivative (loc_pred, R)
                _cache.Builder.mkLookaround (der_R, true, 0, RefSet.empty)
            | Begin
            | End -> _cache.False

#if NO_CACHE_BUILDER
#else
        node.TryGetInfo
        |> ValueOption.iter (fun v -> v.Transitions.TryAdd(loc_pred, result) |> ignore)
#endif

        result

    let rec _createDerivative
        (
            loc: inref<Location<'tchar>>,
            loc_pred: 't,
            node: RegexNode<'t>
        )
        : RegexNode<'t>
        =

        let cachedTransition =
            if loc.Position = loc.Input.Length && node.DependsOnAnchor then
                Algorithm.RegexNode.getEndCachedTransition (loc_pred, node)
            elif loc.Position = 0 && node.DependsOnAnchor then
                Algorithm.RegexNode.getStartCachedTransition (loc_pred, node)
            else
                Algorithm.RegexNode.getCachedTransition (loc_pred, node)


        let result =
            match cachedTransition with
            | ValueSome inf -> inf
            | _ ->

            match node with
            | Epsilon -> _cache.False
            | Singleton pred ->
                if _cache.Solver.elemOfSet pred loc_pred then
                    _cache.Eps
                else
                    _cache.False
            | Loop(R, low, up, info) ->
                let inline decr x =
                    if x = Int32.MaxValue || x = 0 then x else x - 1

                let case1 = low = 0 || info.IsAlwaysNullable = true || not (_isNullable (&loc, R))
                let R_decr = _cache.Builder.mkLoop (R, decr low, decr up)

                match case1 with
                | true ->
                    let R' = _createDerivative (&loc, loc_pred, R)
                    _cache.Builder.mkConcat2 (R', R_decr)
                | false -> _createDerivative (&loc, loc_pred, _cache.Builder.mkConcat2 (R, R_decr))
            // Derx (R | S) = Derx (R) | Derx (S)
            | Or(xs, _) ->
                let pool = ArrayPool<RegexNode<'t>>.Shared
                let rentedArray = pool.Rent(xs.Count)
                use mutable e = xs.GetEnumerator()
                let mutable i = 0
                let mutable count = 0

                while e.MoveNext() do
                    let der = _createDerivative (&loc, loc_pred, e.Current)

                    if not (refEq _cache.False der) then
                        rentedArray[count] <- der
                        count <- count + 1

                    i <- i + 1

                let mem = rentedArray.AsMemory(0, count)
                mem.Span.Sort(physComparison)
                let res = _cache.Builder.mkOr (&mem)
                pool.Return(rentedArray)
                res

            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, _) ->
                let derivatives = ResizeArray()
                let mutable foundFalse = false

                for n in xs do
                    if not foundFalse then
                        let der = _createDerivative (&loc, loc_pred, n)

                        match der with
                        | _ when refEq _cache.False der -> foundFalse <- true
                        | _ -> derivatives.Add der

                if foundFalse then
                    _cache.False
                else
                    let result = _cache.Builder.mkAnd (derivatives)
                    result
            // Derx(~R) = ~Derx (R)
            | Not(inner, _) -> _cache.Builder.mkNot (_createDerivative (&loc, loc_pred, inner))
            | Concat(head, tail, _) when head.IsAlwaysNullable ->
                let R' = _createDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                let S' = _createDerivative (&loc, loc_pred, tail)

                if refEq _cache.Builder.uniques._false S' then R'S
                else if refEq R'S _cache.False then S'
                else _cache.Builder.mkOrSeq [| R'S; S' |]
            | Concat(head, tail, _) when head.HasZerowidthHead ->
                let R' = _createDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)
                let S' = _createDerivative (&loc, loc_pred, tail)

                // let lookaheadEpsilon =
                //     // small semantic detail when lookaround is not in the end
                //     // ex. "1\b-2" \b has to be nullable immediately
                //     match R' with
                //     | LookAround(node=Epsilon;lookBack=false) -> false
                //     | _ -> true

                if not (_isNullable (&loc, head)) then R'S
                else if refEq R'S _cache.False then S'
                else _cache.Builder.mkOrSeq [| R'S; S' |]
            // Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, _) ->
                let R' = _createDerivative (&loc, loc_pred, head)
                let R'S = _cache.Builder.mkConcat2 (R', tail)

                if _isNullable (&loc, head) then
                    let S' = _createDerivative (&loc, loc_pred, tail)

                    if refEq _cache.Builder.uniques._false S' then R'S
                    else if refEq R'S _cache.False then S'
                    else _cache.Builder.mkOrSeq [| R'S; S' |]
                else
                    R'S
            // Lookahead
            | LookAround(
                node = R
                lookBack = false
                relativeTo = rel
                pendingNullables = pendingNulls
                info = _) ->
                let der_R = _createDerivative (&loc, loc_pred, R)

                match der_R with
                // start a new pending match
                | _ when pendingNulls.IsEmpty ->
                    match _isNullable (&loc, der_R) with
                    | true ->
                        _cache.Builder.mkLookaround (der_R, false, rel + 1, RefSet<int>.zeroList)
                    | false ->
                        match der_R with
                        // ⊤*\A special case - always known to be a match
                        | Concat(head = TrueStar _cache.Solver; tail = Begin) ->
                            _cache.Builder.mkLookaround (
                                _cache.Eps,
                                false,
                                rel + 1,
                                RefSet<int>.zeroList
                            )
                        | _ ->
                            // if der_R.DependsOnAnchor then failwith "anchor der"
                            // _cache.Builder.mkLookaround(der_R, false, rel, pendingNulls)
                            // this is very expensive but so be it
                            _cache.Builder.mkLookaround (
                                der_R,
                                false,
                                rel + 1,
                                RefSet<int>.zeroList
                            )

                | _ -> _cache.Builder.mkLookaround (der_R, false, rel + 1, pendingNulls)
            // Lookback
            | LookAround(node = R; lookBack = true; relativeTo = _; pendingNullables = _; info = _) ->
                let der_R = _createDerivative (&loc, loc_pred, R)
                _cache.Builder.mkLookaround (der_R, true, 0, RefSet.empty)
            | Begin
            | End -> _cache.False


#if NO_CACHE_BUILDER
#else
        if loc.Position = loc.Input.Length && node.DependsOnAnchor then
            node.TryGetInfo
            |> ValueOption.iter (fun v -> v.EndTransitions.TryAdd(loc_pred, result) |> ignore)
        elif loc.Position = 0 && node.DependsOnAnchor then
            node.TryGetInfo
            |> ValueOption.iter (fun v -> v.StartTransitions.TryAdd(loc_pred, result) |> ignore)
        else
            node.TryGetInfo
            |> ValueOption.iter (fun v -> v.Transitions.TryAdd(loc_pred, result) |> ignore)
#endif

        result




    and mkLang node =
        _minterms
        |> Array.map (fun mt ->
            _createNonInitialDerivative(mt,node)
        )


    and _canonicalize(node: RegexNode<'t>) =

        match node.TryGetInfo with
        | ValueSome info when info.IsCanonical -> node
        | ValueSome info when info.HasCanonicalForm.IsSome -> info.HasCanonicalForm.Value
        | ValueSome info when not info.PendingNullables.IsEmpty -> node
        | _ ->
        // if node.DependsOnAnchor || node.ContainsLookaround then node else
        if node.DependsOnAnchor then
            node
        else

            match node with
            | Concat(head, tail, _) ->
                let ch = _canonicalize head
                let ct = _canonicalize tail
                let cat = _cache.Builder.mkConcat2 (ch, ct)
                _cache.Builder.GetCanonical(node, mkLang cat, (fun _ -> cat))
            | Or(nodes = nodes; info = _) ->
                let canonNodes = nodes |> Seq.map _canonicalize |> Seq.toArray
                let languages = canonNodes |> Seq.map mkLang
                let mergedLanguage = attemptMergeUnionLang _cache mkLang node languages
                let mknode = (fun _ -> _cache.Builder.mkOrSeq (canonNodes))
                _cache.Builder.GetCanonical(node, mergedLanguage, mknode)
            | Singleton pred ->
                _cache.Builder.GetCanonicalSingleton(pred, (fun _ -> Memory.op_Implicit (mkLang node)))
            | Loop(regexNode, low, up, _) ->
                let inner = _canonicalize regexNode
                let mknode = (fun _ -> _cache.Builder.mkLoop (inner, low, up))
                _cache.Builder.GetCanonical(node, mkLang node, mknode)
            | And(nodes = nodes) -> // node
                let canonNodes = nodes |> Seq.map _canonicalize |> Seq.toArray
                let languages = canonNodes |> Seq.map mkLang
                let mergedLanguage = attemptMergeIntersectLang _cache mkLang node languages
                let mknode = (fun _ -> _cache.Builder.mkAnd (canonNodes))
                _cache.Builder.GetCanonical(node, mergedLanguage, mknode)
            | Not(node = inner) -> // node
                let canon_inner = _canonicalize inner
                let mknode = (fun _ -> _cache.Builder.mkNot (canon_inner))
                _cache.Builder.GetCanonical(node, mkLang node, mknode)
            | LookAround _ -> node
            | Begin
            | End
            | Epsilon -> node


    let _createStartset(state: MatchState<'t>, initial: bool) =
        // expensive for a single match
        if state.Node.ContainsLookaround && not options.FindLookaroundPrefix then
            ()
        else
            let minterms = _cache.Minterms()

            let derivatives =
                minterms
                |> Seq.map (fun minterm ->
                    match RegexNode.getCachedTransition (minterm, state.Node) with
                    | ValueSome v -> v
                    | _ -> _createNonInitialDerivative(minterm,state.Node)
                )

            // let ders = Array.zip minterms derivatives
            // Optimizations.printPrettyDerivs _cache (ders)

            let condition =
                if initial then
                    (fun d -> not (refEq d state.Node || refEq d _cache.False))
                else
                    (fun d -> not (refEq d state.Node))

            let startsetPredicate =
                Seq.zip minterms derivatives
                |> Seq.where (fun (_, d) -> condition d)
                |> Seq.map fst
                |> Solver.mergeSets _cache.Solver

            // let dbg_startset = _cache.PrettyPrintMinterm(unbox startsetPredicate)
            // invert empty startset (nothing to skip to)
            let setChars = _cache.MintermSearchValues(startsetPredicate)
            state.MintermSearchValues <- setChars
            state.Startset <- startsetPredicate

            if
                not (_cache.Solver.IsEmpty(state.Startset) || _cache.Solver.IsFull(state.Startset))
            then
                if _minterms.Length > 40 then
                    ()
                else
                    match setChars.Mode, setChars.SearchValuesSize with
                    | MintermSearchMode.TSet, n ->
                        state.Flags <- state.Flags ||| RegexStateFlags.CanSkipFlag
                    | MintermSearchMode.InvertedSearchValues, n ->
                        if n > 25 then
                            state.Flags <- state.Flags ||| RegexStateFlags.CanSkipFlag
                    | MintermSearchMode.SearchValues, n ->
                        // if _minterms.Length > 60 then () else
                        if n < 64 then
                            state.Flags <- state.Flags ||| RegexStateFlags.CanSkipFlag
                        else
                            ()
                    | _ -> ()
            else
                ()




    let rec _getOrCreateState(revTruestar, origNode, isInitial) =
        let node =
            if options.CanonicalizeStates then
                _canonicalize origNode
            else
                origNode

        match _stateCache.TryGetValue(node) with
        | true, v -> v // a dfa state already exists for this regex
        | _ ->
            let state = MatchState(node)
            _stateCache.Add(node, state)
            state.Id <- _stateCache.Count
            let nodeFlags = node.GetFlags()

            if _stateArray.Length = state.Id then
                if _stateArray.Length > options.MaxDfaCapacity then
                    failwith
                        "Maximum allowed state space reached! increase SbreOptions.MaxDfaSize if this is intended"

                let newsize = _stateArray.Length * 2
                Array.Resize(&_stateArray, newsize)
                Array.Resize(&_flagsArray, newsize)
                Array.Resize(&_dfaDelta, newsize <<< _mintermsLog)

            _stateArray[state.Id] <- state

            if nodeFlags.IsAlwaysNullable then
                state.Flags <- state.Flags ||| RegexStateFlags.AlwaysNullableFlag

            if nodeFlags.CanBeNullable then
                state.Flags <- state.Flags ||| RegexStateFlags.CanBeNullableFlag

            if nodeFlags.ContainsLookaround then
                state.Flags <- state.Flags ||| RegexStateFlags.ContainsLookaroundFlag

            if refEq _cache.False node then
                state.Flags <- state.Flags ||| RegexStateFlags.DeadendFlag

            if isInitial then
                state.Flags <- state.Flags ||| RegexStateFlags.InitialFlag

            // generate startset
            _createStartset (state, isInitial)


            if
                not isInitial
                // && not (state.Flags.HasFlag(RegexStateFlags.CanSkipFlag))
                && not (state.Flags.HasFlag(RegexStateFlags.ContainsLookaroundFlag))
            then
                // see if limited skip possible
                let limitedSkip =
                    Optimizations.tryGetLimitedSkip
                        options
                        _createNonInitialDerivative
                        (fun v -> _getOrCreateState(revTruestar, v, false).Flags)
                        (fun v -> _getOrCreateState(revTruestar, v, false).Id)
                        (fun v -> _getOrCreateState(revTruestar, v, false).Startset)
                        _cache
                        revTruestar
                        state.Node

                match limitedSkip with
                | Some ls ->
                    state.ActiveOptimizationsChar <- ls

                    state.Flags <-
                        state.Flags
                        ||| RegexStateFlags.CanSkipFlag
                        ||| RegexStateFlags.ActiveBranchOptimizations
                | _ -> ()

            // attempt find active optimizations
            // match node with
            // | LookAround(lookBack=false) when not state.Flags.CanSkip ->
            //     let dbg = 1
            //     let activeOpts =
            //         Optimizations.findActiveBranchOptimizations
            //             options
            //             getNonInitialDerivative
            //             (fun v -> _getOrCreateState(revTruestar,v,false).Id )
            //             (fun v -> _getOrCreateState(revTruestar,v,false).Flags )
            //             _cache
            //             node
            //     ()
            // | _ -> ()

            if node.ContainsLookaround && node.CanBeNullable && not isInitial then
                state.PendingNullablePositions <-
                    node.PendingNullables.inner |> Seq.toArray |> Memory

                if state.PendingNullablePositions.Length > 0 then
                    state.Flags <- state.Flags ||| RegexStateFlags.IsPendingNullableFlag

            if node.DependsOnAnchor then
                state.Flags <- state.Flags ||| RegexStateFlags.DependsOnAnchor

            _flagsArray[state.Id] <- state.Flags
            state


    // do _cache.Builder.CanonicalizeCallback <- Some _canonicalize
    do _cache.Builder.InitCanonical(_minterms)
    let R_canonical = _canonicalize uncanonicalizedNode
    let reverseNode = RegexNode.rev _cache.Builder R_canonical
    let reverseTrueStarredNode = _cache.Builder.mkConcat2 (_cache.TrueStar, reverseNode)
    let trueStarredNode = _cache.Builder.mkConcat2 (_cache.TrueStar, R_canonical)
    let _noprefix = mkNodeWithoutLookbackPrefix _cache.Builder R_canonical

    let DFA_TR_rev =
        _getOrCreateState(reverseTrueStarredNode, reverseTrueStarredNode, true).Id // R_rev

    let DFA_R_noPrefix = _getOrCreateState(reverseTrueStarredNode, _noprefix, false).Id


#if OPTIMIZE
    let _utf16InitialOptimizations =
        Optimizations.findInitialOptimizations
            options
            _createNonInitialDerivative
            (fun node -> _getOrCreateState(reverseTrueStarredNode, node, false).Id)
            (fun node -> _getOrCreateState(reverseTrueStarredNode, node, false).Flags)
            _cache
            reverseNode
            reverseTrueStarredNode

    let _byteInitialOptimizations =
        lazy
            Optimizations.convertInitialOptimizations _utf16InitialOptimizations

    let _lengthLookup =
        Optimizations.inferLengthLookup
            (fun node -> _getOrCreateState(reverseTrueStarredNode, node, false).Id)
            _createNonInitialDerivative
            _cache
            _noprefix

    let _regexOverride =
        Optimizations.inferOverrideRegex
            _utf16InitialOptimizations
            _lengthLookup
            _cache
            R_canonical
            reverseNode

    let _byteRegexOverride =
        lazy
            Optimizations.convertOverrideRegex _regexOverride

    let _prefixSets =
        match _utf16InitialOptimizations with
        | InitialOptimizations.SearchValuesPotentialStart(prefix) -> prefix.ToArray() |> Array.rev
        | InitialOptimizations.SearchValuesPrefix(prefix, _) -> prefix.ToArray() |> Array.rev
        | _ -> [||]

    let _commonalityScoreSimple(charSet: char array) =
        charSet
        |> Array.map (fun c ->
            if not (Char.IsAscii(c)) then 12.0
            elif Char.IsWhiteSpace c then 11.0
            elif Char.IsAsciiLetterLower c then 10.0
            elif Char.IsAsciiDigit c then 8.0
            else 9.0
        )
        |> Array.sum

    let _weightedSets =
        if _prefixSets.Length = 0 then
            [||]
        else
            _prefixSets
            // Calculate weights
            |> Array.mapi (fun i set ->
                let mintermSV = set

                match mintermSV.Mode with
                | MintermSearchMode.TSet -> (i, mintermSV, 100000.0)
                | MintermSearchMode.SearchValues ->
                    let weight =
                        _commonalityScoreSimple (
                            mintermSV.CharactersInMinterm.Value.Span.ToArray()
                        )

                    (i, mintermSV, weight)
                | MintermSearchMode.InvertedSearchValues -> (i, mintermSV, 10000.0)
                | _ -> failwith "impossible!"
            )
            |> Array.sortBy (fun (_, _, score) -> score)
            |> Array.map (fun (i, set, _) -> (i, set))
            |> Array.map (fun (i, set) -> struct (i, set))

#else
    let _initialOptimizations = InitialOptimizations<'t>.NoOptimizations
    let _lengthLookup = LengthLookup<'t>.MatchEnd
    let _regexOverride = None
#endif


    override this.IsMatch(input) =
        let mutable acc = new SharedResizeArrayStruct<int>(1)
        let mutable loc = Location.createReversedSpan input
        this.CollectReverseFirstNull(&acc, &loc)


    override this.Match(input) : SingleMatchResult =
        let firstMatch = this.MatchPositions(input).AllocateArray()
        match firstMatch.Length = 0 with
        | true -> SingleMatchResult.Empty
        | false ->
            let result = firstMatch[0]

            {
                Success = true
                Value = input.Slice(result.Index, result.Length).ToString()
                Index = result.Index
                Length = result.Length
            }

    /// replace all occurrences in string
    override this.Replace(input, replacementPattern) =
        let sb = System.Text.StringBuilder()
        let mutable offset = 0
        let replacementCount = replacementPattern.Count("$0")
        let replacementString = replacementPattern.ToString()

        for result in this.MatchPositions(input) do
            let preceding = input.Slice(offset, result.Index - offset)
            let replacement =
                if replacementCount > 0 then
                    // todo: allocation here
                    let textSlice = input.Slice(result.Index, result.Length).ToString()
                    let replacement = replacementString.Replace("$0", textSlice)
                    replacement.AsSpan()
                else
                    replacementPattern

            sb.Append(preceding) |> ignore
            sb.Append(replacement) |> ignore
            let nextStart = offset + preceding.Length + result.Length
            offset <- nextStart

        let remaining = input.Slice(offset)
        sb.Append(remaining) |> ignore
        sb.ToString()

    /// return all matches on input
    override this.Matches(input) =
        let mr = ResizeArray()
        let allResults = this.llmatch_all input

        for result in allResults do
            mr.Add(
                {
                    Value = input.Slice(result.Index, result.Length).ToString()
                    Index = result.Index
                    Length = result.Length
                }
            )

        mr

    /// counts the number of matches
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Count(input: ReadOnlySpan<char>) =
        use mutable results = this.llmatch_all (input)
        results.size

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.Count(input: ReadOnlySpan<byte>) =
        match _byteRegexOverride.Value with
        | Some _override ->
            use mutable results = this.llmatch_all_byte (input)
            results.size
        | _ -> this.llmatch_all_count_only (input)


    member this.llmatch_all_count_only(input: ReadOnlySpan<byte>) : int =
        let mutable loc = Location.createReversedSpan input
        use mutable acc = new SharedResizeArrayStruct<int>(64)
        this.CollectReverseNullablePositionsByte(&acc, &loc)
        loc.Reversed <- false
        let startSpans = acc.AsSpan()
        let mutable count = 0

        for i = startSpans.Length - 1 downto 0 do
            let currStart = startSpans[i]

            if currStart >= loc.Position then
                loc.Position <- currStart
                let matchEnd = this.getMatchEndByte (&loc)
                // let matchEnd = this.DfaEndPositionByte(&loc, DFA_R_noPrefix)
                count <- count + 1
                loc.Position <- matchEnd

        count

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member internal this.CreateStartset(state: MatchState<'t>, initial: bool) =
        _createStartset (state, initial)

    /// initialize regex in DFA
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member internal this.GetOrCreateState(node: RegexNode<'t>) : MatchState<'t> =
        _getOrCreateState (reverseTrueStarredNode, node, false)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member private this.GetDeltaOffset(stateId: int, mintermId: int) =
        (stateId <<< _mintermsLog) ||| mintermId

    // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member private this.TryNextDerivative
        (
            currentState: int,
            mintermId: int,
            loc: inref<Location<'tchar>>
        ) =
        let minterm = _cache.MintermById(mintermId)

        let targetState =
            this.GetOrCreateState(
                match Location.isEdge loc with
                | true -> this.CreateDerivative(&loc, minterm, _stateArray[currentState].Node)
                | _ -> this.CreateNonInitialDerivative(minterm, _stateArray[currentState].Node)
            )
        targetState.Id


#if DEBUG
    member internal this.GetStateAndFlagsById(stateId: int) = _stateArray[stateId]
#endif


    member this.TakeAnchorTransition
        (
            currentState: byref<int>,
            loc: inref<Location<'tchar>>,
            mtId: int
        ) =
        if (loc.Position = loc.Input.Length) then
            let dfaOffset = this.GetDeltaOffset(currentState, mtId)
            let nextStateId = _dfaDelta[dfaOffset]
            let cachedStateId = _revStartStates[nextStateId]
            // existing transition in dfa
            if cachedStateId > 0 then
                currentState <- cachedStateId
            else

            let dfaOffset = this.GetDeltaOffset(currentState, mtId)
            let nextState = this.TryNextDerivative(currentState, mtId, &loc)
            _revStartStates[dfaOffset] <- nextState
            currentState <- nextState
        else
            // dont cache start at all, only nullability changes here
            let nextState = this.TryNextDerivative(currentState, mtId, &loc)
            currentState <- nextState


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TakeTransitionWithAnchors
        (
            flags: RegexStateFlags,
            currentState: byref<int>,
            loc: inref<Location<_>>
        ) =
        let mintermId = _cache.MintermIdChar(loc)
        let dfaOffset = this.GetDeltaOffset(currentState, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        // caching workaround until context implementation
        if
            StateFlags.cannotBeCached flags
            && (loc.Position = loc.Input.Length || loc.Position = 0)
        then
            this.TakeAnchorTransition(&currentState, &loc, mintermId)
        else if
            // existing transition in dfa
            nextStateId > 0
        then
            currentState <- nextStateId
        else if

            // new transition
            obj.ReferenceEquals(null, _stateArray[nextStateId])
        then
            let nextState = this.TryNextDerivative(currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TakeTransitionWithAnchorsByte
        (
            flags: RegexStateFlags,
            currentState: byref<int>,
            loc: inref<Location<byte>>
        ) =
        let mintermId = _cache.MintermIdByte(loc)
        let dfaOffset = this.GetDeltaOffset(currentState, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        // caching workaround until context implementation
        if
            StateFlags.cannotBeCached flags
            && (loc.Position = loc.Input.Length || loc.Position = 0)
        then
            this.TakeAnchorTransition(&currentState, &loc, mintermId)
        else if
            // existing transition in dfa
            nextStateId > 0
        then
            currentState <- nextStateId
        else if

            // new transition
            obj.ReferenceEquals(null, _stateArray[nextStateId])
        then
            let nextState = this.TryNextDerivative(currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TakeTransitionChar(currentState: byref<int>, loc: inref<Location<char>>) =
        let mintermId = _cache.MintermIdChar(loc)
        let dfaOffset = this.GetDeltaOffset(currentState, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        if
            // existing transition in dfa
            nextStateId > 0
        then
            currentState <- nextStateId
        else if

            // new transition
            obj.ReferenceEquals(null, _stateArray[nextStateId])
        then
            let nextState = this.TryNextDerivative(currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TakeTransitionByte(currentState: byref<int>, loc: inref<Location<byte>>) =
        let mintermId = _cache.MintermIdByte(loc)
        let dfaOffset = this.GetDeltaOffset(currentState, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        if
            // existing transition in dfa
            nextStateId > 0
        then
            currentState <- nextStateId
        else if

            // new transition
            obj.ReferenceEquals(null, _stateArray[nextStateId])
        then
            let nextState = this.TryNextDerivative(currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TakeMintermTransition
        (
            currentState: byref<int>,
            mintermId: int,
            loc: inref<Location<'tchar>>
        ) =
        let dfaOffset = this.GetDeltaOffset(currentState, mintermId)
        let nextStateId = _dfaDelta[dfaOffset]

        if
            // existing transition in dfa
            nextStateId > 0
        then
            currentState <- nextStateId
        else if
            // new transition
            obj.ReferenceEquals(null, _stateArray[nextStateId])
        then
            let nextState = this.TryNextDerivative(currentState, mintermId, &loc)
            _dfaDelta[dfaOffset] <- nextState
            currentState <- nextState


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.StateIsNullable
        (
            flags: RegexStateFlags,
            loc: inref<Location<_>>,
            stateId: int
        ) : bool =
        StateFlags.canBeNullable flags
        && (StateFlags.isAlwaysNullable flags
            ||
            // important: prevent unnecessary work if anchor can not be nullable
            (if Location.isEdge loc then
                 this.IsNullable(&loc, _stateArray[stateId].Node)
             else
                 false))

    member this.IsNullable(loc: inref<Location<_>>, node: RegexNode<'t>) : bool =
        _isNullable (&loc, node)

    member this.CreateDerivative<'tchar when 'tchar: struct>
        (
            loc: inref<Location<'tchar>>,
            loc_pred: 't,
            node: RegexNode<'t>
        ) : RegexNode<'t> =
        let canonNode = node
        _createDerivative (&loc, loc_pred, canonNode)

    member this.CreateNonInitialDerivative<'tchar when 'tchar: struct>
        (
            loc_pred: 't,
            node: RegexNode<'t>
        ) : RegexNode<'t> =
        _createNonInitialDerivative (loc_pred, node)

    /// end position with DFA
    member this.DfaEndPositionChar(loc: byref<Location<char>>, startStateId: int) : int32 =
        let mutable looping = true
        let mutable currentStateId = startStateId
        let mutable currentMax = -2

        while looping do
            let flags = _flagsArray[currentStateId]

            if StateFlags.isDeadEnd flags then
                looping <- false
            else if

                StateFlags.canSkipLeftToRight flags
                && this.TrySkipActiveFwd(flags, &loc, &currentStateId)
            then
                ()
            else
                // set max nullability after skipping
                if this.StateIsNullable(flags, &loc, currentStateId) then
                    this.HandleNullableFwd(flags, &currentMax, loc.Position, currentStateId)

                if loc.Position < loc.Input.Length then
                    let mintermId =
                        let i = int loc.Input[loc.Position]

                        match i < 128 with
                        | true -> _ascii[int i]
                        | false -> _nonAscii.Find(i)

                    this.TakeMintermTransition(&currentStateId, mintermId, &loc)
                    loc.Position <- loc.Position + 1
                else
                    looping <- false

        currentMax

    member this.DfaEndPositionByte(loc: byref<Location<byte>>, startStateId: int) : int32 =
        let mutable looping = true
        let mutable currentStateId = startStateId
        let mutable currentMax = -2

        while looping do
            let flags = _flagsArray[currentStateId]

            if StateFlags.isDeadEnd flags then
                looping <- false
            else if

                StateFlags.canSkipLeftToRight flags
                && this.TrySkipActiveFwdByte(flags, &loc, &currentStateId)
            then
                ()
            else

                // set max nullability after skipping
                if this.StateIsNullable(flags, &loc, currentStateId) then
                    this.HandleNullableFwd(flags, &currentMax, loc.Position, currentStateId)

                if loc.Position < loc.Input.Length then
                    let mintermId =
                        let i = loc.Input[loc.Position]

                        match i < 128uy with
                        | true -> _ascii[int i]
                        | false -> 0

                    this.TakeMintermTransition(&currentStateId, mintermId, &loc)
                    loc.Position <- loc.Position + 1
                else
                    looping <- false

        currentMax

    // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TrySkipInitialRevWeightedChar(loc: byref<Location<char>>) : int voption =
        let textSpan = loc.Input
        let currentPosition = loc.Position
        let charSetsCount = _weightedSets.Length
        let wsetspan = _weightedSets.AsSpan()
        let struct (rarestCharSetIndex, rarestCharSet) = wsetspan[0]
        let mutable searching = true
        let mutable prevMatch = currentPosition
        let rarestOffset = _prefixSets.Length - rarestCharSetIndex
        let mutable resultEnd = ValueNone

        while searching do
            if prevMatch < rarestOffset then
                searching <- false
                resultEnd <- ValueNone
            else

            let slice = textSpan.Slice(0, prevMatch - rarestOffset + 1)
            let sharedIndex = _cache.TryNextIndexRightToLeftChar(&slice, rarestCharSet)
            prevMatch <- sharedIndex

            match sharedIndex with
            | n when n - rarestCharSetIndex < 0 ->
                searching <- false
                resultEnd <- ValueNone
            | curMatch ->
                let absMatchStart = curMatch - rarestCharSetIndex
                let mutable fullMatch = true
                let mutable i = 1

                while fullMatch && i < charSetsCount do
                    let struct (weightedSetIndex, weightedSet) = wsetspan[i]

                    if not (weightedSet.Contains(textSpan[absMatchStart + weightedSetIndex])) then
                        fullMatch <- false

                    i <- i + 1

                if fullMatch && i = charSetsCount then
                    searching <- false
                    resultEnd <- ValueSome(absMatchStart + _prefixSets.Length)
                else
                    prevMatch <- curMatch + rarestOffset - 1

        resultEnd

    member this.TrySkipInitialRevWeightedByte(loc: byref<Location<byte>>) : int voption =
        let textSpan = loc.Input
        let currentPosition = loc.Position
        let charSetsCount = _weightedSets.Length
        let wsetspan = _weightedSets.AsSpan()
        let struct (rarestCharSetIndex, rarestCharSet) = wsetspan[0]
        let mutable searching = true
        let mutable prevMatch = currentPosition
        let rarestOffset = _prefixSets.Length - rarestCharSetIndex
        let mutable resultEnd = ValueNone

        while searching do
            if prevMatch < rarestOffset then
                searching <- false
                resultEnd <- ValueNone
            else

            let slice = textSpan.Slice(0, prevMatch - rarestOffset + 1)
            let sharedIndex = rarestCharSet.TryNextIndexRightToLeftByte(&slice)
            prevMatch <- sharedIndex

            match sharedIndex with
            | n when n - rarestCharSetIndex < 0 ->
                searching <- false
                resultEnd <- ValueNone
            | curMatch ->
                let absMatchStart = curMatch - rarestCharSetIndex
                let mutable fullMatch = true
                let mutable i = 1

                while fullMatch && i < charSetsCount do
                    let struct (weightedSetIndex, weightedSet) = wsetspan[i]

                    if not (weightedSet.Contains(textSpan[absMatchStart + weightedSetIndex])) then
                        fullMatch <- false

                    i <- i + 1

                if fullMatch && i = charSetsCount then
                    searching <- false
                    resultEnd <- ValueSome(absMatchStart + _prefixSets.Length)
                else
                    prevMatch <- curMatch + rarestOffset - 1

        resultEnd

    member this.TrySkipInitialRevChar
        (
            loc: byref<Location<char>>,
            currentStateId: byref<int>
        ) : bool =
        match _utf16InitialOptimizations with
        | InitialOptimizations.SearchValuesPrefix(prefix, transitionNodeId) ->
#if SKIP_PREFIX
            let skipResult = this.TrySkipInitialRevWeightedChar &loc
#else
            let skipResult = _cache.TryNextStartsetLocationArrayReversed(&loc, prefix.Span)
#endif
            match skipResult with
            | ValueSome resultEnd ->
                let suffixStart = resultEnd - prefix.Length
                currentStateId <- transitionNodeId
                loc.Position <- suffixStart
                loc.Position <> resultEnd
            | ValueNone -> // no matches remaining
                loc.Position <- 0
                false
        | InitialOptimizations.SearchValuesPotentialStart(prefix) ->
#if SKIP_PREFIX
            let skipResult = this.TrySkipInitialRevWeightedChar &loc
#else
            let skipResult = _cache.TryNextStartsetLocationArrayReversed(&loc, prefix.Span)
#endif
            match skipResult with
            | ValueSome resultEnd ->
                let cnd = loc.Position <> resultEnd
                loc.Position <- resultEnd
                cnd
            | ValueNone -> // no matches remaining
                loc.Position <- 0
                false
        | InitialOptimizations.StringPrefix(prefix, transitionNodeId) ->
            let slice = loc.Input.Slice(0, loc.Position)
            let resultStart = slice.LastIndexOf(prefix.Span)

            if resultStart = -1 then
                loc.Position <- Location.final loc
                false
            else
                currentStateId <- transitionNodeId
                loc.Position <- resultStart
                true
        | InitialOptimizations.StringPrefixCaseIgnore(prefix, isAscii, transitionNodeId) ->
            let mutable resultStart = loc.Position
            let mutable found = false
            let lastChar = prefix.Span[prefix.Length - 1]
            let lowChar = Char.ToLowerInvariant lastChar
            let upChar = Char.ToUpperInvariant lastChar
            let prefixSpan = prefix.Span.Slice(0, prefix.Length - 1)
            let textSpan = loc.Input

            while not found do
                let mutable slice = textSpan.Slice(0, resultStart)
                resultStart <- slice.LastIndexOfAny(lowChar, upChar)

                if resultStart = -1 then
                    found <- true
                else
                    slice <- textSpan.Slice(0, resultStart)

                    match slice.EndsWith(prefixSpan, StringComparison.OrdinalIgnoreCase) with
                    | true ->
                        resultStart <- resultStart - prefix.Span.Length + 1
                        found <- true
                    | _ -> ()

            if resultStart = -1 then
                loc.Position <- Location.final loc
                false
            else
                currentStateId <- transitionNodeId
                loc.Position <- resultStart
                true
        | _ -> false

    member this.TrySkipInitialRevByte
        (
            loc: byref<Location<byte>>,
            currentStateId: byref<int>
        ) : bool =
        match _byteInitialOptimizations.Value with
        | InitialOptimizations.SearchValuesPrefix(prefix, transitionNodeId) ->
#if SKIP_PREFIX
            let skipResult = this.TrySkipInitialRevWeightedByte &loc
#else
            let skipResult = _cache.TryNextStartsetLocationArrayReversed(&loc, prefix.Span)
#endif
            match skipResult with
            | ValueSome resultEnd ->
                let suffixStart = resultEnd - prefix.Length
                currentStateId <- transitionNodeId
                loc.Position <- suffixStart
                loc.Position <> resultEnd
            | ValueNone -> // no matches remaining
                loc.Position <- 0
                false
        | InitialOptimizations.SearchValuesPotentialStart(prefix) ->
#if SKIP_PREFIX
            let skipResult = this.TrySkipInitialRevWeightedByte &loc
#else
            let skipResult = _cache.TryNextStartsetLocationArrayReversed(&loc, prefix.Span)
#endif
            match skipResult with
            | ValueSome resultEnd ->
                let cnd = loc.Position <> resultEnd
                loc.Position <- resultEnd
                cnd
            | ValueNone -> // no matches remaining
                loc.Position <- 0
                false
        | InitialOptimizations.StringPrefix(prefix, transitionNodeId) ->
            let slice = loc.Input.Slice(0, loc.Position)
            let resultStart = slice.LastIndexOf(prefix.Span)

            if resultStart = -1 then
                loc.Position <- Location.final loc
                false
            else
                currentStateId <- transitionNodeId
                loc.Position <- resultStart
                true
        | _ ->
            let dfaState = _stateArray[currentStateId]
            let msv = dfaState.MintermSearchValues
            let slice = loc.Input.Slice(0, loc.Position)
            let resultStart = msv.TryNextIndexRightToLeftByte(&slice)

            if resultStart = -1 then
                loc.Position <- Location.final loc
                false
            else
                loc.Position <- resultStart + 1
                this.TakeTransitionByte(&currentStateId, &loc)
                loc.Position <- resultStart
                true

    // false


    member this.TrySkipActiveFwd
        (
            flags: RegexStateFlags,
            loc: byref<Location<char>>,
            currentStateId: byref<int>
        ) =
        let dfaState = _stateArray[currentStateId]

        if StateFlags.hasActiveBranchOptimizations flags then

            match dfaState.ActiveOptimizationsChar with
            | LimitedSkipOnePath(distance, _, failPred, _, cachedTransitions) ->
                let maxLen = loc.Input.Length

                if distance + loc.Position >= maxLen then // no more matches
                    false
                else
                    let endPos = loc.Position + distance

                    let limitedSlice =
                        if endPos > maxLen then
                            loc.Input.Slice(loc.Position)
                        else
                            loc.Input.Slice(loc.Position, distance)

                    match _cache.TryNextIndexLeftToRight(limitedSlice, failPred) with
                    | -1 -> false
                    | idx ->
                        if idx = 0 then
                            false
                        else
                            let nskipped = idx
                            let mutable tempStateId = currentStateId
                            // lazily cache the skip transitions
                            match cachedTransitions.Span[nskipped] with
                            | -1 ->
                                for i = 0 to nskipped - 1 do
                                    let _ = _flagsArray[tempStateId]
                                    this.TakeTransitionChar(&tempStateId, &loc)

                                cachedTransitions.Span[nskipped] <- tempStateId
                            | v -> tempStateId <- v

                            loc.Position <- loc.Position + nskipped
                            this.TakeTransitionChar(&tempStateId, &loc)
                            currentStateId <- tempStateId
                            loc.Position <- loc.Position + 1
                            true
            | LimitedSkip2Chars(distance, _, failPred, _, cachedTransitions) ->
                let maxLen = loc.Input.Length

                if distance + loc.Position >= maxLen then // no more matches
                    false
                else
                    let endPos = loc.Position + distance

                    let limitedSlice =
                        if endPos > maxLen then
                            loc.Input.Slice(loc.Position)
                        else
                            loc.Input.Slice(loc.Position, distance)

                    let skipResult =
                        match _cache.TryNextIndexLeftToRight(limitedSlice, failPred) with
                        | -1 ->
                            let slice2 = loc.Input.Slice(loc.Position, distance + 2)
                            let pos2 = _cache.TryNextIndexLeftToRight(slice2, failPred)
                            this.TakeTransitionChar(&currentStateId, &loc)
                            loc.Position <- loc.Position + 1
                            this.TakeTransitionChar(&currentStateId, &loc)
                            loc.Position <- loc.Position + 1
                            pos2 - 2
                        | n -> n


                    match skipResult with
                    | -1 -> false
                    | idx ->

                        if idx = 0 then
                            false
                        else
                            let nskipped = idx
                            let mutable tempStateId = currentStateId
                            loc.Position <- loc.Position + 1
                            // lazily cache the skip transitions
                            match cachedTransitions[nskipped] with
                            | -1 ->
                                for i = 0 to nskipped do
                                    let _ = _flagsArray[tempStateId]
                                    this.TakeTransitionChar(&tempStateId, &loc)

                                cachedTransitions[nskipped] <- tempStateId
                            | n -> tempStateId <- n

                            loc.Position <- loc.Position + (nskipped - 2)
                            // let n1 = _stateArray[tempStateId]
                            this.TakeTransitionChar(&tempStateId, &loc)
                            currentStateId <- tempStateId
                            // let n2 = _stateArray[tempStateId]
                            loc.Position <- loc.Position + 1
                            true
            | _ -> false
        else
        // default single char skip
        let sharedIndex =
            _cache.TryNextIndexLeftToRight(
                loc.Input.Slice(loc.Position),
                dfaState.MintermSearchValues
            )

        match sharedIndex with
        | -1 ->
            loc.Position <- Location.final loc
            false
        | _ ->
            loc.Position <- loc.Position + sharedIndex
            sharedIndex <> 0

    member this.TrySkipActiveFwdByte
        (
            flags: RegexStateFlags,
            loc: byref<Location<byte>>,
            currentStateId: byref<int>
        ) =
        let dfaState = _stateArray[currentStateId]

        if StateFlags.hasActiveBranchOptimizations flags then
            match dfaState.ActiveOptimizationsChar with
            | LimitedSkipOnePath(distance, _, failPred, _, cachedTransitions) ->
                let maxLen = loc.Input.Length

                if distance + loc.Position >= maxLen then // no more matches
                    false
                else
                    let endPos = loc.Position + distance

                    let limitedSlice =
                        if endPos > maxLen then
                            loc.Input.Slice(loc.Position)
                        else
                            loc.Input.Slice(loc.Position, distance)

                    match failPred.TryNextIndexLeftToRightByte(limitedSlice) with
                    | -1 -> false
                    | idx ->
                        if idx = 0 then
                            false
                        else
                            let nskipped = idx
                            let mutable tempStateId = currentStateId
                            // lazily cache the skip transitions
                            match cachedTransitions.Span[nskipped] with
                            | -1 ->
                                for i = 0 to nskipped - 1 do
                                    let _ = _flagsArray[tempStateId]
                                    this.TakeTransitionByte(&tempStateId, &loc)

                                cachedTransitions.Span[nskipped] <- tempStateId
                            | v -> tempStateId <- v

                            loc.Position <- loc.Position + nskipped
                            this.TakeTransitionByte(&tempStateId, &loc)
                            currentStateId <- tempStateId
                            loc.Position <- loc.Position + 1
                            true
            | LimitedSkip2Chars(distance, _, failPred, _, cachedTransitions) ->
                let maxLen = loc.Input.Length

                if distance + loc.Position >= maxLen then // no more matches
                    false
                else
                    let endPos = loc.Position + distance

                    let limitedSlice =
                        if endPos > maxLen then
                            loc.Input.Slice(loc.Position)
                        else
                            loc.Input.Slice(loc.Position, distance)

                    let skipResult =

                        match failPred.TryNextIndexLeftToRightByte(limitedSlice) with
                        | -1 ->
                            let slice2 = loc.Input.Slice(loc.Position, distance + 2)
                            let pos2 = failPred.TryNextIndexLeftToRightByte(slice2)
                            this.TakeTransitionByte(&currentStateId, &loc)
                            loc.Position <- loc.Position + 1
                            this.TakeTransitionByte(&currentStateId, &loc)
                            loc.Position <- loc.Position + 1
                            pos2 - 2
                        | n -> n


                    match skipResult with
                    | -1 -> false
                    | idx ->

                        if idx = 0 then
                            false
                        else
                            let nskipped = idx
                            let mutable tempStateId = currentStateId
                            loc.Position <- loc.Position + 1
                            // lazily cache the skip transitions
                            match cachedTransitions[nskipped] with
                            | -1 ->
                                for i = 0 to nskipped do
                                    let _ = _flagsArray[tempStateId]
                                    this.TakeTransitionByte(&tempStateId, &loc)

                                cachedTransitions[nskipped] <- tempStateId
                            | n -> tempStateId <- n

                            loc.Position <- loc.Position + (nskipped - 2)
                            // let n1 = _stateArray[tempStateId]
                            this.TakeTransitionByte(&tempStateId, &loc)
                            currentStateId <- tempStateId
                            // let n2 = _stateArray[tempStateId]
                            loc.Position <- loc.Position + 1
                            true
            | _ -> false
        else
        // default single char skip
        let sharedIndex =
            dfaState.MintermSearchValues.TryNextIndexLeftToRightByte(loc.Input.Slice(loc.Position))

        match sharedIndex with
        | -1 ->
            loc.Position <- Location.final loc
            false
        | _ ->
            loc.Position <- loc.Position + sharedIndex
            sharedIndex <> 0

    member this.TrySkipActiveRevByte
        (
            flags: RegexStateFlags,
            loc: byref<Location<byte>>,
            currentStateId: byref<int>,
            acc: byref<SharedResizeArrayStruct<int>>
        ) : bool =

        if StateFlags.hasActiveBranchOptimizations flags then
            let dfaState = _stateArray[currentStateId]

            match dfaState.ActiveOptimizationsByte with
            | LimitedSkipOnePath(distance, _, failPred, skipToEndTransitionId, cachedTransitions) ->
                if distance > loc.Position then // no more matches
                    false
                else
                    let startPos = loc.Position - distance
                    let length = if startPos < 0 then distance + startPos else distance

                    let limitedSlice = loc.Input.Slice(max 0 startPos, length)

                    match failPred.TryNextIndexRightToLeftByte(&limitedSlice) with
                    | -1 ->
                        loc.Position <- loc.Position - length
                        currentStateId <- skipToEndTransitionId
                        length <> 0
                    | idx ->
                        let nskipped = (length - idx)

                        if nskipped <= 1 then
                            false
                        else
                            let mutable tempStateId = currentStateId

                            // lazily cache the skip transitions
                            match cachedTransitions.Span[nskipped] with
                            | -1 ->
                                for i = 1 to nskipped - 1 do
                                    this.TakeTransitionByte(&tempStateId, &loc)

                                cachedTransitions.Span[nskipped] <- tempStateId
                            | v -> tempStateId <- v

                            loc.Position <- loc.Position - length + idx + 1
                            this.TakeTransitionByte(&tempStateId, &loc)
                            currentStateId <- tempStateId
                            loc.Position <- loc.Position - 1
                            true
            | NoOptimizations -> false
            | SkippableLookahead _ -> false
            | LimitedSkip2Chars _ -> false
        else
            let dfaState = _stateArray[currentStateId]

            if not (dfaState.MintermSearchValues.CanUseAscii()) then
                false
            else
                let tmp_loc = loc.Position
                let slice = loc.Input.Slice(0, loc.Position)

                match dfaState.MintermSearchValues.TryNextIndexRightToLeftByte(&slice) with
                | -1 -> loc.Position <- Location.final loc
                | n -> loc.Position <- n + 1
                // adding all skipped locations
                if tmp_loc > loc.Position && this.StateIsNullable(flags, &loc, currentStateId) then
                    for i = tmp_loc downto loc.Position + 1 do
                        acc.Add(i)

                    true
                else
                    false

    member this.TrySkipActiveRev
        (
            flags: RegexStateFlags,
            loc: byref<Location<_>>,
            currentStateId: byref<int>,
            acc: byref<SharedResizeArrayStruct<int>>
        ) : bool =

        if StateFlags.hasActiveBranchOptimizations flags then

            let dfaState = _stateArray[currentStateId]

            match dfaState.ActiveOptimizationsChar with
            | LimitedSkipOnePath(distance, _, failPred, skipToEndTransitionId, cachedTransitions) ->
                if distance > loc.Position then // no more matches
                    false
                else
                    let startPos = loc.Position - distance
                    let length = if startPos < 0 then distance + startPos else distance

                    let limitedSlice = loc.Input.Slice(max 0 startPos, length)

                    match _cache.TryNextIndexRightToLeftChar(&limitedSlice, failPred) with
                    | -1 ->
                        loc.Position <- loc.Position - length
                        currentStateId <- skipToEndTransitionId
                        length <> 0
                    | idx ->
                        let nskipped = (length - idx)

                        if nskipped <= 1 then
                            false
                        else
                            let mutable tempStateId = currentStateId

                            // lazily cache the skip transitions
                            match cachedTransitions.Span[nskipped] with
                            | -1 ->
                                for i = 1 to nskipped - 1 do
                                    this.TakeTransitionChar(&tempStateId, &loc)

                                cachedTransitions.Span[nskipped] <- tempStateId
                            | v -> tempStateId <- v

                            loc.Position <- loc.Position - length + idx + 1
                            this.TakeTransitionChar(&tempStateId, &loc)
                            currentStateId <- tempStateId
                            loc.Position <- loc.Position - 1
                            true
            | NoOptimizations -> false
            | SkippableLookahead _ -> false
            | LimitedSkip2Chars _ -> false
        else
            let tmp_loc = loc.Position
            let dfaState = _stateArray[currentStateId]
            let slice = loc.Input.Slice(0, loc.Position)

            match _cache.TryNextIndexRightToLeftChar(&slice, dfaState.MintermSearchValues) with
            | -1 -> loc.Position <- Location.final loc
            | n -> loc.Position <- n + 1
            // adding all skipped locations
            if tmp_loc > loc.Position && this.StateIsNullable(flags, &loc, currentStateId) then
                for i = tmp_loc downto loc.Position + 1 do
                    acc.Add(i)

                true
            else
                false

    member this.HandleNullableRev
        (
            flags: RegexStateFlags,
            acc: byref<SharedResizeArrayStruct<int>>,
            currPosition: int,
            currentStateId: int
        ) =
        if StateFlags.isPendingNullable flags then
            let span = _stateArray[currentStateId].PendingNullablePositions.Span

            for i = span.Length - 1 downto 0 do
                acc.Add(span[i] + currPosition)
        else
            acc.Add currPosition

    member this.HandleNullableFwd
        (
            flags: RegexStateFlags,
            currentMax: byref<int>,
            currPosition: int,
            currentStateId
        ) =
        if StateFlags.isPendingNullable flags then
            let pending = _stateArray[currentStateId].PendingNullablePositions.Span

            for p in pending do
                currentMax <- max currentMax (currPosition - p)
        else
            currentMax <- currPosition


    // ensure begin and end anchor works correctly
    member this.TakeStepWithAnchors
        (
            acc: byref<SharedResizeArrayStruct<int>>,
            loc: byref<Location<_>>,
            currentStateId: byref<int>
        ) : unit =
        let flags = _flagsArray[currentStateId]

        if this.StateIsNullable(flags, &loc, currentStateId) then
            this.HandleNullableRev(flags, &acc, loc.Position, currentStateId)

        if loc.Position > 0 then
            this.TakeTransitionWithAnchors(flags, &currentStateId, &loc)
            loc.Position <- loc.Position - 1

    member this.TakeStepWithAnchorsByte
        (
            acc: byref<SharedResizeArrayStruct<int>>,
            loc: byref<Location<byte>>,
            currentStateId: byref<int>
        ) : unit =
        let flags = _flagsArray[currentStateId]

        if this.StateIsNullable(flags, &loc, currentStateId) then
            this.HandleNullableRev(flags, &acc, loc.Position, currentStateId)

        if loc.Position > 0 then
            this.TakeTransitionWithAnchorsByte(flags, &currentStateId, &loc)
            loc.Position <- loc.Position - 1


    member this.CollectReverseNullablePositions
        (
            acc: byref<SharedResizeArrayStruct<int>>,
            loc: byref<Location<char>>
        ) : unit =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)

        let mutable looping = true
        let mutable currentStateId = DFA_TR_rev

        // only consider anchors in the start
        if StateFlags.cannotBeCached _flagsArray[currentStateId] then
            this.TakeStepWithAnchors(&acc, &loc, &currentStateId)

        while looping do
            // let dfaState = _stateArray[currentStateId]
            let flags = _flagsArray[currentStateId]
#if SKIP
            // if false
            if
                (StateFlags.canSkip flags)
                && ((StateFlags.isInitial flags && this.TrySkipInitialRevChar(&loc, &currentStateId))
                    || this.TrySkipActiveRev(flags, &loc, &currentStateId, &acc))
            then
                ()
            else
#endif
            if this.StateIsNullable(flags, &loc, currentStateId) then
                this.HandleNullableRev(flags, &acc, loc.Position, currentStateId)

            if loc.Position > 0 then
                let mintermId =
                    let i = int loc.Input[loc.Position - 1]

                    match i < 128 with
                    | true -> _ascii[int i]
                    | false -> _nonAscii.Find(i)

                this.TakeMintermTransition(&currentStateId, mintermId, &loc)
                loc.Position <- loc.Position - 1
            else
                looping <- false

    member this.CollectReverseNullablePositionsByte
        (
            acc: byref<SharedResizeArrayStruct<int>>,
            loc: byref<Location<byte>>
        ) : unit =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)

        let mutable looping = true
        let mutable currentStateId = DFA_TR_rev

        // only consider anchors in the start
        if StateFlags.cannotBeCached _flagsArray[currentStateId] then
            this.TakeStepWithAnchorsByte(&acc, &loc, &currentStateId)

        while looping do
            let flags = _flagsArray[currentStateId]
#if SKIP
            // if false
            if
                (StateFlags.canSkip flags)
                && ((StateFlags.isInitial flags && this.TrySkipInitialRevByte(&loc, &currentStateId))
                    || this.TrySkipActiveRevByte(flags, &loc, &currentStateId, &acc))
            then
                ()
            else
#endif
            if this.StateIsNullable(flags, &loc, currentStateId) then
                this.HandleNullableRev(flags, &acc, loc.Position, currentStateId)

            if loc.Position > 0 then
                let mintermId =
                    let i = loc.Input[loc.Position - 1]

                    match i < 128uy with
                    | true -> _ascii[int i]
                    | false -> 0

                this.TakeMintermTransition(&currentStateId, mintermId, &loc)
                loc.Position <- loc.Position - 1
            else
                looping <- false

    member this.CollectReverseFirstNull
        (
            acc: byref<SharedResizeArrayStruct<int>>,
            loc: byref<Location<_>>
        ) : bool =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)

        let mutable found = false
        let mutable looping = true
        let mutable currentStateId = DFA_TR_rev

        // only consider anchors in the start
        if StateFlags.cannotBeCached _flagsArray[currentStateId] then
            this.TakeStepWithAnchors(&acc, &loc, &currentStateId)

        while looping do
            let flags = _flagsArray[currentStateId]
            // let _ = _stateArray[currentStateId]
#if SKIP
            if
                (StateFlags.canSkipInitial flags
                 && this.TrySkipInitialRevChar(&loc, &currentStateId))

#if SKIP_ACTIVE
                || (StateFlags.canSkip flags
                    && this.TrySkipActiveRev(flags, &loc, &currentStateId, &acc))
#endif
            then
                ()
            else
#endif
            if this.StateIsNullable(flags, &loc, currentStateId) then
                this.HandleNullableRev(flags, &acc, loc.Position, currentStateId)

            if loc.Position > 0 then
                this.TakeTransitionChar(&currentStateId, &loc)
                loc.Position <- loc.Position - 1
            else
                looping <- false

            if acc.size > 0 then
                looping <- false
                found <- true

        found


    member this.PrintAllDerivatives
        (
            acc: byref<SharedResizeArrayStruct<int>>,
            loc: byref<Location<_>>
        ) : string list =
        assert (loc.Position > -1)
        assert (loc.Reversed = true)
        let mutable looping = true
        let mutable currentStateId = DFA_TR_rev
        let ders = ResizeArray()

        let _flagsSpan = _flagsArray.AsSpan()
        let _dfaDeltaSpan = _dfaDelta.AsSpan()

        while looping do
            let flags = _flagsArray[currentStateId]

            if this.StateIsNullable(flags, &loc, currentStateId) then
                if flags.IsPendingNullable then
                    for pos in _stateArray[currentStateId].PendingNullablePositions.Span do
                        acc.Add pos
                else
                    acc.Add loc.Position

            if loc.Position > 0 then
                this.TakeTransitionChar(&currentStateId, &loc)
                let state = _stateArray[currentStateId]

                let modified =
                    match state.Node with
                    | Or(nodes = nodes) ->
                        nodes
                        |> Seq.where (
                            function
                            | Concat(head = TrueStar _cache.Solver) -> false
                            | _ -> true
                        )
                        |> _cache.Builder.mkOrSeq
                    | _ -> state.Node

                // ders.Add(state.Node.ToString())
                ders.Add(modified.ToString())
                loc.Position <- loc.Position - 1
            else
                looping <- false

        ders |> Seq.toList

    /// tries to match ^PATTERN from the beginning, for use in parser
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.SingleMatchLeftToRight(input: ReadOnlySpan<char>) : int =
        let mutable loc = Location.createSpan input 0
        this.DfaEndPositionChar(&loc, DFA_R_noPrefix)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.getMatchEnd(loc: byref<Location<char>>) : int =
        match _lengthLookup with
        | LengthLookup.FixedLength fl -> loc.Position + fl
        | LengthLookup.FixedLengthPrefixMatchEnd(fl, stateId) ->
            loc.Position <- loc.Position + fl
            this.DfaEndPositionChar(&loc, stateId)
        | _ -> this.DfaEndPositionChar(&loc, DFA_R_noPrefix)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.getMatchEndByte(loc: byref<Location<byte>>) : int =
        match _lengthLookup with
        | LengthLookup.FixedLength fl -> loc.Position + fl
        | LengthLookup.FixedLengthPrefixMatchEnd(fl, stateId) ->
            loc.Position <- loc.Position + fl
            this.DfaEndPositionByte(&loc, stateId)
        | _ -> this.DfaEndPositionByte(&loc, DFA_R_noPrefix)

    member internal this.llmatch_all_override
        (
            acc: byref<SharedResizeArrayStruct<MatchPosition>>,
            loc: byref<Location<_>>,
            overridden: OverrideRegex<char>
        ) =
        let tspan = loc.Input

        match overridden with
        | OverrideRegex.FixedLengthString s ->
            let pspan = s.Span
            let mutable looping = true
            let mutable currPos = 0
            let textLength = s.Length

            while looping do
                // LastIndexOf with ignore case is NOT VECTORIZED
                match tspan.Slice(currPos).IndexOf(pspan, StringComparison.Ordinal) with
                | -1 -> looping <- false
                | n ->
                    let start = currPos + n
                    acc.Add({ MatchPosition.Index = start; Length = textLength })
                    currPos <- start + textLength
        | OverrideRegex.FixedLengthStringCaseIgnore(s, ascii) ->
            let pspan = s.Span
            let mutable looping = true
            let mutable currPos = 0
            let textLength = s.Length
            // ascii in .net is implicitly vectorized - use it if possible
            if ascii then
                while looping do
                    // LastIndexOf with ignore case is NOT VECTORIZED
                    match
                        tspan.Slice(currPos).IndexOf(pspan, StringComparison.OrdinalIgnoreCase)
                    with
                    | -1 -> looping <- false
                    | n ->
                        let start = currPos + n
                        acc.Add({ MatchPosition.Index = start; Length = textLength })
                        currPos <- start + textLength
            // case insensitive unicode is suboptimal for now
            else
                while looping do
                    let slice = tspan.Slice(currPos)
                    let start = slice.IndexOf(pspan.Slice(0, 1), StringComparison.OrdinalIgnoreCase)

                    if start = -1 then
                        looping <- false
                    else
                        match
                            slice.Slice(start).StartsWith(pspan, StringComparison.OrdinalIgnoreCase)
                        with
                        | false -> currPos <- currPos + start + 1
                        | _ ->
                            acc.Add({ MatchPosition.Index = start; Length = textLength })
                            currPos <- currPos + start + textLength


    member internal this.llmatch_all_override_byte
        (
            acc: byref<SharedResizeArrayStruct<MatchPosition>>,
            loc: byref<Location<byte>>,
            overridden: OverrideRegex<byte>
        ) =
        match overridden with
        | OverrideRegex.FixedLengthString s -> Overrides.locateStringsByte &acc loc.Input s.Span
        | OverrideRegex.FixedLengthStringCaseIgnore(s, ascii) ->
            failwith "todo case insensitive byte search"

    member this.llmatch_all_byte
        (input: ReadOnlySpan<byte>)
        : SharedResizeArrayStruct<MatchPosition> =

        let mutable matches = new SharedResizeArrayStruct<MatchPosition>(256)
        let mutable loc = Location.createReversedSpan input

        match _byteRegexOverride.Value with
        | Some regOverride -> this.llmatch_all_override_byte (&matches, &loc, regOverride)
        | _ ->
            let mutable acc = new SharedResizeArrayStruct<int>(512)
            this.CollectReverseNullablePositionsByte(&acc, &loc)
            loc.Reversed <- false
            let mutable nextValidStart = 0
            let startSpans = acc.AsSpan()

            for i = (startSpans.Length - 1) downto 0 do
                let currStart = startSpans[i]

                if currStart >= nextValidStart then
                    loc.Position <- currStart
                    let matchEnd = this.getMatchEndByte (&loc)
                    matches.Add(
                        { MatchPosition.Index = currStart; Length = (matchEnd - currStart) }
                    )

                    nextValidStart <- matchEnd

            acc.Dispose()

        matches

    member this.llmatch_all(input: ReadOnlySpan<char>) : SharedResizeArrayStruct<MatchPosition> =
        let mutable matches = new SharedResizeArrayStruct<MatchPosition>(256)
        let mutable loc = Location.createReversedSpan input

        match _regexOverride with
        | Some regOverride -> this.llmatch_all_override (&matches, &loc, regOverride)
        | _ ->
            let mutable acc = new SharedResizeArrayStruct<int>(512)
            this.CollectReverseNullablePositions(&acc, &loc)
            loc.Reversed <- false
            let mutable nextValidStart = 0
            let startSpans = acc.AsSpan()

            for i = (startSpans.Length - 1) downto 0 do
                let currStart = startSpans[i]

                if currStart >= nextValidStart then
                    loc.Position <- currStart
                    let matchEnd = this.getMatchEnd (&loc)

                    matches.Add(
                        { MatchPosition.Index = currStart; Length = (matchEnd - currStart) }
                    )

                    nextValidStart <- matchEnd

            acc.Dispose()

        matches


    /// return just the positions of matches without allocating the result
    override this.MatchPositions
        (input: ReadOnlySpan<char>)
        : SharedResizeArrayStruct<MatchPosition> =
        (this.llmatch_all input)

    override this.MatchPositions
        (input: ReadOnlySpan<byte>)
        : SharedResizeArrayStruct<MatchPosition> =
        (this.llmatch_all_byte input)

    override this.EnumerateMatches(input) = (this.llmatch_all input).AsSpan()

    // accessors
    member this.TrueStarredPattern = box trueStarredNode :?> RegexNode<uint64>
    member this.ReverseTrueStarredPattern = reverseTrueStarredNode
    member this.RawPattern = R_canonical //:?> RegexNode<uint64>

    member this.PrettyPrintNode(node) =
        let bddNode =
            Minterms.transformBack
                _cache.Builder
                _cache.BddBuilder
                _cache.Solver
                _cache.CharsetSolver
                node

        bddNode.ToString()

    /// print full node with expanded sets
    member this.PrettyPrintNodeLong(node) =
        let bddNode =
            Minterms.transformBack
                _cache.Builder
                _cache.BddBuilder
                _cache.Solver
                _cache.CharsetSolver
                node

        bddNode.ToStringLong()

    member this.GetBddNode(node) =
        let bddNode =
            Minterms.transformBack
                _cache.Builder
                _cache.BddBuilder
                _cache.Solver
                _cache.CharsetSolver
                node

        bddNode

    member this.RawPatternWithoutLookback = _stateArray[DFA_R_noPrefix].Node
    member this.ReversePattern = reverseNode
    member this.Cache = _cache
    member internal this.InternalOptimizations = _utf16InitialOptimizations
    member this.AttemptCanonicalize n = _canonicalize n

module internal Helpers =
    let rec createMatcher
        (
            bddBuilder: RegexBuilder<BDD>,
            bddMinterms: BDD array,
            charsetSolver,
            converter,
            symbolicBddnode,
            symbolicBuilder,
            options
        )
        : GenericRegexMatcher
        =
        match bddMinterms.Length with
        | n when n <= 64 ->
            let solver = UInt64Solver(bddMinterms, charsetSolver)
#if DEBUG
            Common.debuggerSolver <- Some solver
#endif
            let uintbuilder = RegexBuilder(converter, solver, charsetSolver, options)

            let rawNode =
                (Minterms.transform bddBuilder uintbuilder charsetSolver solver) symbolicBddnode



            let cache =
                Sbre.Cache.RegexCache<uint64>(
                    solver,
                    charsetSolver,
                    bddMinterms,
                    _rawPattern = rawNode,
                    _builder = uintbuilder,
                    _bddbuilder = bddBuilder
                )

            let m = RegexMatcher(rawNode, cache, options)

            if not (refEq m.RawPattern rawNode) then
                let backToBdd =
                    Minterms.transformBack uintbuilder bddBuilder solver charsetSolver m.RawPattern

                let recomputedMinterms = backToBdd |> Minterms.compute symbolicBuilder
                // TODO: analyze this
                // if recomputedMinterms.Length <> bddMinterms.Length then
                //     let oldprettymts = bddMinterms |> Array.map charsetSolver.PrettyPrint
                //     let prettymts = recomputedMinterms |> Array.map charsetSolver.PrettyPrint
                //     failwith $"reduced minterms from {bddMinterms.Length} to {recomputedMinterms.Length}, {rawNode} ==> {m.RawPattern}\nold:%A{oldprettymts}\nremaining: %A{prettymts}"
                createMatcher (
                    bddBuilder,
                    recomputedMinterms,
                    charsetSolver,
                    converter,
                    backToBdd,
                    symbolicBuilder,
                    options
                )
            else

            m

        | n ->
            // ideally subsume the minterms to 64 or below
            let solver = BitVectorSolver(bddMinterms, charsetSolver)
            let tsetbuilder = RegexBuilder(converter, solver, charsetSolver, options)

            let rawNode =
                (Minterms.transform bddBuilder tsetbuilder charsetSolver solver) symbolicBddnode

            let cache =
                Sbre.Cache.RegexCache<BitVector>(
                    solver,
                    charsetSolver,
                    bddMinterms,
                    _rawPattern = rawNode,
                    _builder = tsetbuilder,
                    _bddbuilder = bddBuilder
                )

            let m = RegexMatcher(rawNode, cache, options)

            // let backToBdd =
            //     Minterms.transformBack tsetbuilder bddBuilder solver charsetSolver m.RawPatternObj
            // let newMinterms = backToBdd |> Minterms.compute symbolicBuilder

            if not (refEq (m.RawPattern) (rawNode)) then
                let backToBdd =
                    Minterms.transformBack
                        tsetbuilder
                        bddBuilder
                        solver
                        charsetSolver
                        m.RawPattern

                let recomputedMinterms = backToBdd |> Minterms.compute symbolicBuilder
                // TODO: analyze this
                // if recomputedMinterms.Length <> bddMinterms.Length then
                //     let oldprettymts = bddMinterms |> Array.map charsetSolver.PrettyPrint
                //     let prettymts = recomputedMinterms |> Array.map charsetSolver.PrettyPrint
                //     failwith $"reduced minterms from {bddMinterms.Length} to {recomputedMinterms.Length}, {rawNode} ==> {m.RawPattern}\nold:%A{oldprettymts}\nremaining: %A{prettymts}"
                createMatcher (
                    bddBuilder,
                    recomputedMinterms,
                    charsetSolver,
                    converter,
                    backToBdd,
                    symbolicBuilder,
                    options
                )
            else

            m

[<Sealed>]
type Regex
    (pattern: string, [<Optional; DefaultParameterValue(null: SbreOptions)>] options: SbreOptions) =
    inherit GenericRegexMatcher()
    let options = ifNull (fun _ -> SbreOptions()) options
    let pattern =
        Sbre.Parser.processString
            (pattern.Replace("⊤", @"[\s\S]"))

    // experimental parser!
    let regexTree =
        ExtendedRegexParser.Parse(
            pattern,
            RegexOptions.ExplicitCapture
            ||| RegexOptions.NonBacktracking
            ||| RegexOptions.Multiline
            ||| RegexOptions.CultureInvariant,
            CultureInfo.InvariantCulture
        )
    let charsetSolver = CharSetSolver()
    let runtimeBddBuilder = SymbolicRegexBuilder<BDD>(charsetSolver, charsetSolver)
    let converter = RegexNodeConverter(runtimeBddBuilder, null)
    let regexBuilder = RegexBuilder(converter, charsetSolver, charsetSolver, options)

    let symbolicBddnode: RegexNode<BDD> =
        RegexNodeConverter.convertToSymbolicRegexNode (charsetSolver, regexBuilder, regexTree.Root)

    let minterms = symbolicBddnode |> Minterms.compute runtimeBddBuilder

    let matcher =
        Helpers.createMatcher (
            regexBuilder,
            minterms,
            charsetSolver,
            converter,
            symbolicBddnode,
            runtimeBddBuilder,
            options
        )

    /// utf-16 match with unicode support
    override this.Count(input: ReadOnlySpan<char>) = matcher.Count(input)
    /// ascii-only match over bytes
    override this.Count(input: ReadOnlySpan<byte>) = matcher.Count(input)

    override this.IsMatch(input) = matcher.IsMatch(input)

    /// utf-16 match with unicode support
    override this.MatchPositions(input: ReadOnlySpan<char>) = matcher.MatchPositions(input)

    /// ascii-only match over bytes
    override this.MatchPositions
        (input: ReadOnlySpan<byte>)
        : SharedResizeArrayStruct<MatchPosition> =
        matcher.MatchPositions(input)

    override this.EnumerateMatches(input) = matcher.EnumerateMatches(input)
    override this.Matches(input) = matcher.Matches(input)
    override this.Replace(input, replacement) = matcher.Replace(input, replacement)
    override this.Match(input) = matcher.Match(input)
    /// internal regex matcher for debugging
    member this.Matcher: GenericRegexMatcher = matcher
    member this.Options: SbreOptions = options
    /// internal regex matcher for debugging
    member this.TSetMatcher = matcher :?> RegexMatcher<uint64>
