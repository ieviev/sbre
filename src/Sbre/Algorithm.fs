module rec Sbre.Algorithm

open System
open System.Buffers
open System.Collections.Immutable
open System.Runtime.InteropServices
open Sbre.Info
open Sbre.Optimizations
open Sbre.Pat
open Sbre.Types

module RegexNode =

    let rec rev (builder: RegexBuilder<_>) (node: RegexNode<_>) =
        match node with
        // ψr = ψ
        | Singleton _ -> node
        // // (R|S)r = Rr|Sr
        | Or(xs, info) ->
            let xs' = xs |> map (rev builder)
            builder.mkOr (Seq.toArray xs')
        // R{m, n, b}r = Rr{m, n, b}
        | Loop(xs, low, up, info) ->
            let xs' = (rev builder) xs
            builder.mkLoop (xs', low, up)
        // (R & S)r = Rr & S r
        | And(xs, info) ->
            let xs' = xs |> map (rev builder)
            builder.mkAnd (Seq.toArray xs')
        // (~R)r = ~(Rr)
        | Not(xs, info) ->
            let xs' = (rev builder) xs
            builder.mkNot xs'
        // (?=R)r = (?<=Rr)
        | LookAround(node = node'; lookBack = false; negate = false) ->
            LookAround((rev builder) node', lookBack = true, negate = false)
        // (?<=R)r = (?=Rr)
        | LookAround(node = node'; lookBack = true; negate = false) ->
            LookAround((rev builder) node', lookBack = false, negate = false)
        // (?!R)r = (?<!Rr)
        | LookAround(node = node'; lookBack = false; negate = true) ->
            LookAround((rev builder) node', lookBack = true, negate = true)
        // (?<!R)r = (?!Rr)
        | LookAround(node = node'; lookBack = true; negate = true) ->
            LookAround((rev builder) node', lookBack = false, negate = true)
        | Concat(head, tail, info) ->
            let rec revConcatNode acc curr =
                match curr with
                | Concat(head, (Concat _ as tail), tinfo) ->
                    revConcatNode (rev builder head :: acc) tail
                | Concat(head, tail, tinfo) -> rev builder tail :: rev builder head :: acc
                | single -> rev builder single :: acc

            let reversedList = revConcatNode [] node
            builder.mkConcat reversedList
        | Epsilon -> Epsilon


    let rec isNullable(cache: RegexCache<_>, loc: inref<Location>, node: RegexNode<_>) : bool =

        // short-circuit
        if canNotBeNullable node then
            false
        elif isAlwaysNullable node then
            true
        else

        match node with
        // Nullx () = true
        | Epsilon -> true
        // Nullx (ψ) = false
        | Singleton _ -> false
        // Nullx (R) or Nullx (S)
        | Or(xs, info) ->
            use mutable e = xs.GetEnumerator()
            let mutable found = false
            while not found && e.MoveNext() do
                found <- isNullable (cache, &loc, e.Current)
            found
        // Nullx (R) and Nullx (S)
        | And(xs, info) ->
            use mutable e = xs.GetEnumerator()
            let mutable forall = true
            while forall && e.MoveNext() do
                forall <- isNullable (cache, &loc, e.Current)
            forall
        // Nullx (R{m, n}) = m = 0 or Nullx (R)
        | Loop(R, low, _, info) -> low = 0 || (isNullable (cache, &loc, R))
        // not(Nullx (R))
        | Not(node, info) -> not (isNullable (cache, &loc, node))
        // Nullx (R) and Nullx (S)
        | Concat(head, tail, _) -> isNullable (cache, &loc, head) && isNullable (cache, &loc, tail)
        | LookAround(body, lookBack, negate) ->
            let mutable _tlo = cache.False
            match lookBack, negate with
            // Nullx ((?=R)) = IsMatch(x, R)
            | false, false ->
                let mutable loc2 = Location.clone &loc
                match matchEnd cache &loc2 body &_tlo with
                | ValueNone -> false
                | ValueSome _ -> true

            // Nullx ((?!R)) = not IsMatch(x, R)
            | false, true ->
                let mutable loc2 = Location.clone &loc
                match matchEnd cache &loc2 body &_tlo with
                | ValueNone -> true
                | ValueSome _ -> false

            | true, false -> // Nullx ((?<=R)) = IsMatch(xr, Rr)
                let mutable revnode = (RegexNode.rev cache.Builder body)
                let mutable revloc = Location.createSpanRev loc.Input loc.Position loc.Reversed
                match matchEnd cache &revloc revnode &_tlo with
                | ValueNone -> false
                | ValueSome _ -> true

            | true, true -> // Nullx ((?<!R)) = not IsMatch(x r, Rr)
                let R_rev = RegexNode.rev cache.Builder body
                let mutable revloc = Location.createSpanRev loc.Input loc.Position loc.Reversed
                match matchEnd cache &revloc R_rev &_tlo with
                | ValueNone -> true
                | ValueSome _ -> false




    let inline getTransitionInfo(pred: ^t, node: RegexNode< ^t >) =
        let mutable result = ValueNone

        match node with
        | Or(info = info)
        | Loop(info = info)
        | And(info = info)
        | Not(info = info)
        | Concat(info = info) ->
            let mutable e = CollectionsMarshal.AsSpan(info.Transitions)
            //.GetEnumerator()
            // use mutable e = info.Transitions.GetEnumerator()
            let mutable looping = true
            let mutable i = 0


            // while looping && e.MoveNext() do
            while looping && i < e.Length do
                let curr = e[i]

                if Solver.elemOfSet pred curr.Set then
                    looping <- false
                    result <- ValueSome(curr.Node)

                i <- i + 1

            result
        | _ -> result

    let inline getCachedTransition(pred: ^t, info: RegexNodeInfo< ^t > voption) =
        let mutable result = ValueNone
        match info with
        | ValueSome info ->
            let mutable e = CollectionsMarshal.AsSpan(info.Transitions)
            let mutable looping = true
            let mutable i = 0

            while looping && i < e.Length do
                let curr = e[i]

                if Solver.elemOfSet pred curr.Set then
                    looping <- false
                    result <- ValueSome(curr.Node)

                i <- i + 1
        | _ -> ()
        result

    let deriveActiveBranch(
        toplevelOr:byref<RegexNode<_>>,
        locationPredicate,
        loc:byref<Location>,
        cache:RegexCache<TSet>,
        updated:RegexNode<_>
        ) : bool =
        // let updated =
        //     match getTransitionInfo2 (locationPredicate, toplevelOrInfo) with
        //     | ValueSome v -> v
        //     | _ -> createDerivative (cache, &loc, locationPredicate, toplevelOr)
        // if refEq toplevelOr updated then
        //     match toplevelOrInfo with
        //     | ValueSome info ->
        //         info.CanBeNullable
        //         && (info.IsAlwaysNullable || isNullable (cache, &loc, toplevelOr))
        //     | _ -> isNullable (cache, &loc, toplevelOr)
        // else

        let mutable isFinalNullable = false
        if refEq cache.False updated then
            isFinalNullable <-
                match toplevelOr.TryGetInfo with
                | ValueSome info ->
                    info.CanBeNullable
                    && (info.IsAlwaysNullable || isNullable (cache, &loc, toplevelOr))
                | _ -> isNullable (cache, &loc, toplevelOr)
        toplevelOr <- updated
        isFinalNullable

    let deriveInitialBranch(
        _currentActiveBranch:byref<RegexNode<_>>,
        _initialNode:RegexNode<_>,
        _initialInfo:RegexNodeInfo<_> voption,
        locationPredicate,
        loc:byref<Location>,
        cache:RegexCache<TSet>,
        foundmatch:byref<bool>
        ) =
        let deriv =
            match getCachedTransition (locationPredicate, _initialInfo) with
            | ValueSome v -> v
            | _ ->
                createDerivative (
                    cache,
                    &loc,
                    locationPredicate,
                    _initialNode
                )

        if refEq deriv cache.False then
            if isAlwaysNullable _initialNode then
                foundmatch <- true
        else
            if refEq _currentActiveBranch cache.False then
                _currentActiveBranch <- deriv

            let isSubsumed =
                refEq _currentActiveBranch deriv ||
                isAlwaysNullable (_currentActiveBranch) ||
                match cache.Builder.SubsumptionCache.TryGetValue(struct (_currentActiveBranch, deriv)) with
                | true, subsumed -> subsumed
                | _ -> cache.Builder.trySubsumeTopLevelOr (_currentActiveBranch, deriv)

            if not isSubsumed then
                _currentActiveBranch <- cache.Builder.mkOr [| deriv; _currentActiveBranch |]




    let matchEnd
        (cache: RegexCache<TSet>)
        (loc: byref<Location>)
        (initialNode: RegexNode<TSet>)
        (toplevelOr: byref<RegexNode<TSet>>)
        : int voption
        =
        let mutable currentMax = ValueNone
        let mutable foundmatch = false

        // initial node
        let _initialWithoutDotstar =
            if cache.IsImplicitDotStarred initialNode then
                cache.InitialPatternWithoutDotstar
            else
                toplevelOr <- initialNode
                initialNode

        let _startsetPredicate = cache.GetInitialStartsetPredicate
        let _builder = cache.Builder
        let _initialInfo = _initialWithoutDotstar.TryGetInfo

        while not foundmatch do
            if Location.isFinal loc then
                foundmatch <- true
            else
                let mt_id = cache.MintermId(loc)
                let locationPredicate = cache.MintermById(mt_id)
                // current active branches
                if not (refEq toplevelOr cache.False) then
                    let updated =
                        match getCachedTransition (locationPredicate, toplevelOr.TryGetInfo) with
                        | ValueSome v -> v
                        | _ -> createDerivative (cache, &loc, locationPredicate, toplevelOr)

                    let isFinalNullable =
                        deriveActiveBranch(
                            &toplevelOr,
                            locationPredicate,&loc,cache,updated)
                    if isFinalNullable then
                        currentMax <- ValueSome loc.Position
                        foundmatch <- true

                // create implicit dotstar derivative only if startset matches
                if
                    Solver.elemOfSet _startsetPredicate locationPredicate
                    && cache.IsImplicitDotStarred initialNode
                    && currentMax.IsNone
                then
                    deriveInitialBranch(
                            &toplevelOr,
                            _initialWithoutDotstar,
                            _initialInfo,
                            locationPredicate,&loc,cache, &foundmatch)

                if not foundmatch then
                    loc.Position <- Location.nextPosition loc
                    // check nullability
                    if
                        canBeNullableV (toplevelOr.TryGetInfo,toplevelOr)
                    then
                        if isAlwaysNullable toplevelOr || isNullable (cache, &loc, toplevelOr) then
                            currentMax <- (ValueSome loc.Position)

                    // won't try to jump further if final
                    if
                        Location.isFinal loc
                        || (not (cache.IsImplicitDotStarred initialNode) && refEq toplevelOr cache.False)
                    then
                        foundmatch <- true
                    else if
                        // check if some input can be skipped
                        Solver.notElemOfSet _startsetPredicate (cache.MintermForLocation(loc))
                    then
                        // jump from initial state
                        if refEq toplevelOr cache.False then
                            cache.TryNextStartsetLocationArray(&loc,cache.GetInitialStartsetPrefix().Span)

                        // jump mid-regex
                        match toplevelOr.TryGetInfo with
                        | ValueSome i ->
                            if i.Flags.HasFlag(Flag.CanSkipFlag) then
                                loc.Position <- tryJumpToStartset cache &loc toplevelOr
                        | ValueNone -> ()



        if foundmatch then
            match currentMax with
            | ValueSome(n) when n <> loc.Position ->
                if isNullable (cache, &loc, toplevelOr) then
                    currentMax <- (ValueSome loc.Position)
            | _ ->
                if _initialWithoutDotstar.IsAlwaysNullable then
                    currentMax <- (ValueSome loc.Position)

        currentMax

let rec createDerivative
    (
        c: RegexCache<TSet>,
        loc: inref<Location>,
        loc_pred: TSet,
        node: RegexNode<TSet>
    )
    : RegexNode<TSet>
    =
    // let Der newNode = createDerivative (c, &loc, loc_pred, newNode) //
    match RegexNode.getTransitionInfo (loc_pred, node) with
    | ValueSome n -> n
    | _ ->
        let result =
            match node with
            // Derx (R) = ⊥ if R ∈ ANC or R = ()
            | LookAround _
            | Epsilon -> c.False
            // Der s⟨i⟩ (ψ) = if si ∈ [[ψ]] then () else ⊥
            | Singleton pred ->
                // if c.Solver.isElemOfSet(pred,loc_pred) then Epsilon else c.False
                if Solver.elemOfSet pred loc_pred then Epsilon else c.False

            // Derx (R{m, n}) =
            // if m=0 or Null ∀(R)=true or Nullx (R)=false
            // then Derx (R)·R{m −1, n −1}
            // else Derx (R·R{m −1, n −1})
            | Loop(R, low, up, info) ->
                let inline decr x =
                    if x = Int32.MaxValue || x = 0 then x else x - 1

                let case1 =
                    low = 0
                    || info.IsAlwaysNullable = true
                    || not (RegexNode.isNullable (c, &loc, R))

                match case1 with
                | true ->
                    // Derx (R)·R{m −1, n −1, l}
                    let derR = createDerivative (c, &loc, loc_pred, R)
                    c.Builder.mkConcat2 (derR, c.Builder.mkLoop (R, decr low, decr up))

                | false ->
                    // Derx (R·R{m −1, n −1, l})
                    createDerivative (
                        c,
                        &loc,
                        loc_pred,
                        c.Builder.mkConcat2 (R, c.Builder.mkLoop (R, decr low, decr up))
                    )



            // Derx (R | S) = Derx (R) | Derx (S)
            | Or(xs, info) ->
                // let derR x = createDerivative (c, loc, loc_pred, x)
                // xs |> Seq.map derR
                let derivatives = ResizeArray()
                for n in xs do
                    derivatives.Add (createDerivative (c, &loc, loc_pred, n))
                derivatives |> c.Builder.mkOr

            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, info) as head ->
                // let derR x = createDerivative (c, loc, loc_pred, x)
                // xs |> Seq.map derR |> c.Builder.mkAnd
                let derivatives = ResizeArray()
                for n in xs do
                    derivatives.Add (createDerivative (c, &loc, loc_pred, n))
                c.Builder.mkAnd(derivatives)

            // Derx(~R) = ~Derx (R)
            | Not(inner, info) ->
                let derR = createDerivative (c, &loc, loc_pred, inner)
                c.Builder.mkNot (derR)
            // Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, info) ->
                let R' = createDerivative (c, &loc, loc_pred, head)
                let R'S = c.Builder.mkConcat2 (R', tail)

                if RegexNode.isNullable (c, &loc, head) then
                    let S' = createDerivative (c, &loc, loc_pred, tail)

                    if refEq c.Builder.uniques._false S' then
                        R'S
                    else
                        let newConcat =
                            c.Builder.mkOr (
                                seq {
                                    R'S
                                    S'
                                }
                            )

                        newConcat
                else
                    R'S

        if not (containsLookaround node) then
            c.Builder.AddTransitionInfo(loc_pred, node, result)

        result
