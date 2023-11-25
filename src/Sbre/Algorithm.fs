module rec Sbre.Algorithm

open System
open System.Collections.Generic
open Sbre.Info
open Sbre.Optimizations
open Sbre.Pat
open Sbre.Types

module RegexNode =

    let rec rev (builder: RegexBuilder<uint64>) (node: RegexNode<_>) =
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
                | Concat(head, tail, tinfo) ->
                    rev builder tail :: rev builder head :: acc
                | single -> rev builder single :: acc

            let reversedList = revConcatNode [] node
            builder.mkConcat reversedList
        | Epsilon -> Epsilon


    let rec isNullable(cache: RegexCache<uint64>, loc: inref<Location>, node: RegexNode<uint64>) : bool =
        let inline Null loc node = isNullable (cache, &loc, node)

        let recursiveIsMatch (loc:Location) body =
            let mutable loc1 = loc
            match matchEnd (cache, &loc1, body) with
            | ValueNone -> false
            | ValueSome _ -> true

        // short-circuit
        if canNotBeNullable node then false
        elif isAlwaysNullable node then true
        else

        match node with
        // Nullx () = true
        | Epsilon -> true
        // Nullx (ψ) = false
        | Singleton _ -> false
        // Nullx (R) or Nullx (S)
        | Or(xs, info) -> (exists (Null loc) xs)
        // Nullx (R) and Nullx (S)
        | And(xs, info) -> forall (Null loc) xs
        // Nullx (R{m, n}) = m = 0 or Nullx (R)
        | Loop(R, low, _, info) -> low = 0 || (isNullable (cache, &loc, R))
        // not(Nullx (R))
        | Not(node, info) -> not (isNullable (cache, &loc, node))
        // Nullx (R) and Nullx (S)
        | Concat(head, tail, _) -> isNullable (cache, &loc, head) && isNullable (cache, &loc, tail)
        | LookAround(body, lookBack, negate) ->
            match lookBack, negate with
            | false, false -> recursiveIsMatch loc body // Nullx ((?=R)) = IsMatch(x, R)
            // Nullx ((?!R)) = not IsMatch(x, R)
            | false, true ->
                not (recursiveIsMatch loc body)

            | true, false -> // Nullx ((?<=R)) = IsMatch(xr, Rr)
                let revnode = (RegexNode.rev cache.Builder body)
                let revloc = (Location.rev loc)
                let ism = recursiveIsMatch revloc revnode
                ism
            | true, true -> // Nullx ((?<!R)) = not IsMatch(x r, Rr)
                let loc_rev = Location.rev loc
                let R_rev = RegexNode.rev cache.Builder body
                not (recursiveIsMatch loc_rev R_rev)


    let matchEnd
        (
            cache: RegexCache<uint64>,
            loc: byref<Location>,
            initialNode: RegexNode<uint64>
        )
        : int voption
        =

        let mutable currentMax = ValueNone
        let mutable foundmatch = false

        // initial node
        let mutable _toplevelOr = cache.False
        let _initialWithoutDotstar =
            if cache.IsImplicitDotStarred initialNode then
                cache.InitialPatternWithoutDotstar
            else
                _toplevelOr <- initialNode
                initialNode
        let _startsetPredicate = cache.GetInitialStartsetPredicate()
        let _builder = cache.Builder


        while not foundmatch do
            if Location.isFinal loc then
                foundmatch <- true
            else
                let locationPredicate = cache.MintermForLocation(loc)

                // current active branches
                if not (refEq _toplevelOr cache.False) then
                    let updated =
                        match _builder.GetTransitionInfo(locationPredicate, _toplevelOr) with
                        | ValueSome v -> v
                        | _ ->
                            createDerivative(cache,loc,locationPredicate,_toplevelOr)
                    if refEq cache.False updated then
                        if isNullable(cache,&loc, _toplevelOr) then
                            currentMax <- ValueSome loc.Position
                        if currentMax.IsSome then
                            foundmatch <- true
                    _toplevelOr <- updated

                // create implicit dotstar derivative only if startset matches
                if
                    Solver.elemOfSet _startsetPredicate locationPredicate
                    && cache.IsImplicitDotStarred initialNode
                    && currentMax.IsNone
                then
                    let deriv =
                        match _builder.GetTransitionInfo(locationPredicate, _initialWithoutDotstar) with
                        | ValueSome v -> v
                        | _ ->
                            createDerivative (cache, loc, locationPredicate, _initialWithoutDotstar)

                    match deriv with
                    | _ when refEq deriv cache.False ->
                        if _initialWithoutDotstar.IsAlwaysNullable then
                            foundmatch <- true
                    | _ ->
                        if refEq _toplevelOr cache.False then
                            _toplevelOr <- deriv
                        else
                            if refEq _toplevelOr deriv || _toplevelOr.IsAlwaysNullable then () else
                            match cache.Builder.AndSubsumptionCache.TryGetValue(struct(_toplevelOr,deriv)) with
                            | true, true -> ()
                            | true, false ->
#if OPTIMIZE
                                failwith $"unoptimized:\n{_toplevelOr.ToStringHelper()}\n{deriv}"
#endif
                                // failwith $"unoptimized:\n{_toplevelOr.ToStringHelper()}\n{deriv}"
                                _toplevelOr <- _builder.mkOr (seq {yield deriv; yield _toplevelOr})
                            | _ ->

                            if cache.Builder.trySubsumeTopLevelOr(_toplevelOr,deriv) then () else
#if OPTIMIZE
                            failwith $"unoptimized:\n{_toplevelOr.ToStringHelper()}\n{deriv}"
#endif
                            // failwith $"unoptimized:\n{_toplevelOr.ToStringHelper()}\n{deriv}"
                            _toplevelOr <- _builder.mkOr (seq {yield deriv; yield _toplevelOr})


                if not foundmatch then
                    loc.Position <- Location.nextPosition loc
                    // check nullability
                    if _toplevelOr.CanBeNullable && (_toplevelOr.IsAlwaysNullable || isNullable(cache,&loc,_toplevelOr)) then
                        currentMax <- (ValueSome loc.Position)

                    // won't try to jump further if final
                    if Location.isFinal loc || (not (cache.IsImplicitDotStarred initialNode) && refEq _toplevelOr cache.False) then
                        foundmatch <- true
                    else
                        // check if some input can be skipped
                        if
                            Solver.notElemOfSet _startsetPredicate (cache.MintermForLocation(loc))
                             && (_toplevelOr.CanSkip || refEq _toplevelOr cache.False)
                        then
                            // jump mid-regex
                            loc.Position <- tryJumpToStartset (cache, &loc, _toplevelOr)

        if foundmatch then
            match currentMax with
            | ValueSome(n) when n <> loc.Position ->
                if isNullable(cache,&loc,_toplevelOr) then
                    currentMax <- (ValueSome loc.Position)
            | _ ->
                if _initialWithoutDotstar.IsAlwaysNullable then
                    currentMax <- (ValueSome loc.Position)

        currentMax

let rec createDerivative
    (
        c: RegexCache<uint64>,
        loc: Location,
        loc_pred: uint64,
        node: RegexNode<uint64>
    )
    : RegexNode<uint64>
    =
    let inline Der newNode = createDerivative (c, loc, loc_pred, newNode) //
    match c.Builder.GetTransitionInfo(loc_pred,node) with
    | ValueSome n -> n
    | _ ->
        let result =
            match node with
            // Derx (R) = ⊥ if R ∈ ANC or R = ()
            | LookAround _
            | Epsilon -> c.False
            // Der s⟨i⟩ (ψ) = if si ∈ [[ψ]] then () else ⊥
            | Singleton pred ->
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
                    c.Builder.mkConcat2 (Der R, c.Builder.mkLoop (R, decr low, decr up))


                | false ->
                    // Derx (R·R{m −1, n −1, l})
                    c.Builder.mkConcat2 (
                        R,
                        c.Builder.mkLoop (R, decr low, decr up)
                    )
                    |> Der

            // Derx (R | S) = Derx (R) | Derx (S)
            | Or(xs, info) ->
                xs
                |> Seq.map Der
                |> c.Builder.mkOr
                //
                // use mutable e = xs.GetEnumerator()
                // c.Builder.mkOrEnumerator (&e, Der)



            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, info) as head ->
                xs
                |> Seq.map Der
                |> c.Builder.mkAnd

            // Derx(~R) = ~Derx (R)
            | Not(inner, info) ->
                c.Builder.mkNot (Der inner)
            // Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, info) ->
                let R' = Der head
                let R'S = c.Builder.mkConcat2 (R', tail)
                if RegexNode.isNullable (c, &loc, head) then
                    let S' = Der(tail)
                    if refEq c.Builder.uniques._false S' then R'S else
                    // let newConcat = c.Builder.mkOr [|R'S; S'|]
                    let newConcat = c.Builder.mkOr [|R'S; S'|]
                    newConcat
                else R'S

        if not (containsLookaround node) then
            c.Builder.AddTransitionInfo(loc_pred, node, result)

        result
