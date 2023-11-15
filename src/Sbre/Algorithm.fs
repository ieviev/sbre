module rec Sbre.Algorithm

open System
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
        // B: EXTENDED REGEXES
        // (R & S)r = Rr & S r
        | And(xs, info) ->
            let xs' = xs |> map (rev builder)
            builder.mkAnd (Seq.toArray xs')
        // (~R)r = ~(Rr)
        | Not(xs, info) ->
            let xs' = (rev builder) xs
            builder.mkNot xs'
        // 3.7 Lookarounds
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
            match matchEnd (cache, &loc1, ValueNone, body) with
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
        // 3.7 Lookarounds
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

    // max(x,⊥) = max(⊥,x) = x
    let inline maxPos(x: int voption, y: int voption) =
        match y with
        | ValueSome _ -> y
        | ValueNone -> x

    let matchEnd
        (
            cache: RegexCache<uint64>,
            loc: byref<Location>,
            initialMax: int voption,
            initialNode: RegexNode<uint64>
        )
        : int voption
        =

        let mutable currentMax = initialMax
        let mutable looping = true
        let mutable foundmatch = false

        // initial node
        let _implicitDotstarred = cache.IsImplicitDotStarred initialNode

        // current active branches, without implicit dotstar node
        let toplevelOr =
            // cache only top matchEnds for now
            if _implicitDotstarred then cache.GetTopLevelOr()
            elif cache.IsOrigReversePattern(initialNode) then cache.GetReverseTopLevelOr()
            else ToplevelORCollection()

        let _initialWithoutDotstar =
            if _implicitDotstarred then
                cache.InitialPatternWithoutDotstar
            else
                toplevelOr.Add( initialNode )
                initialNode


        let _startsetPredicate = cache.GetInitialStartsetPredicate()
        let _derivativeCache = cache.Builder.DerivativeCache

        while looping do

            let mutable canSkipAll = true
            let mutable canBeNullableBranch = false
            let mutable alwaysNullableBranch = false
            if Location.isFinal loc then
                foundmatch <- true

            // 3.3 Derivatives and MatchEnd optimizations
            match foundmatch with
            | true ->
                looping <- false
                // check if any nullable in the end
                match currentMax with
                | ValueSome(n) when n <> loc.Position ->
                    let mutable topSpan = toplevelOr.Items()
                    let mutable found = false
                    let mutable i = 0
                    while not found && i < topSpan.Length  do
                        found <- isNullable(cache,&loc,topSpan[i])
                        i <- i + 1
                    if found then
                        currentMax <- (ValueSome loc.Position)
                | _ -> ()

            | false ->
                let locationPredicate = cache.MintermForLocation(loc)

                let _topCount = toplevelOr.Count
                // current active branches
                if _topCount > 0 then
                    let toplevelOrSpan = toplevelOr.Items()

                    for i = _topCount - 1 downto 0 do
                        let curr = toplevelOrSpan[i]

                        let deriv =
                            match _derivativeCache.TryGetValue(struct (locationPredicate, curr)) with
                            | true, v -> v
                            | _ ->
                                createDerivative (cache, loc, locationPredicate, curr)

                        if obj.ReferenceEquals(deriv,cache.False) then
                            if
                                currentMax.IsSome
                            then
                                // a pattern successfully matched and turned to false,
                                // so we can return match
                                foundmatch <- true
                            else
                                toplevelOr.Remove(i)
                        else
                            toplevelOr.UpdateTransition(i, deriv)

                            if canBeNullable deriv then
                                canBeNullableBranch <- true
                            if isAlwaysNullable deriv then
                                alwaysNullableBranch <- true
                            // check nullability
                            canSkipAll <- canSkipAll && deriv.CanSkip


                // create implicit dotstar derivative only if startset matches
                if
                    _implicitDotstarred
                    && currentMax.IsNone
                    && Solver.elemOfSet _startsetPredicate locationPredicate
                then

                    let deriv =
                        // attempt to not even call createDerivative
                        match _derivativeCache.TryGetValue(struct (locationPredicate, _initialWithoutDotstar)) with
                        | true, v -> v
                        | _ ->
                            createDerivative (cache, loc, locationPredicate, _initialWithoutDotstar)

                    match deriv with
                    | _ when refEq deriv cache.Builder.uniques._false -> ()
                    | deriv ->
                        if canBeNullable deriv then canBeNullableBranch <- true
                        if isAlwaysNullable deriv then alwaysNullableBranch <- true
                        canSkipAll <- canSkipAll && canSkip deriv

                        match toplevelOr.Count with
                        | 0 -> toplevelOr.Add( deriv)
                        | 1 ->
                            let first = toplevelOr.First
                            if refEq first deriv || first.IsAlwaysNullable then () else
                            match cache.Builder.AndSubsumptionCache.TryGetValue(struct(first,deriv)) with
                            | true, true -> ()
                            | true, false -> toplevelOr.Add( deriv)
                            | _ ->
                            if cache.Builder.trySubsumeTopLevelOr(first,deriv) then () else
#if OPTIMIZE
                            failwith "top or not subsumed"
#endif
                            toplevelOr.Add( deriv )
                        | _ ->

#if OPTIMIZE
                            let first = toplevelOr.First
                            [| first; deriv |]
                            |> Array.map (fun v -> v.ToString())
                            // |> Array.map (fun v -> v.ToStringHelper())
                            |> String.concat "\n"
                            |> failwith
#endif
                            toplevelOr.Add( deriv )

                if not foundmatch then
                    // check nullability
                    loc.Position <- Location.nextPosition loc
                    if alwaysNullableBranch then
                        currentMax <- (ValueSome loc.Position)
                    elif canBeNullableBranch then
                        let mutable e = toplevelOr.Items().GetEnumerator()
                        let mutable found = false
                        while not found && e.MoveNext() do
                            if e.Current.CanBeNullable then
                                found <- e.Current.IsAlwaysNullable || isNullable(cache,&loc,e.Current)
                        if found then
                            currentMax <- (ValueSome loc.Position)

                    // won't try to jump further if final
                    if Location.isFinal loc then () else
                    if toplevelOr.Count = 0 then
                        if not _implicitDotstarred then
                            looping <- false
                        else
                            if Solver.notElemOfSet _startsetPredicate (cache.MintermForLocation(loc)) then
                                loc.Position <- tryJumpToStartset (cache, &loc, &toplevelOr)
                    else
                        // check if some input can be skipped
                        if
                            canSkipAll &&
                            Solver.notElemOfSet _startsetPredicate (cache.MintermForLocation(loc))
                        then
                            // jump mid-regex
                            loc.Position <- tryJumpToStartset (cache, &loc, &toplevelOr)


        currentMax


/// creates derivative without returning initial dot-starred pattern
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


    match c.Builder.DerivativeCache.TryGetValue(struct (loc_pred, node)) with
    | true, v -> v
    | _ ->

        //
        let result =
            match node with
            // Derx (R) = ⊥ if R ∈ ANC or R = ()
            | LookAround _
            | Epsilon -> c.False
            // ----------------

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
                use mutable e = xs.GetEnumerator()
                c.Builder.mkOrEnumerator (&e, Der)

            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, info) as head ->
                use mutable e = xs.GetEnumerator()
                c.Builder.mkAndEnumerator (&e, Der)

            // Derx(~R) = ~Derx (R)
            | Not(inner, info) ->
                c.Builder.mkNot (Der inner)

            // Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, info) ->
                let R' = Der head
                // Derx (R)·S
                let R'S = c.Builder.mkConcat2 (R', tail)
                if RegexNode.isNullable (c, &loc, head) then
                    let S' = Der(tail)
                    if refEq c.Builder.uniques._false S' then R'S else
                    let newConcat = c.Builder.mkOr [|R'S; S'|]
                    newConcat
                else
                    // Derx (R)·S
                    R'S

        if not (containsLookaround node) then
            c.Builder.DerivativeCache.Add(struct (loc_pred, node), result)

        result
