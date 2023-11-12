module rec Sbre.Algorithm

open System
open System.Reflection
open System.Runtime.InteropServices
open FSharp.Data.Adaptive
open FSharpx.Collections
open Sbre.Info
open Sbre.Optimizations
open Sbre.Pat
open Sbre.Types
open Sbre.Cache

module P = Sbre.Pat
module C = Sbre.Cache

module RegexNode =

#if DEBUG
    let display(node: RegexNode<'tset>) = node.ToStringHelper()
#endif

    // 3.6 Reversal

    //  (R·S)r = Sr·Rr
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
                | Concat(head, (Concat(_) as tail), tinfo) ->
                    revConcatNode (rev builder head :: acc) tail
                | Concat(head, tail, tinfo) ->
                    rev builder tail :: rev builder head :: acc
                | single -> rev builder single :: acc

            let reversedList = revConcatNode [] node
            builder.mkConcat reversedList
        | Epsilon -> Epsilon


    let rec isNullable(cache: RegexCache<uint64>, loc: Location, node: RegexNode<uint64>) : bool =
        let inline Null node = isNullable (cache, loc, node)

        let recursiveIsMatch (loc:Location) body =
            let mutable loc1 = (loc)
            match matchEnd (cache, &loc1, ValueNone, body) with
            | ValueNone -> false
            | ValueSome _ -> true

        if not (canBeNullable node) then false
        elif isAlwaysNullable node then true
        else

        // 3.2 Nullability and anchor-contexts
        match node with
        // Nullx () = true
        | Epsilon -> true
        // Nullx (ψ) = false
        | Singleton _ -> false
        // Nullx (R) or Nullx (S)
        | Or(xs, info) -> (if isNull xs then false else exists Null xs)
        // Nullx (R) and Nullx (S)
        | And(xs, info) -> if isNull xs then true else forall Null xs
        // Nullx (R{m, n}) = m = 0 or Nullx (R)
        | Loop(R, low, _, info) -> low = 0 || Null R
        // not(Nullx (R))
        | Not(node, info) -> not (Null node)
        // Nullx (R) and Nullx (S)
        | Concat(head, tail, _) -> isNullable (cache, loc, head) && isNullable (cache, loc, tail)
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

    /// 3.3 Derivatives and MatchEnd: Null(fail)x(R) = if Nullx (R) then x else None
    let isNullpos(cache: RegexCache<uint64>, loc: Location, node: RegexNode<uint64>) =
        match isNullable (cache, loc, node) with
        | true -> ValueSome(loc.Position)
        | false -> ValueNone


    /// uses .NET FindMatchOptimizations to find the next startset
    let inline jumpNextDotnetLocation(cache: RegexCache<'t>, loc: byref<Location>) : bool =
        let mutable newPos = loc.Position

        let success =
            cache.Optimizations.TryFindNextStartingPositionLeftToRight(
                loc.Input.AsSpan(),
                &newPos,
                loc.Position
            )

        if success then
            loc.Position <- newPos
            true
        else
            false

    let enumeratorToSeq (enumerator:System.Collections.IEnumerator) = seq {while enumerator.MoveNext() do enumerator.Current}

    // 3.3 Derivatives and MatchEnd: if Final(x) then Nullx (R) else max(Nullx (R), MatchEnd(x+1, Derx (R)))
    // [<MethodImpl(MethodImplOptions.AggressiveOptimization)>]
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
        let initialIsDotStarred = cache.IsImplicitDotStarred initialNode

        // current active branches, without implicit dotstar node
        let toplevelOr =
            // optimize the top matchEnds only for now
            if initialIsDotStarred then cache.GetTopLevelOr()
            elif cache.IsOrigReversePattern(initialNode) then cache.GetReverseTopLevelOr()
            else new ToplevelORCollection()

        let _initialWithoutDotstar =
            if initialIsDotStarred then
                cache.InitialPatternWithoutDotstar
            else
                initialNode


        if not initialIsDotStarred then
            let branchNullPos = if isNullable (cache, loc, initialNode) then loc.Position else -1
            toplevelOr.Add(initialNode, branchNullPos, loc.Position)


        let _startsetPredicate = cache.GetInitialStartsetPredicate()
        let _derivativeCache = cache.Builder.DerivativeCache


        while looping do

            let mutable canSkipAll = true
            let mutable canBeNullableBranch = false

            // 3.3 Derivatives and MatchEnd optimizations
            match Location.isFinal loc || foundmatch with
            | true ->
                // check if any null
                let mutable topSpan = toplevelOr.Items()
                let mutable found = false
                match currentMax with
                | ValueSome(n) when n <> loc.Position ->
                    for i = (topSpan.Length - 1) downto 0 do
                        let currLastNullable = toplevelOr.GetLastNullPos(i)
                        let curr = topSpan[i]
                        if not found then
                            found <- isNullable(cache,loc,curr)

                    if found then
                        currentMax <- (ValueSome loc.Position)
                | _ -> ()

                looping <- false

            | false ->
                let locationPredicate = cache.MintermForLocation(loc)

                // current active branches
                if toplevelOr.Count > 0 then
                    let toplevelOrSpan = toplevelOr.Items()

                    for i = (toplevelOrSpan.Length - 1) downto 0 do
                        let curr = toplevelOrSpan[i]

                        let deriv =
                            // attempt to not even call createDerivative
                            match _derivativeCache.TryGetValue(struct (locationPredicate, curr)) with
                            | true, v -> v
                            | _ ->
                                createDerivative (cache, loc, locationPredicate, curr)

                        match deriv with
                        | deriv when obj.ReferenceEquals(deriv,cache.False) ->
                            if
                                currentMax.IsSome
                                && toplevelOr.GetLastNullPos(i) > -1
                            then
                                // a pattern successfully matched and turned to false,
                                // so we can return match
                                if toplevelOr.IsOldestNullableBranch(i) then
                                    foundmatch <- true
                            else
                                toplevelOr.Remove(i)
                        | deriv when obj.ReferenceEquals(deriv, curr) ->
                            if isAlwaysNullable deriv || (canBeNullable deriv && isNullable (cache, loc, deriv)) then
                                toplevelOr.UpdateNullability(i, loc.Position )
                        | deriv ->
                            if isAlwaysNullable deriv || (canBeNullable deriv && isNullable (cache, loc, deriv)) then
                                toplevelOr.UpdateTransition(i, deriv, loc.Position)
                            else
                                toplevelOr.UpdateTransitionNode(i, deriv)
                        // check nullability

                        if deriv.CanBeNullable then
                            canBeNullableBranch <- true
                        canSkipAll <- canSkipAll && deriv.CanSkip




                // create implicit dotstar derivative only if startset matches
                if
                    initialIsDotStarred
                    && currentMax.IsNone
                    && Solver.isElemOfSetU64 _startsetPredicate locationPredicate
                then

                    let deriv =
                        // attempt to not even call createDerivative
                        match _derivativeCache.TryGetValue(struct (locationPredicate, _initialWithoutDotstar)) with
                        | true, v -> v
                        | _ ->
                            createDerivative (cache, loc, locationPredicate, _initialWithoutDotstar)

                    match deriv with
                    | IsFalse cache -> ()
                    | deriv ->
                        let nullableState =
                            if
                                canNotBeNullable deriv then -1
                                elif (canBeNullableBranch <- true ; isAlwaysNullable deriv) || (isNullable (cache, loc, deriv))
                                then loc.Position
                                else -1
                        canSkipAll <- canSkipAll && canSkip deriv

                        match toplevelOr.Count with
                        | 0 -> toplevelOr.Add( deriv, nullableState, loc.Position)
                        | 1 ->
                            let first = toplevelOr.First

                            if refEq first deriv || first.IsAlwaysNullable then () else
                            match cache.Builder.AndSubsumptionCache.TryGetValue(struct(first,deriv)) with
                            | true, v ->
                                if v then ()
                                else toplevelOr.Add( deriv, nullableState, loc.Position)
                            | _ ->

                            let isSubsumed = cache.Builder.trySubsumeTopLevelOr(first,deriv)
                            if isSubsumed then () else
#if OPTIMIZE
                            failwith "top or not subsumed"
#endif
                                toplevelOr.Add( deriv, nullableState, loc.Position)
                        | _ ->
#if OPTIMIZE
                            [| first; deriv |]
                            |> Array.map (fun v -> v.ToStringHelper())
                            |> String.concat "\n"
                            |> failwith
#endif
                            toplevelOr.Add(
                                deriv,
                                nullableState,
                                loc.Position)



                // found successful match - exit early
                if not foundmatch then
                    let mutable e = toplevelOr.Items().GetEnumerator()
                    let mutable found = false
                    // check nullability
                    loc.Position <- Location.nextPosition loc

                    if canBeNullableBranch then
                        while not found && e.MoveNext() do
                            if e.Current.CanBeNullable then
                                found <- e.Current.IsAlwaysNullable || isNullable(cache,loc,e.Current)
                        if found then
                            currentMax <- (ValueSome (loc.Position))

                    // won't try to jump further if final
                    if Location.isFinal loc then () else

                    // try to skip to next valid predicate if not matching
                    let nextLocationPredicate = cache.MintermForLocation(loc)

                    if toplevelOr.Count = 0 then
                        if not initialIsDotStarred then
                            looping <- false
                        else
                            // jump from initial pattern
                            // use .net startset lookup, have to be careful about negation here
                            // if not (jumpNextDotnetLocation(cache, &loc)) then
                            //     match initialWithoutDotstar with
                            //     | Not _ | Concat(head=Not _) -> ()
                            //     | _ -> looping <- false

                            // use our own startset lookup (not optimized for long strings)
                            if Solver.notElemOfSet _startsetPredicate nextLocationPredicate then
                                loc.Position <- tryJumpToStartset (cache, &loc, &toplevelOr)
                    else
                        // check if some input can be skipped
                        if
                            canSkipAll &&
                            Solver.notElemOfSet _startsetPredicate nextLocationPredicate
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
            // major optimization
            // | Cache.ReturnsInitialDerivative c loc loc_pred -> node

            // 3.3: Derx (R) = ⊥ if R ∈ ANC or R = ()
            | LookAround _
            | Epsilon -> c.False
            // ----------------

            // 3.3: Der s⟨i⟩ (ψ) = if si ∈ [[ψ]] then () else ⊥
            | Singleton pred ->
                // if c.IsValidPredicate(pred, loc_pred) then Epsilon else c.False

                // marginally faster
                if Solver.isElemOfSetU64 pred loc_pred then Epsilon else c.False

            // 3.3: Derx (R{m, n}) =
            // if m=0 or Null ∀(R)=true or Nullx (R)=false
            // then Derx (R)·R{m −1, n −1}
            // else Derx (R·R{m −1, n −1})
            | Loop(R, low, up, info) ->

                let inline decr x =
                    if x = Int32.MaxValue || x = 0 then x else x - 1

                let case1 =
                    low = 0
                    || info.IsAlwaysNullable = true
                    || not (RegexNode.isNullable (c, loc, R))

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

            // 3.3: Derx (R | S) = Derx (R) | Derx (S)
            | Or(xs, info) ->
                // let ders = xs |> Seq.map Der |> Seq.toArray
                // c.Builder.mkOr ders

                use mutable e = xs.GetEnumerator()
                c.Builder.mkOrEnumerator (&e, Der)
                // newNode

            // B: EXTENDED REGEXES
            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, info) as head ->
                use mutable e = xs.GetEnumerator()
                c.Builder.mkAndEnumerator (&e, Der)

            // Derx(~R) = ~Derx (R)
            | Not(inner, info) ->
                let innerDerivative = Der inner

                if refEq innerDerivative inner then
                    node
                else
                    c.Builder.mkNot innerDerivative


            // 3.3: Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, info) ->
                let R' = Der head

                // Derx (R)·S
                let R'S =
                    // e.g. head is T*
                    match R' with
                    | Epsilon -> tail
                    | _ when obj.ReferenceEquals(R', head) -> node
                    | IsFalse c -> R'
                    | _ -> c.Builder.mkConcat2 (R', tail)



                if RegexNode.isNullable (c, loc, head) then
                    let S' = Der(tail)
                    if refEq c.Builder.uniques._false S' then R'S else
                    let newConcat = c.Builder.mkOr [|R'S; S'|]
                    newConcat
                else
                    // Derx (R)·S
                    R'S


        if not node.ContainsLookaround then
            c.Builder.DerivativeCache.Add(struct (loc_pred, node), result)

        result
