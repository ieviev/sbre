module rec Sbre.Regex

open System
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
    let rec rev (cache: RegexCache<uint64>) (node: RegexNode<_>) =
        match node with
        // ψr = ψ
        | Singleton _ -> node
        // // (R|S)r = Rr|Sr
        | Or(xs, info) ->
            let xs' = xs |> map (rev cache)

            cache.Builder.mkOr (Seq.toArray xs')
        // R{m, n, b}r = Rr{m, n, b}
        | Loop(xs, low, up, info) ->
            let xs' = (rev cache) xs
            cache.Builder.mkLoop (xs', low, up)
        // B: EXTENDED REGEXES
        // (R & S)r = Rr & S r
        | And(xs, info) ->
            let xs' = xs |> map (rev cache)

            cache.Builder.mkAnd (Seq.toArray xs')
        // (~R)r = ~(Rr)
        | Not(xs, info) ->
            let xs' = (rev cache) xs
            cache.Builder.mkNot xs'
        // 3.7 Lookarounds
        // (?=R)r = (?<=Rr)
        | LookAround(node = node'; lookBack = false; negate = false) ->
            LookAround((rev cache) node', lookBack = true, negate = false)
        // (?<=R)r = (?=Rr)
        | LookAround(node = node'; lookBack = true; negate = false) ->
            LookAround((rev cache) node', lookBack = false, negate = false)
        // (?!R)r = (?<!Rr)
        | LookAround(node = node'; lookBack = false; negate = true) ->
            LookAround((rev cache) node', lookBack = true, negate = true)
        // (?<!R)r = (?!Rr)
        | LookAround(node = node'; lookBack = true; negate = true) ->
            LookAround((rev cache) node', lookBack = false, negate = true)

        | Concat(head, tail, info) ->
            let rev_tail =
                let der = rev cache tail
                if der = tail then tail else der

            let rev_head =
                let der = rev cache head
                if der = head then head else der

            match rev_tail with
            | Concat(thead, ttail, tinfo) ->
                let inner = cache.Builder.mkConcat2 (ttail, rev_head)
                cache.Builder.mkConcat2 (thead, inner)
            | _ -> cache.Builder.mkConcat2 (rev_tail, rev_head)
        | Epsilon -> Epsilon


    let rec isNullable(cache: RegexCache<uint64>, loc: Location, node: RegexNode<uint64>) : bool =
        let inline Null node = isNullable (cache, loc, node)

        let recursiveIsMatch (loc:Location) body =
            let mutable loc1 = (loc)
            match matchEnd (cache, &loc1, ValueNone, body) with
            | ValueNone -> false
            | ValueSome _ -> true

        // 3.2 Nullability and anchor-contexts
        match node with
        // Nullx () = true
        | Epsilon -> true
        // Nullx (ψ) = false
        | Singleton _ -> false
        // Nullx (R) or Nullx (S)
        | Or(info = CanNotBeNullable) -> false
        | Or(info = IsAlwaysNullable) -> true
        | Or(xs, info) -> (if isNull xs then false else exists Null xs)
        // Nullx (R) and Nullx (S)
        | And(info = CanNotBeNullable as info) -> false
        | And(info = IsAlwaysNullable as info) -> true
        | And(xs, info) -> if isNull xs then true else forall Null xs
        // Nullx (R{m, n}) = m = 0 or Nullx (R)
        | Loop(info = CanNotBeNullable as info) -> false
        | Loop(info = IsAlwaysNullable as info) -> true
        | Loop(R, low, _, info) -> low = 0 || Null R
        // not(Nullx (R))
        | Not(info = CanNotBeNullable as info) -> false
        | Not(info = IsAlwaysNullable as info) -> true
        | Not(node, info) -> not (Null node)
        // Nullx (R) and Nullx (S)
        | Concat(info = CanNotBeNullable as info) -> false
        | Concat(info = IsAlwaysNullable as info) -> true
        | Concat(head, tail, _) -> isNullable (cache, loc, head) && isNullable (cache, loc, tail)
        // 3.7 Lookarounds
        | LookAround(body, lookBack, negate) ->
            match lookBack, negate with
            | false, false -> recursiveIsMatch loc body // Nullx ((?=R)) = IsMatch(x, R)
            // Nullx ((?!R)) = not IsMatch(x, R)
            | false, true ->
                not (recursiveIsMatch loc body)

            | true, false -> // Nullx ((?<=R)) = IsMatch(xr, Rr)
                let revnode = (RegexNode.rev cache body)
                let revloc = (Location.rev loc)
                let ism = recursiveIsMatch revloc revnode
                ism
            | true, true -> // Nullx ((?<!R)) = not IsMatch(x r, Rr)
                let loc_rev = Location.rev loc
                let R_rev = RegexNode.rev cache body
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
            // optimize the top matchEnd only for now
            // if initialIsDotStarred then cache.GetTopLevelOr()
            // else
                new ToplevelORCollection()

        let initialWithoutDotstar =
            if initialIsDotStarred then
                cache.InitialPatternWithoutDotstar
            else
                initialNode

        let startsetPredicate = cache.GetInitialStartsetPredicate()

        if not initialIsDotStarred then
            let branchNullPos = if isNullable (cache, loc, initialNode) then loc.Position else -1
            toplevelOr.Add(initialNode, branchNullPos, loc.Position)

        // let inputSpan = loc.Input.AsSpan()

        while looping do

            // 3.3 Derivatives and MatchEnd optimizations
            match Location.isFinal loc || foundmatch with
            | true ->
                // check if any null
                let mutable topSpan = toplevelOr.Items
                let mutable found = false
                let mutable hadEpsilon = false
                let mutable lastNullable = -1

                let frameCount = System.Diagnostics.StackTrace().FrameCount
                if frameCount < 116 then
                    ()


                for i = (topSpan.Length - 1) downto 0 do
                    let currLastNullable = toplevelOr.GetLastNullPos(i)
                    lastNullable <- max lastNullable currLastNullable
                    let curr = topSpan[i]
                    if not found then
                        found <- isNullable(cache,loc,curr)

                    // if e.Current.ContainsEpsilon then
                    //     hadEpsilon <- true
                    //     match cache.Builder.purgeEpsilons e.Current with
                    //     | ValueSome n ->
                    //         found <- isNullable(cache,loc,n)
                    //     | _ -> ()
                    // else


                if found then
                    currentMax <- (ValueSome loc.Position)
                elif currentMax.IsNone && lastNullable > -1 then
                    currentMax <- (ValueSome lastNullable)




                looping <- false

            | false ->
                let locationPredicate = cache.MintermForLocation(loc)

                if initialIsDotStarred then
                    stdout.WriteLine (loc.DebugDisplay)
                    for n in toplevelOr.Items do
                        stdout.WriteLine (n.ToStringHelper())
#if DIAGNOSTIC

                // match node with
                // | LookAround(_) -> ()
                // | _ ->
                //     let diagMsg =
                //         [
                //             loc.DebugDisplay()
                //             $"{node.ToStringHelper()} -> {result.ToStringHelper()}"
                //         ]
                //         |> String.concat "\n"
                //
                //     stdout.WriteLine diagMsg
#endif


                // current active branches
                if toplevelOr.Count > 0 then
                    let toplevelOrSpan = toplevelOr.Items

                    for i = (toplevelOrSpan.Length - 1) downto 0 do
                        let curr = toplevelOrSpan[i]

                        let deriv = createDerivative (cache, loc, locationPredicate, curr)



                        match deriv with
                        | IsFalse cache ->
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

                        // else
                        | _ ->
                            // if obj.ReferenceEquals(curr, deriv) then
                            //     // optimization
                            //     if curr.CanNotBeNullable then ()
                            //     elif curr.IsAlwaysNullable then
                            //         toplevelOr.UpdateNullability(i, loc.Position + 1)
                            //     else
                            //         if isNullable (cache, loc, deriv) then
                            //             toplevelOr.UpdateTransition(i, deriv, loc.Position + 1)
                            //         else
                            //             // in the case of a negation, we can return match
                            //             match deriv with
                            //             | Not _ when toplevelOr.GetLastNullPos(i) > -1 -> foundmatch <- true
                            //             | And(nodes=nodes) when
                            //                 nodes |> Seq.exists (function | Not (info=info) when info.CanNotBeNullable -> true | _ -> false) ->
                            //                 foundmatch <- true
                            //             | _ -> ()
                            //             toplevelOr.UpdateTransitionNode(i, deriv)
                            // else

                                // if deriv.IsAlwaysNullable then
                                //     toplevelOr.UpdateTransition(i, deriv, loc.Position + 1)
                                // elif deriv.CanNotBeNullable then
                                //     toplevelOr.UpdateTransitionNode(i, deriv)
                                //     if toplevelOr.GetLastNullPos(i) > -1 then
                                //         failwith $"todo: {deriv.ToStringHelper()}"
                                //         match deriv with
                                //         | Not _ -> foundmatch <- true
                                //         | And(nodes=nodes) when
                                //             nodes |> Seq.exists (function | Not (info=info) when info.CanNotBeNullable -> true | _ -> false) ->
                                //             foundmatch <- true
                                //         | _ -> ()
                                // else
                                    if isNullable (cache, loc, deriv) then
                                        if initialIsDotStarred then
                                            if toplevelOr.Items.Length > 3 && deriv.ToStringHelper() = "(.*-|ε)" then
                                                failwith (curr.ToStringHelper())
                                        toplevelOr.UpdateTransition(i, deriv, loc.Position)
                                    else
                                        // in the case of a negation, we can return match
                                        if toplevelOr.GetLastNullPos(i) > -1 then
                                            match curr with
                                            | Not _ -> foundmatch <- true
                                            | And(nodes=nodes) when
                                                nodes |> Seq.exists (function | Not (info=info) when info.CanNotBeNullable -> true | _ -> false) ->
                                                foundmatch <- true
                                            | _ -> ()
                                        toplevelOr.UpdateTransitionNode(i, deriv)


                // create implicit dotstar derivative only if startset matches
                if
                    initialIsDotStarred && Solver.isElemOfSetU64 startsetPredicate locationPredicate
                then
                    match createDerivative (cache, loc, locationPredicate, initialWithoutDotstar) with
                    | IsFalse cache -> ()
                    | deriv ->
                        let nullableState = if isNullable (cache, loc, deriv) then loc.Position else -1
                        toplevelOr.Add(
                            deriv,
                            nullableState,
                            loc.Position)

                // found successful match - exit early
                if not foundmatch then
                    let mutable e = toplevelOr.Items.GetEnumerator()
                    let mutable found = false
                    let mutable canSkipAll = true

                    // check nullability
                    loc.Position <- Location.nextPosition loc

                    // let frameCount = System.Diagnostics.StackTrace().FrameCount
                    // if frameCount < 116 then
                    //     ()

                    while e.MoveNext() do
                        found <- found || isNullable(cache,loc,e.Current)
                        canSkipAll <- canSkipAll && e.Current.CanSkip
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
                            ()
                            // use our own startset lookup (not optimized for long strings)
                            // if not (Solver.isElemOfSetU64 startsetPredicate nextLocationPredicate) then
                            //     loc.Position <- tryJumpToStartset (cache, &loc, &toplevelOr)
                    else



                        // check if current position is nullable or skippable

                        if
                            canSkipAll &&
                            not (Solver.isElemOfSetU64 startsetPredicate nextLocationPredicate)
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
                // 16.7.ms
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
                let ders =
                    xs |> Seq.sortBy LanguagePrimitives.PhysicalHash |> Seq.map Der |> Seq.toArray
                let newOr = c.Builder.mkOr ders

                // use mutable e = xs.GetEnumerator()
                // let newOr = c.Builder.mkOrEnumerator (&e,Der)

                // let isSameDerivative() =
                //     match newOr with
                //     | Or(newxs, _) ->
                //         xs.Count = newxs.Count && (xs, newxs) ||> Seq.forall2 (fun v1 v2 -> v1 = v2)
                //     | _ -> false

                newOr

            // B: EXTENDED REGEXES
            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, info) as head ->
                // todo: slightly faster but unreliable
                use mutable e = xs.GetEnumerator()
                let newAnd = c.Builder.mkAndEnumerator (&e, Der)
                newAnd


            // Derx(~R) = ~Derx (R)
            | Not(inner, info) ->
                let innerDerivative = Der inner

                if refEq innerDerivative inner then
                    node
                else

                    let newNot = c.Builder.mkNot innerDerivative


// #if DEBUG
//                     // TODO: force reference equals
//                     if not (obj.ReferenceEquals(newNot, node)) && isSameDerivative () then
//                         // node
//                         failwith $"DEBUG: duplicate derivative in: {node}"
//                     else
// #endif

                    newNot

            // 3.3: Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, info) ->
                let R' = Der head

                // Derx (R)·S
                let R'S =
                    // e.g. head is T*
                    match R' with
                    | Epsilon -> tail
                    | _ when obj.ReferenceEquals(R', head) -> node
                    | IsFalse c -> c.Builder.uniques._false
                    | _ ->
                        let newConcat = c.Builder.mkConcat2 (R', tail)
                        newConcat


                if RegexNode.isNullable (c, loc, head) then
                    let S' = Der(tail)
                    if refEq c.Builder.uniques._false S' then R'S else
                    // c.Builder.TempArray[0] <- R'S
                    // c.Builder.TempArray[1] <- S'
                    // c.Builder.TempArray
                    let newConcat = c.Builder.mkOr [|R'S; S'|]
                    newConcat
                else
                    // Derx (R)·S
                    R'S



        //
        // let asstr = "(?<=-.*).*"
        // if result = c.Builder.uniques._false && node.ToStringHelper() = asstr then
        //     let dbg = 1
        //
        //     let l2 = loc |> Location.withPos (loc.Position - 1)
        //     // let n2 = createDerivative(c,l2,c.MintermForLocation(l2),node)
        //     let n2 = createDerivative(c,loc,loc_pred,node)
        //
        //     let nodeInfo =
        //         match node with
        //         | Concat(h,t,info) ->
        //             [
        //                 $"{n2.ToStringHelper()}"
        //                 $"%A{info}"
        //                 $"%A{h}"
        //                 $"%A{t}"
        //             ]
        //             |> String.concat "\n"
        //         | _ -> failwith "todo"
        //     failwith nodeInfo

        match node with
        | Concat(h, LookAround(_),_) ->
             // if ["(.*-|ε)"; "(ε|.*-)"] |> List.contains (result.ToStringHelper()) then
             //    if node.ToStringHelper() <> ".*-" && node.ToStringHelper() <> "(.*-|ε)" then
             //        failwith $"{node.ToStringHelper()}"
             //    ()
            let r = result
            ()
        | And(h,_) ->
            let r = result
            ()
        | _ -> ()

// #if DIAGNOSTIC
//
//         match node with
//         | LookAround(_) -> ()
//         | _ ->
//             let diagMsg =
//                 [
//                     loc.DebugDisplay()
//                     $"{node.ToStringHelper()} -> {result.ToStringHelper()}"
//                 ]
//                 |> String.concat "\n"
//
//             stdout.WriteLine diagMsg
// #endif

        if not node.ContainsLookaround then
            c.Builder.DerivativeCache.Add(struct (loc_pred, node), result)

        result
