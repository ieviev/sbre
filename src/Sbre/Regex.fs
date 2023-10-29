module rec Sbre.Regex

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open FSharpx.Collections
open Microsoft.FSharp.Core.CompilerServices
open Sbre.Info
open Sbre.Optimizations
open Sbre.Pat
open Sbre.Types
open Sbre.Cache

module P = Sbre.Pat
module C = Sbre.Cache


module Helpers =
    let isNullablePosition
        (
            c: RegexCache<uint64>,
            loc: inref<Location>,
            // nodes: HashSet<ToplevelOR>
            // nodes: ResizeArray<ToplevelOR>
            nodes: ToplevelORCollection
        )
        : bool
        =
        let mutable e = nodes.Items.GetEnumerator()
        let mutable isNullable = false

        while e.MoveNext() && not isNullable do
            let curr = e.Current

            match Regex.RegexNode.isNullable (c, loc, curr) with
            | true -> isNullable <- true
            | _ -> ()

        isNullable


module RegexNode =

#if DEBUG
    let display(node: RegexNode<'tset>) = node.ToStringHelper()

    let displayIndividual(node: RegexNode<'tset> list) =
        node |> Seq.map (fun v -> v.ToStringHelper()) |> String.concat "\n"



#endif

    // 3.6 Reversal

    // Concat is done to a list
    //  (R·S)r = Sr·Rr
    // important identities are preserved with cache
    let rec rev (cache: RegexCache<'tset>) (node: RegexNode<'tset>) =
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
            cache.Builder.mkNot (xs')
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

            // if
            //     head = rev_head
            //     && tail = rev_tail
            // then
            //     node
            // else

            cache.Builder.mkConcat2 (rev_tail, rev_head)
        | Epsilon -> Epsilon


    let rec isNullable(cache: RegexCache<uint64>, loc: Location, node: RegexNode<uint64>) : bool =
        let inline Null node = isNullable (cache, loc, node)

        let isMatch' loc body =
            match matchEnd (cache, loc, ValueNone, body) with
            | ValueNone -> false
            | ValueSome _ -> true


#if DIAGNOSTIC
        //logDiagnostic $"{RegexNode.display node}"
#endif
        // 3.2 Nullability and anchor-contexts
        // epsilon is considered an empty list of nodes
        match node with
        // Nullx () = true
        | Epsilon -> true
        // Nullx (ψ) = false
        | Singleton _ -> false
        // Nullx (R) or Nullx (S)
        | Or(info = Info.CanNotBeNullable) -> false
        | Or(info = Info.IsAlwaysNullable) -> true
        | Or(xs, info) -> (if isNull xs then false else exists Null xs)
        // Nullx (R) and Nullx (S)
        | And(info = Info.CanNotBeNullable as info) -> false
        | And(info = Info.IsAlwaysNullable as info) -> true
        | And(xs, info) -> if isNull xs then true else forall Null xs
        // Nullx (R{m, n}) = m = 0 or Nullx (R)
        | Loop(info = Info.CanNotBeNullable as info) -> false
        | Loop(info = Info.IsAlwaysNullable as info) -> true
        | Loop(R, low, _, info) -> low = 0 || Null R
        // not(Nullx (R))
        | Not(info = Info.CanNotBeNullable as info) -> false
        | Not(info = Info.IsAlwaysNullable as info) -> true
        | Not(node, info) -> not (Null node)
        // Nullx (R) and Nullx (S)
        | Concat(info = Info.CanNotBeNullable as info) -> false
        | Concat(info = Info.IsAlwaysNullable as info) -> true
        | Concat(head, tail, _) -> isNullable (cache, loc, head) && isNullable (cache, loc, tail)
        // 3.7 Lookarounds
        | LookAround(body, lookBack, negate) ->
            match lookBack, negate with
            | false, false -> isMatch' loc body // Nullx ((?=R)) = IsMatch(x, R)
            // Nullx ((?!R)) = not IsMatch(x, R)
            | false, true ->
                let ism = isMatch' loc body
                not ism

            | true, false -> // Nullx ((?<=R)) = IsMatch(xr, Rr)
                let revnode = (RegexNode.rev cache body)
                let revloc = (Location.rev loc)
                let ism = isMatch' revloc revnode
                ism
            | true, true -> // Nullx ((?<!R)) = not IsMatch(x r, Rr)
                let loc_rev = Location.rev loc
                let R_rev = RegexNode.rev cache body
                not (isMatch' loc_rev R_rev)


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
    let inline jumpNextLocation(cache: RegexCache<'t>, loc: byref<Location>) : bool =
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


    let tryMakeTransition(cache: RegexCache<_>, loc: Location, currNode: RegexNode<_>, loc_pred) =
        createDerivative (cache, loc, loc_pred, currNode)


    // 3.3 Derivatives and MatchEnd: if Final(x) then Nullx (R) else max(Nullx (R), MatchEnd(x+1, Derx (R)))
    let matchEnd
        (
            cache: RegexCache<uint64>,
            initialLocation: Location,
            initialMax: int voption,
            initialNode: RegexNode<uint64>
        )
        : int voption
        =

        let mutable loc = initialLocation
        let mutable currentMax = initialMax
        // current active branches, without implicit dotstar node
        let mutable toplevelOr = ToplevelORCollection()
        let tempArray = ResizeArray()
        let mutable looping = true
        let mutable foundmatch = false

        // initial node
        let initialIsDotStarred = cache.IsImplicitDotStarred initialNode

        let initialWithoutDotstar =
            if initialIsDotStarred then
                cache.InitialPatternWithoutDotstar
            else
                initialNode


        let startsetPredicate =
            Info.Startset.inferStartset (cache.Solver) (initialWithoutDotstar)

        if not initialIsDotStarred then
            let branchNullPos = if isNullable (cache, loc, initialNode) then loc.Position else -1
            // ToplevelOR.Of(initialNode, branchNullPos)
            toplevelOr.Add(initialNode, branchNullPos) |> ignore

        let inline exitWith(pos: int voption) =
            currentMax <- pos
            looping <- false


        while looping do
            // 3.3 Derivatives and MatchEnd optimizations
            match Location.isFinal loc || foundmatch with
            | true ->
                let isnullable = (fun v -> isNullable (cache, loc, v))

                match toplevelOr with
                | ToplevelOrNullable isnullable -> exitWith (ValueSome loc.Position)
                | _ -> exitWith (currentMax)

            | false ->

                let locationPredicate = cache.MintermForLocation(loc)


                let createDerivatives() =
                    // current active branches
                    if toplevelOr.Count > 0 then
                        tempArray.Clear()
                        tempArray.AddRange(toplevelOr.Items)

                        // for curr in toplevelOr.Items() do
                        for curr in tempArray do
                            let derivative = tryMakeTransition (cache, loc, curr, locationPredicate)


                            match derivative with
                            | (Cache.IsFalse cache as derivative) ->
                                if
                                    currentMax.IsSome
                                    // && toplevelOr.GetLastNullPos(curr) curr.LastNullablePos > -1
                                    && toplevelOr.GetLastNullPos(curr) > -1
                                then
                                    // a pattern successfully matched and turned to false,
                                    // so we can return match
                                    foundmatch <- true
                                else
                                    toplevelOr.Remove(curr)

                            | deriv ->

                                if obj.ReferenceEquals(curr, deriv) then
                                    if curr.CanNotBeNullable then
                                        ()
                                    elif curr.IsAlwaysNullable then
                                        toplevelOr.BumpIsAlwaysNullable(curr, loc.Position + 1)
                                    else
                                        let newNullablePos =
                                            if isNullable (cache, loc, deriv) then
                                                loc.Position + 1
                                            else
                                                // in the case of a negation, we can return match
                                                let lastNullPos = toplevelOr.GetLastNullPos(curr)

                                                match deriv with
                                                // | Not(_) when curr.LastNullablePos > -1 ->
                                                | Not(_) when lastNullPos > -1 -> foundmatch <- true
                                                | _ -> ()

                                                lastNullPos

                                        toplevelOr.UpdateTransition(curr, deriv, newNullablePos)
                                else

                                    let currloc = loc

                                    let newNullablePos =
                                        if isNullable (cache, loc, deriv) then
                                            loc.Position + 1
                                        else
                                            // in the case of a negation, we can return match
                                            let lastNullPos = toplevelOr.GetLastNullPos(curr)

                                            match deriv with
                                            // | Not(_) when curr.LastNullablePos > -1 ->
                                            | Not(_) when lastNullPos > -1 -> foundmatch <- true
                                            | _ -> ()

                                            lastNullPos

                                    toplevelOr.UpdateTransition(curr, deriv, newNullablePos)

                    // create implicit dotstar derivative only if startset matches
                    if
                        cache.Solver.isElemOfSet (startsetPredicate, locationPredicate)
                        && initialIsDotStarred
                    then


                        let newNode =
                            tryMakeTransition (cache, loc, initialWithoutDotstar, locationPredicate)

                        match newNode with
                        | Cache.IsFalse cache -> ()
                        | deriv ->
                            let branchNullPos =
                                if isNullable (cache, loc, deriv) then loc.Position else -1

                            toplevelOr.Add(deriv, branchNullPos) |> ignore


                createDerivatives ()

                // for node in pendingRemoval do
                //     toplevelOr.Remove(node)
                //     |> ignore
                //
                // pendingRemoval.Clear()
                //
                // for node in pendingAddition do
                //     toplevelOr.Add(node)
                //     |> ignore
                //
                // pendingAddition.Clear()

                // found successful match - exit early
                if foundmatch = true then
                    ()
                else

                    loc.Position <- Location.nextPosition loc

#if DIAGNOSTIC
                    // if loc.Position < 500 then
                    //     let locationstr =
                    //         if Location.isFinal loc then
                    //             ""
                    //         else
                    //             loc.DebugDisplay()
                    //
                    //     logDiagnostic ($"pos:{locationstr}\n{displayTopOR toplevelOr}")
#endif

                    if toplevelOr.Count = 0 then
                        if not initialIsDotStarred then
                            looping <- false
                        else
                            let oldLocation = loc.Position
                            Optimizations.tryJumpToStartset (cache, &loc, &toplevelOr)
                    else
                        if Helpers.isNullablePosition (cache, &loc, toplevelOr) then
                            currentMax <- ValueSome(loc.Position)

                        if
                            not (cache.Solver.isElemOfSet (startsetPredicate, locationPredicate))
                        then
                            Optimizations.tryJumpToStartset (cache, &loc, &toplevelOr)


        currentMax



let refEqualsComparer: IEqualityComparer<struct ('t * RegexNode<'t>)> =
    { new IEqualityComparer<struct (^t * RegexNode< ^t >)> with
        member this.Equals((x1, x2), (y1, y2)) = obj.ReferenceEquals(x2, y2)
        member this.GetHashCode((x, y)) = 0
    }

let equalsComparer: IEqualityComparer<struct ('t * RegexNode<'t>)> =
    { new IEqualityComparer<struct (^t * RegexNode< ^t >)> with
        member this.Equals((x1, x2), (y1, y2)) = obj.Equals(x2, y2)
        member this.GetHashCode((x, y)) = 0
    }


let set1: HashSet<struct (uint64 * RegexNode<uint64>)> =
    HashSet(comparer = refEqualsComparer)

let set2: HashSet<struct (uint64 * RegexNode<uint64>)> =
    HashSet(comparer = equalsComparer)

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
            | Singleton pred -> if c.IsValidPredicate(pred, loc_pred) then Epsilon else c.False

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

                let newOr = c.Builder.mkOr (ders)

                let isSameDerivative() =
                    match newOr with
                    | Or(newxs, _) ->
                        xs.Count = newxs.Count && (xs, newxs) ||> Seq.forall2 (fun v1 v2 -> v1 = v2)
                    | _ -> false


                // if isSameDerivative() then node else newOr

#if DEBUG
                // TODO: force reference equals
                // if not (obj.ReferenceEquals(newOr, node)) && isSameDerivative () then
                //     let h1 = LanguagePrimitives.PhysicalHash newOr
                //     let h2 = LanguagePrimitives.PhysicalHash head
                //     failwith $"TODO: {head.ToStringHelper()}"
                // else
#endif

                newOr

            // B: EXTENDED REGEXES
            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, info) as head ->
                let ders = xs |> Seq.map Der |> Seq.toArray

                let newAnd = c.Builder.mkAnd (ders)

                let isSameDerivative() =
                    match newAnd with
                    | And(newXs, _) ->
                        xs.Count = newXs.Count && (xs, newXs) ||> Seq.forall2 (fun v1 v2 -> v1 = v2)
                    | _ -> false

#if DEBUG
                // TODO: force reference equals
                if not (obj.ReferenceEquals(newAnd, head)) && isSameDerivative () then
                    failwith $"DEBUG: duplicate derivative in: {head}"
#endif
                newAnd


            // Derx(~R) = ~Derx (R)
            | Not(inner, info) ->
                let innerDerivative = Der inner

                if refEq innerDerivative inner then
                    node
                else

                    let newNot = c.Builder.mkNot (innerDerivative)

                    let isSameDerivative() =
                        match newNot with
                        | Not(newInner, _) ->
                            let v = inner = newInner
                            v
                        | _ -> false


#if DEBUG
                    // TODO: force reference equals
                    if not (obj.ReferenceEquals(newNot, node)) && isSameDerivative () then
                        // node
                        failwith $"DEBUG: duplicate derivative in: {node}"
                    else
#endif

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
                    | _ ->
                        let newConcat = c.Builder.mkConcat2 (R', tail)
                        newConcat


                if RegexNode.isNullable (c, loc, head) then
                    let S' = Der(tail)
                    let newConcat = c.Builder.mkOr [| R'S; S' |]
                    newConcat
                else
                    // Derx (R)·S
                    R'S


        if not (node.ContainsLookaround) then
            c.Builder.DerivativeCache.Add(struct (loc_pred, node), result)

        result
