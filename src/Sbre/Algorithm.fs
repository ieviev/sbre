module rec Sbre.Algorithm

open System
open System.Buffers
open System.Collections.Immutable
open System.Runtime.InteropServices
open Sbre.CountingSet
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


    let rec isNullable(cache: RegexCache<_>, state:RegexState, loc: inref<Location>, node: RegexNode<_>) : bool =

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
                found <- isNullable (cache, state, &loc, e.Current)
            found
        // Nullx (R) and Nullx (S)
        | And(xs, info) ->
            use mutable e = xs.GetEnumerator()
            let mutable forall = true
            while forall && e.MoveNext() do
                forall <- isNullable (cache, state, &loc, e.Current)
            forall
        // Nullx (R{m, n}) = m = 0 or Nullx (R)
        | Loop(R, low, _, info) -> low = 0 || (isNullable (cache,state, &loc, R))
        // not(Nullx (R))
        | Not(inner, info) ->
            if info.NodeFlags.IsCounter then
                failwith "counter nullability"
                // match state.CounterCanExit(node) with
                // | ValueSome(true) -> not (isNullable (cache,state, &loc, inner))
                // | ValueSome(false) -> true
                // | _ -> not (isNullable (cache,state, &loc, inner))
            else
                not (isNullable (cache,state, &loc, inner))
        // Nullx (R) and Nullx (S)
        | Concat(head, tail, info) ->
            if info.NodeFlags.IsAlwaysNullable then true else
            if info.NodeFlags.IsCounter then
                let counter = state.TryGetCounter(node)
                match counter with
                | ValueNone -> false
                | ValueSome counter ->
                let canExit = counter.CanExit()
                match canExit with
                | true -> isNullable (cache,state, &loc, tail)
                | false -> false
            else
                isNullable (cache,state, &loc, head) && isNullable (cache,state, &loc, tail)
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
            if info.NodeFlags.HasCounter then ValueNone else
            let mutable e = CollectionsMarshal.AsSpan(info.Transitions)

            let mutable looping = true
            let mutable i = 0

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
        state:RegexState,
        locationPredicate,
        loc:byref<Location>,
        cache:RegexCache<TSet>,
        updated:RegexNode<_>
        ) : bool =
        let mutable isFinalNullable = false
        if refEq cache.False updated then
            isFinalNullable <-
                match toplevelOr.TryGetInfo with
                | ValueSome info ->
                    info.CanBeNullable
                    && (info.IsAlwaysNullable || isNullable (cache,state, &loc, toplevelOr))
                | _ -> isNullable (cache, state, &loc, toplevelOr)
        toplevelOr <- updated
        isFinalNullable

    let matchEnd
        (cache: RegexCache<TSet>)
        (loc: byref<Location>)
        (initialNode: RegexNode<TSet>)
        (toplevelOr: byref<RegexNode<TSet>>)
        : int voption
        =
        let mutable foundmatch = false
        let _state = RegexState(cache.NumOfMinterms())
        let _startsetPredicate : TSet = cache.Solver.Full
        let _builder = cache.Builder
        let _initialAlwaysNullable = isAlwaysNullable initialNode
        toplevelOr <- initialNode

        let mutable currentMax =
            // compute initial nullability
            match _initialAlwaysNullable with
            | true ->
                ValueSome loc.Position
            | _ ->
                ValueNone

        while not foundmatch do
            if Location.isFinal loc then
                foundmatch <- true
            else
                let loc_pred = cache.MintermForLocation(loc)



                // current active branches
                if not (refEq toplevelOr cache.False) then
                    let updated = createDerivative (cache, _state, &loc, loc_pred, toplevelOr)
                    let isFinalNullable =
                        deriveActiveBranch(
                            &toplevelOr,
                            _state,
                            loc_pred,&loc,cache,updated)
                    if isFinalNullable then
                        currentMax <- ValueSome loc.Position
                        foundmatch <- true

                if not foundmatch then
                    loc.Position <- Location.nextPosition loc
                    // check nullability
                    if
                        canBeNullableV (toplevelOr.TryGetInfo,toplevelOr)
                    then
                        if isAlwaysNullable toplevelOr || isNullable (cache, _state, &loc, toplevelOr) then
                            currentMax <- (ValueSome loc.Position)

                    CountingSet.stepCounters _state loc_pred
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
                            failwith "todo: predicate optimization"
                            // let chars = cache.GetInitialSearchValues()
                            // cache.TryNextStartsetLocationArray(&loc,cache.GetInitialStartsetPrefix().Span,chars)
                        // jump mid-regex
                        match toplevelOr.TryGetInfo with
                        | ValueSome i ->
                            failwith "todo can skip"
                            // if i.NodeFlags.HasFlag(Flag.CanSkipFlag) then
                            //     loc.Position <- tryJumpToStartset cache &loc toplevelOr
                        | ValueNone -> ()



        if foundmatch then
            if isNullable (cache, _state, &loc, toplevelOr) then
                currentMax <- (ValueSome loc.Position)

        currentMax



let rec createCounterDerivative
    (
        cache: RegexCache<TSet>,
        state : RegexState,
        loc: inref<Location>,
        loc_pred: TSet,
        node: RegexNode<TSet>
    )
    : RegexNode<TSet> =
        match node with
        | Concat(head, tail, info) ->
            match head with
            | Loop(Singleton pred, headLow, headUp, headInfo) ->
                match Solver.elemOfSet loc_pred pred with
                | false ->
                    state.RemoveCounter(node) |> ignore
                    cache.False
                | true ->

                let counter = state.GetOrInitializeCounter(node)
                let counterState = counter.GetState()
                let offset = counter.Offset

                match counterState with
                | CounterState.Fail -> cache.False
                | CounterState.CanIncr ->
                    let headDerivative =
                        createDerivative (cache, state,&loc, loc_pred, Singleton pred)
                    if refEq headDerivative cache.False then
                        // counter.Reset()
                        cache.False
                    else
                        node
                | CounterState.CanExit ->
                    let deriv = createDerivative (cache, state,&loc, loc_pred, tail)
                    // let deriv = tail
                    deriv
                | CounterState.CanIncrExit ->
                    // let deriv = tail
                    let deriv = createDerivative (cache, state,&loc, loc_pred, tail)
                    cache.Builder.mkOr(seq { node; deriv })
            | _ ->
                failwith "??"
        | _ ->
            failwith "other counter"





let rec createDerivative
    (
        cache: RegexCache<TSet>,
        state : RegexState,
        loc: inref<Location>,
        loc_pred: TSet,
        node: RegexNode<TSet>
    )
    : RegexNode<TSet>
    =

        let result =

            let info = node.GetFlags()
            if info.IsCounter then
                createCounterDerivative(cache,state,&loc,loc_pred,node)
            else

            match node with
            // Derx (R) = ⊥ if R ∈ ANC or R = ()
            | LookAround _
            | Epsilon -> cache.False
            // Der s⟨i⟩ (ψ) = if si ∈ [[ψ]] then () else ⊥
            | Singleton pred ->
                // if c.Solver.isElemOfSet(pred,loc_pred) then Epsilon else c.False
                if Solver.elemOfSet pred loc_pred then Epsilon else cache.False

            // Derx (R{m, n}) =
            // if m=0 or Null ∀(R)=true or Nullx (R)=false
            // then Derx (R)·R{m −1, n −1}
            // else Derx (R·R{m −1, n −1})
            | Loop(R, low, up, info) ->

                // CSA
                let a = 1
                let inline decr x =
                    if x = Int32.MaxValue || x = 0 then x else x - 1

                let case1 =
                    low = 0
                    || info.IsAlwaysNullable = true
                    || not (RegexNode.isNullable (cache, state, &loc, R))

                match case1 with
                | true ->
                    // Derx (R)·R{m −1, n −1, l}
                    let derR = createDerivative (cache, state, &loc, loc_pred, R)
                    cache.Builder.mkConcat2 (derR, cache.Builder.mkLoop (R, decr low, decr up))

                | false ->
                    // Derx (R·R{m −1, n −1, l})
                    createDerivative (
                        cache,
                        state,
                        &loc,
                        loc_pred,
                        cache.Builder.mkConcat2 (R, cache.Builder.mkLoop (R, decr low, decr up))
                    )



            // Derx (R | S) = Derx (R) | Derx (S)
            | Or(xs, info) ->
                let derivatives = ResizeArray()
                for n in xs do
                    derivatives.Add (createDerivative (cache, state,&loc, loc_pred, n))
                derivatives |> cache.Builder.mkOr

            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, info) as head ->
                let derivatives = ResizeArray()
                for n in xs do
                    derivatives.Add (createDerivative (cache, state,&loc, loc_pred, n))
                cache.Builder.mkAnd(derivatives)

            // Derx(~R) = ~Derx (R)
            | Not(inner, info) ->
                let derR = createDerivative (cache, state,&loc, loc_pred, inner)
                cache.Builder.mkNot (derR)
            // Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, info) ->

                let R' = createDerivative (cache, state,&loc, loc_pred, head)
                let R'S = cache.Builder.mkConcat2 (R', tail)

                if RegexNode.isNullable (cache, state, &loc, head) then
                    let S' = createDerivative (cache, state,&loc, loc_pred, tail)
                    if refEq cache.Builder.uniques._false S' then
                        R'S
                    else
                        cache.Builder.mkOr ( seq { R'S ;S' } )
                else
                    R'S

        // if not (containsLookaround node) && not node.HasCounter then
        //     cache.Builder.AddTransitionInfo(loc_pred, node, result)

        result
