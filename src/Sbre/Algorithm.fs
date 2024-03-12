module rec Sbre.Algorithm

open Sbre.Types

module RegexNode =

    let rec rev (builder: RegexBuilder<_>) (node: RegexNode<_>) =
        match node with
        // ψr = ψ
        | Singleton _ -> node
        // // (R|S)r = Rr|Sr
        | Or(xs, info) ->
            let xs' = xs |> map (rev builder)
            builder.mkOrSeq xs'
        // R{m, n, b}r = Rr{m, n, b}
        | Loop(xs, low, up, info) ->
            let xs' = (rev builder) xs
            builder.mkLoop (xs', low, up)
        // (R & S)r = Rr & S r
        | And(xs, info) ->
            let xs' = xs |> map (rev builder)
            builder.mkAnd (xs')
        // (~R)r = ~(Rr)
        | Not(xs, info) ->
            let xs' = (rev builder) xs
            builder.mkNot xs'
        // (?=R)r = (?<=Rr)
        | LookAround(node = node'; lookBack = false) ->
            let revBody = rev builder node'
            builder.mkLookaround(revBody, true, 0, RefSet.empty)
        // (?<=R)r = (?=Rr)
        | LookAround(node = node'; lookBack = true) ->
            let revBody = rev builder node'
            builder.mkLookaround(revBody, false, 0, RefSet.empty)
        | Concat _ ->
            let rec revConcatNode acc curr =
                match curr with
                | Concat(head, (Concat _ as tail), tinfo) ->
                    revConcatNode (rev builder head :: acc) tail
                | Concat(head, tail, tinfo) -> rev builder tail :: rev builder head :: acc
                | single -> rev builder single :: acc
            let reversedList = revConcatNode [] node
            builder.mkConcat reversedList
        | Epsilon -> node
        | Begin | End -> node

    let inline getCachedTransition(pred: ^t, node: RegexNode< ^t >) =
        node.TryGetInfo
        |> ValueOption.bind (fun info ->
            match info.Transitions.TryGetValue(pred) with
            | true, v -> ValueSome v
            | _ -> ValueNone
        )


    let inline getEndCachedTransition(pred: ^t, node: RegexNode< ^t >) =
        node.TryGetInfo
        |> ValueOption.bind (fun info ->
            match info.EndTransitions.TryGetValue(pred) with
            | true, v -> ValueSome v
            | _ -> ValueNone
        )

    let inline getStartCachedTransition(pred: ^t, node: RegexNode< ^t >) =
        node.TryGetInfo
        |> ValueOption.bind (fun info ->
            match info.StartTransitions.TryGetValue(pred) with
            | true, v -> ValueSome v
            | _ -> ValueNone
        )




