module rec Sbre.Algorithm

open System
open System.Buffers
open System.Collections.Immutable
open System.Runtime.InteropServices
open Microsoft.FSharp.Core.CompilerServices
open Sbre.CountingSet
open Sbre.Info
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
            let revBody = rev builder node'
            builder.mkLookaround(revBody, true, false)
        // (?<=R)r = (?=Rr)
        | LookAround(node = node'; lookBack = true; negate = false) ->
            let revBody = rev builder node'
            builder.mkLookaround(revBody, false, false)
        // (?!R)r = (?<!Rr)
        | LookAround(node = node'; lookBack = false; negate = true) ->
            let revBody = rev builder node'
            builder.mkLookaround(revBody, true, true)
        // (?<!R)r = (?!Rr)
        | LookAround(node = node'; lookBack = true; negate = true) ->
            let revBody = rev builder node'
            builder.mkLookaround(revBody, false, true)
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
        | Anchor regexAnchor -> node

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



let rec createStartsetDerivative
    (
        cache: RegexCache<TSet>,
        loc_pred: TSet,
        node: RegexNode<TSet>
    )
    : RegexNode<TSet>
    =
        assert(not node.ContainsLookaround)
        let result =
            match node with
            // Derx (R) = ⊥ if R ∈ ANC or R = ()
            | LookAround _
            | Epsilon -> cache.False
            // Der s⟨i⟩ (ψ) = if si ∈ [[ψ]] then () else ⊥
            | Singleton pred ->
                // if c.Solver.isElemOfSet(pred,loc_pred) then Epsilon else c.False
                if Solver.elemOfSet pred loc_pred then cache.Builder.uniques._eps else cache.False

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

                match case1 with
                | true ->
                    // Derx (R)·R{m −1, n −1, l}
                    let derR = createStartsetDerivative (cache,  loc_pred, R)
                    cache.Builder.mkConcat2 (derR, cache.Builder.mkLoop (R, decr low, decr up))

                | false ->
                    // Derx (R·R{m −1, n −1, l})
                    createStartsetDerivative (
                        cache,
                        loc_pred,
                        cache.Builder.mkConcat2 (R, cache.Builder.mkLoop (R, decr low, decr up))
                    )



            // Derx (R | S) = Derx (R) | Derx (S)
            | Or(xs, info) ->
                let derivatives = ResizeArray()
                for n in xs do
                    derivatives.Add (createStartsetDerivative (cache,  loc_pred, n))
                derivatives |> cache.Builder.mkOr

            // Derx (R & S) = Derx (R) & Derx (S)
            | And(xs, info) as head ->
                let derivatives = ResizeArray()
                for n in xs do
                    derivatives.Add (createStartsetDerivative (cache, loc_pred, n))
                cache.Builder.mkAnd(derivatives)

            // Derx(~R) = ~Derx (R)
            | Not(inner, info) ->
                let derR = createStartsetDerivative (cache, loc_pred, inner)
                cache.Builder.mkNot (derR)
            // Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
            | Concat(head, tail, info) ->

                let R' = createStartsetDerivative (cache, loc_pred, head)
                let R'S = cache.Builder.mkConcat2 (R', tail)

                if head.IsAlwaysNullable then
                    let S' = createStartsetDerivative (cache, loc_pred, tail)
                    if refEq cache.Builder.uniques._false S' then
                        R'S
                    else
                        cache.Builder.mkOr ( seq { R'S ;S' } )
                else
                    R'S
            | Anchor _ -> cache.False

        if not (containsLookaround node) && not node.HasCounter then
            cache.Builder.AddTransitionInfo(loc_pred, node, result)

        result


// this is an experimental highly optimized way of returning llmatch
// let rec createInitialDerivative
//     (
//         cache: RegexCache<TSet>,
//         state : RegexState,
//         loc: inref<Location>,
//         loc_pred: TSet,
//         node: RegexNode<TSet>,
//         initialNode: RegexNode<TSet>
//     )
//     : RegexNode<TSet>
//     =
//         // assert ((function Or _ -> true | _ -> false) node)
//         match node with
//         | Or(nodes, info) ->
//             use e = nodes.GetEnumerator()
//             let mutable existsNullable = false
//
//             let derivatives = ResizeArray()
//             for n in nodes do
//                 if not (refEq n initialNode) then
//                     derivatives.Add (createDerivative (cache, state,&loc, loc_pred, n))
//
//             for n in Seq.takeWhile (fun _ -> not existsNullable ) derivatives do
//                 existsNullable <- RegexNode.isNullable(cache, state, &loc, n)
//
//             if not existsNullable then
//                 let initialDerivative = createDerivative (cache, state,&loc, loc_pred, initialNode)
//                 // todo: redundant branch check
//                 let w = 1
//                 derivatives.Add (initialDerivative)
//             cache.Builder.mkOr(derivatives)
//         | other ->
//             (createDerivative (cache, state,&loc, loc_pred, other))




