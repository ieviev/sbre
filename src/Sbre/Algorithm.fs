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
        | LookAround(node = node'; lookBack = false) ->
            let revBody = rev builder node'
            builder.mkLookaround(revBody, true)
        // (?<=R)r = (?=Rr)
        | LookAround(node = node'; lookBack = true) ->
            let revBody = rev builder node'
            builder.mkLookaround(revBody, false)
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



