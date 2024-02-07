module Sbre.Minterms

open System.Collections.Generic
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types

let rec transform
    (oldBuilder: RegexBuilder<BDD>)
    (builder: RegexBuilder<'tnewset>)
    (charsetSolver: CharSetSolver)
    (newSolver: ISolver<'tnewset>)
    (node: RegexNode<BDD>)
    : RegexNode<'tnewset> =

    match builder.UniquesDict.TryGetValue(node) with
    | true, v -> v
    | _ ->

    let inline transformInner v = transform oldBuilder builder charsetSolver newSolver v
    match node with
    | Singleton tset -> builder.one(newSolver.ConvertFromBDD(tset, charsetSolver))
    | Not(xs,info) -> builder.mkNot(transformInner xs)
    | And (xs,info) ->
        let xs' = xs |> map transformInner
        builder.mkAnd(xs' |> Seq.toArray)
    | Or (xs,info) ->

        let xs' = xs |> map transformInner
        builder.mkOr(xs' |> Seq.toArray)
    | Loop (xs, lower, upper,info) ->
        let xs' = transformInner xs
        builder.mkLoop(xs',lower,upper)
    | LookAround (body, back, neg, pendingNullable) -> LookAround (transformInner body, back, neg, pendingNullable)
    | Concat(head,tail, info) ->
        let head' = transformInner head
        let tail' = transformInner tail
        builder.mkConcat2(head',tail')
    | Epsilon -> Epsilon


let collectSets (node: RegexNode<'tset>) =
    let hs = HashSet()
    let rec collect (node: RegexNode<'tset>) : unit =
        let collectList (nodes: RegexNode<'tset> seq) =
            nodes |> Seq.iter collect
        match node with
        | Singleton pred -> hs.Add pred |> ignore
        | Or (xs,_) ->
            collectList xs
        | And (xs,info) ->
            collectList xs
        | Loop (node=node) -> collect node
        | Not (node,info) -> collect node
        | LookAround (node=body) -> collect body
        | Concat(head,tail, info) ->
            collect head
            collect tail
        | Epsilon -> ()
    collect node
    hs

let compute (builder: SymbolicRegexBuilder<'tset>) (node: RegexNode<'tset>) =
    let hs = collectSets node
    let list = MintermGenerator<'tset>.GenerateMinterms (builder._solver, hs)
    list.Sort()
    list.ToArray()


