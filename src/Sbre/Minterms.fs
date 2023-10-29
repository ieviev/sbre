module Sbre.Minterms

open System
open System.Collections.Generic
open System.Text
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Info
open Sbre.Types
open Sbre.Pat


let rec transform
    (builder: RegexBuilder<'tnewset>)
    (charsetSolver: CharSetSolver)
    (newSolver: ISolver<'tnewset>)
    (node: RegexNode<BDD>)
    : RegexNode<'tnewset> =
    let transformInner = transform builder charsetSolver newSolver
    let transformList: RegexNode<BDD> list -> RegexNode<'tnewset> list =
        fun nodes ->
        nodes |> List.map (transform builder charsetSolver newSolver)

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
    | LookAround (body, back, neg) -> LookAround (transformInner body, back, neg)
    | Concat(head,tail, info) ->
        let head' = transformInner head
        let tail' = transformInner tail
        builder.mkConcat2(head',tail')
    | Epsilon -> Epsilon


let collectSetsHs (_: SymbolicRegexBuilder<'tset>) (node: RegexNode<'tset>) =
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
        | LookAround (body, _, _) -> collect body
        | Concat(head,tail, info) ->
            collect head
            collect tail
        | Epsilon -> ()
    collect node
    hs

let compute (builder: SymbolicRegexBuilder<'tset>) (node: RegexNode<'tset>) =
    let hs = collectSetsHs builder node
    let list =
        MintermGenerator<'tset>.GenerateMinterms (builder._solver, hs)
    list.Sort()
    list.ToArray()



