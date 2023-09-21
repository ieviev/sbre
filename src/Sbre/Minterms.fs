module Sbre.Minterms

open System
open System.Collections.Generic
open System.Text
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open Sbre.Patterns


let rec transform
    (cache: SingletonCache<'tnewset>)
    (charsetSolver: CharSetSolver)
    (newSolver: ISolver<'tnewset>)
    (node: RegexNode<BDD>)
    : RegexNode<'tnewset> =
    let transformList: RegexNode<BDD> list -> RegexNode<'tnewset> list =
        fun nodes ->
        nodes |> List.map (transform cache charsetSolver newSolver)

    match cache.tryCache node with
    | ValueSome node -> node
    | _ ->

    match node with
    | Singleton tset -> Singleton(newSolver.ConvertFromBDD(tset, charsetSolver))
    | Not(xs,info) ->
        let xs' = transformList xs
        let newInfo = Info.ofNegationInner(xs',newSolver,info)
        Not(xs',newInfo)
    | And (xs,info) ->

        let xs' =
            if isNull xs then Unchecked.defaultof<_> else
            xs |> map transformList

        let newInfo = Info.convertFromSetBDD (xs',newSolver, info)
        And(xs', newInfo)

    | Or (xs,info) ->
        let xs' =
            if isNull xs then Unchecked.defaultof<_> else
            xs |> map transformList
        let newInfo = Info.convertFromSetBDD (xs',newSolver, info)
        Or(xs', newInfo)

    | Loop (xs, lower, upper,info) ->
        let xs' = transformList xs
        let newInfo = Info.convertLoop(xs',newSolver,info)
        Loop(xs',lower,upper,newInfo)
    | LookAround (body, back, neg) -> LookAround (transformList body, back, neg)


let collectSetsHs (_: SymbolicRegexBuilder<'tset>) (node: RegexNode<'tset> list) =
    let hs = HashSet()
    let rec collect (node: RegexNode<'tset>) : unit =
        let collectList (nodes: RegexNode<'tset> list) =
            nodes |> List.iter collect
        match node with
        | Singleton pred -> hs.Add pred |> ignore
        | Or (xs,_) ->
            xs |> iter collectList
        | And (xs,info) ->
            xs |> iter collectList
        | Loop (node=node) -> collectList node
        | Not (node,info) -> collectList node
        | LookAround (body, _, _) -> collectList body
    List.iter collect node
    hs

let compute (builder: SymbolicRegexBuilder<'tset>) (node: RegexNode<'tset> list) =
    let hs = collectSetsHs builder node
    let list =
        MintermGenerator<'tset>.GenerateMinterms (builder._solver, hs)
    list.Sort()
    list.ToArray()



