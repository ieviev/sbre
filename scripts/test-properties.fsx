#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types
open Sbre.Pat

let regex = Sbre.Regex("ab.*cd")
let matcher = regex.TSetMatcher
let cache = matcher.Cache

let getImmediateDerivatives (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    cache.Minterms()
    |> Array.map (fun minterm ->
        let loc = Pat.Location.getDefault ()
        let state = Sbre.CountingSet.RegexState(cache.NumOfMinterms())
        let der = Algorithm.createDerivative (cache, state, &loc, minterm, node)
        minterm, der
    )

let printImmediateDerivatives (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    let derivs = getImmediateDerivatives cache node
    derivs
    |> Seq.map (fun (mt, node) ->
        let mt = cache.PrettyPrintMinterm(mt)
        let node = cache.PrettyPrintNode(node)
        $"\t{mt, -20}=>\t{node}"
    )
    |> String.concat "\n"

let printDerivatives (cache: RegexCache<_>) (derivs: (TSet * RegexNode<TSet>) seq) =
    derivs
    |> Seq.map (fun (mt, node) ->
        let mt = cache.PrettyPrintMinterm(mt)
        let node = cache.PrettyPrintNode(node)
        $"\t{mt, -20}=>\t{node}"
    )
    |> String.concat "\n"


let printImmediate node = printImmediateDerivatives cache node
let print derivs = printDerivatives cache derivs
let getDerivs node = getImmediateDerivatives cache node

// let allMinterms = matcher.Cache.Minterms()
// // [|"[^\na-d]"; "\n"; "a"; "b"; "c"; "d"|]
// let prettyMinterms = allMinterms |> Array.map cache.PrettyPrintMinterm

let node1 = matcher.ReverseTrueStarredPattern
let node1immediate = getImmediateDerivatives cache node1

printImmediateDerivatives cache node1

let nonInitialDerivatives =
    node1immediate 
    |> Array.where (fun (mt, node) -> not (refEq node node1) )

printDerivatives cache nonInitialDerivatives

nonInitialDerivatives.Length // there is only 1 potential derivative

// let a = Sbre.Optimizations.NoOptimizations


let nextmt,nextnode = nonInitialDerivatives[0]


printImmediate nextnode





let rec prefix (acc: 't list) (node: RegexNode<_>) =
    match node with
    | Concat(head, tail, info) ->
        match prefix [] head with
        | [] -> acc // no more prefix
        | headPrefix -> prefix (headPrefix @ acc) tail
    | Epsilon -> acc
    | Or(nodes, info) -> failwith "todo"
    | Singleton(set) -> failwith "todo"
    | Loop(node, low, up, info) -> failwith "todo"
    | And(nodes, info) -> failwith "todo"
    | Not(node, info) -> failwith "todo"
    | LookAround(node, lookback, negate) -> failwith "todo"
