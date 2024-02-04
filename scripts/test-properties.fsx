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


let merge (cache: RegexCache<_>) derivs =
    derivs
    |> Seq.groupBy (fun (mt, node) -> node)
    |> Seq.map (fun (_, group) ->
        group |> Seq.map fst |> Seq.fold (|||) cache.Solver.Empty, group |> Seq.head |> snd
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

let printMergedImmediateDerivatives (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    let derivs = getImmediateDerivatives cache node |> merge cache

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
let printMergeDerivs node = printMergedImmediateDerivatives cache node
let print derivs = printDerivatives cache derivs
let getDerivs node = getImmediateDerivatives cache node

// let allMinterms = matcher.Cache.Minterms()
// // [|"[^\na-d]"; "\n"; "a"; "b"; "c"; "d"|]
// let prettyMinterms = allMinterms |> Array.map cache.PrettyPrintMinterm

let initial = matcher.ReverseTrueStarredPattern

printImmediateDerivatives cache initial
printMergeDerivs initial

let nonInitialDerivatives =
    getImmediateDerivatives cache initial
    |> Array.where (fun (mt, node) -> not (refEq node initial))

printDerivatives cache nonInitialDerivatives

nonInitialDerivatives.Length // there is only 1 potential derivative




// let a = Sbre.Optimizations.NoOptimizations


let nextmt, nextnode = nonInitialDerivatives[0]

let immediate2 = getImmediateDerivatives cache nextnode

printImmediate nextnode

let remaining2 =
    immediate2
    |> Array.where (fun (mt, node) -> not (refEq node initial || refEq node nextnode))

print remaining2


let next3mt, next3node = remaining2[0]

printImmediate next3node


// dc[^\n]

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
