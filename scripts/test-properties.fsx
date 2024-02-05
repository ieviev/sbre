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
        $"\t{mt, -23}=>\t{node}"
    )
    |> String.concat "\n"

let printMergedImmediateDerivatives (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    let derivs = getImmediateDerivatives cache node |> merge cache

    derivs
    |> Seq.map (fun (mt, node) ->
        let mt = cache.PrettyPrintMinterm(mt)
        let node = cache.PrettyPrintNode(node)
        $"\t{mt, -23}=>\t{node}"
    )
    |> String.concat "\n"

let printDerivatives (cache: RegexCache<_>) (derivs: (TSet * RegexNode<TSet>) seq) =
    derivs
    |> Seq.map (fun (mt, node) ->
        let mt = cache.PrettyPrintMinterm(mt)
        let node = cache.PrettyPrintNode(node)
        $"\t{mt, -23}=>\t{node}"
    )
    |> String.concat "\n"


let nonInitialNonFalseDerivatives
    (cache: RegexCache<TSet>)
    (redundantNodes: System.Collections.Generic.HashSet<RegexNode<TSet>>)
    (node: RegexNode<TSet>)
    =
    getImmediateDerivatives cache node
    |> merge cache
    |> Seq.where (fun (mt, deriv) -> not (redundantNodes.Contains(deriv)))
    |> Seq.toArray

// ----

let m, c =
    // let r = Sbre.Regex("Huck[a-zA-Z]+|Saw[a-zA-Z]+")
    let r = Sbre.Regex("[a-z]shing")
    // let r = Sbre.Regex("Twain")
    r.TSetMatcher, r.TSetMatcher.Cache


// ideal case
// let rec calcPrefix (cache: RegexCache<_>)  acc (node: RegexNode<_>) =
//     let prefix_derivs = nonInitialNonFalseDerivatives c node

//     match prefix_derivs with
//     | [| (mt, deriv) |] ->
//         let acc' = mt :: acc
//         calcPrefix cache acc' deriv
//     | _ ->
//         printDerivatives cache prefix_derivs |> stdout.WriteLine
//         acc |> Seq.map cache.PrettyPrintMinterm |> Seq.rev |> Seq.toList

let rec calcPrefixSets (cache: RegexCache<_>) acc (node: RegexNode<_>) =
    let redundant = System.Collections.Generic.HashSet<RegexNode<TSet>>([ cache.False ])

    let rec loop node =
        if not (redundant.Add(node)) then
            []
        else if node.IsAlwaysNullable then
            acc |> List.rev
        else
            let prefix_derivs = nonInitialNonFalseDerivatives c redundant node
            match prefix_derivs with
            | [| (mt, deriv) |] ->
                let acc' = mt :: acc
                calcPrefixSets cache acc' deriv
            | _ ->
                // let merged_pred = prefix_derivs |> Seq.map fst |> Seq.fold (|||) cache.Solver.Empty
                prefix_derivs |> Seq.map snd |> Seq.iter (redundant.Add >> ignore)
                acc |> List.rev

    loop node

let printPrefixSets (cache:RegexCache<_>) (sets:TSet list) = 
    sets
    |> Seq.map cache.PrettyPrintMinterm
    |> String.concat ";"

let rec applyPrefixSets (cache:RegexCache<_>) (node:RegexNode<TSet>) (sets:TSet list) = 
    assert (not node.ContainsLookaround)
    match sets with 
    | [] -> node
    | head :: tail -> 
        let loc = Pat.Location.getDefault ()
        let state = Sbre.CountingSet.RegexState(cache.NumOfMinterms())
        let der = Algorithm.createDerivative (cache, state, &loc, head, node)
        applyPrefixSets cache der tail

let pref = calcPrefixSets c [] m.ReversePattern
printPrefixSets c pref

let afterPrefix =
    applyPrefixSets c m.ReverseTrueStarredPattern pref

afterPrefix.ToString()

string m.ReversePattern

let c1 = nonInitialNonFalseDerivatives c m.ReversePattern
printDerivatives c c1
let c2 = nonInitialNonFalseDerivatives c (snd c1[0])
printDerivatives c c2

let c31 = nonInitialNonFalseDerivatives c (snd c2[0])
printDerivatives c c31
let c2 = nonInitialNonFalseDerivatives c (snd c1[0])
printDerivatives c c2
