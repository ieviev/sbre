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
open Sbre.Optimizations
open System.Collections.Generic

let pat = """["'][^"']{0,30}[?!\.]["']"""
let r = Sbre.Regex(pat)
let m = r.TSetMatcher
let c = r.TSetMatcher.Cache
let prefix1 = r.InitialReversePrefix

let prefixSets1 =
    match prefix1 with
    | InitialOptimizations.SetsPrefix(prefix, transitionId) -> 
        Array.toList (prefix.ToArray())
    | _ -> failwith "debug"

let prefixPretty1 = Optimizations.printPrefixSets c (prefixSets1)

// "["'];[!.?]"

let s = Sbre.CountingSet.RegexState(c.NumOfMinterms())
let l (char:char) = Location.create $"%c{char}" 0
let lp (loc:Location) = c.MintermForLocation(loc)

let mkDer (c:char) (n:RegexNode<_>) = 
    let loc = l c
    m.CreateDerivative(s, &loc, lp loc, n)    

let state (n:RegexNode<_>) = m.GetOrCreateState(n)

let printders  (c:RegexCache<_>) (ders:(TSet * RegexNode<TSet>)[]) =
    ders
    |> Array.map (fun (ts,n) -> 
        $"\t{c.PrettyPrintMinterm(ts), -23}==>  {c.PrettyPrintNode(n)}"
    )
    |> String.concat "\n"

let der1 = mkDer '"' m.ReverseTrueStarredPattern

stdout.WriteLine der1
(state der1).Flags

let der2 = mkDer '.' der1
stdout.WriteLine der2

(state der2).Flags // None

let ders2 = 
    Optimizations.getNonRedundantDerivatives c (HashSet()) der2 

printders c (ders2)

// [^"']                  ==>([^"']{0,29}["']|⊤*["'][!.?][^"']{0,30}["'])
//         ["']                   ==>([!.?][^"']{0,30}["']|ε|⊤*["'][!.?][^"']{0,30}["']
let der3 = mkDer '.' der2
stdout.WriteLine der3
// (([^"']){0,29}["']|⊤*["'][!.?]([^"']){0,30}["'])
(state der3).Flags // None
let ders3 = Optimizations.getNonRedundantDerivatives c (HashSet()) der3
printders c (ders3)

// "     [^"']                  ==>(⊤*["'][!.?][^"']{0,30}["']|[^"']{0,28}["'])
//         ["']                   ==>([!.?][^"']{0,30}["']|ε|⊤*["'][!.?][^"']{0,30}["'])"



// let ders1 = Optimizations.getNonRedundantDerivatives c redundant node 
    // match ders1 with 
    // | [| d1m,d1n; (d2m,d2n) |] -> 
    //     redundant.Add(d1n) |> ignore
    //     redundant.Add(d2n) |> ignore
    //     let ders21 = Optimizations.getNonRedundantDerivatives c redundant d1n 
    //     let ders22 = Optimizations.getNonRedundantDerivatives c redundant d2n
    //     Some (Array.collect id [|ders21; ders22|])
    // | _ -> 
    //     None


// canskiplimited, either "' or pos -30
let hasLimitedSkip (initial:RegexNode<_>) (node:RegexNode<_>) =
    let redundant = HashSet([initial])
    let skipTerm = m.GetOrCreateState(initial).Startset
    match node with 
    | Or(nodes, info) -> 
        let nonInitial = nodes |> Seq.where (fun v -> not (refEq v initial)) |> c.Builder.mkOr
        let nonInitialDerivs = Optimizations.getNonRedundantDerivatives c redundant nonInitial
        let nonInitialNonTerm =
            nonInitialDerivs |> Array.where (fun (mt,node) -> not (Solver.contains skipTerm mt) ) 
        let nonTermDerivatives (node: RegexNode<TSet>) = 
            let ders1 = Optimizations.getNonRedundantDerivatives c redundant node
            ders1 |> Array.where (fun (mt,node) -> not (Solver.contains skipTerm mt) )
        match nonInitialNonTerm with 
        | [| singlePath |] -> 
            let path = ResizeArray()
            let rec loop (node: RegexNode<_>) =
                match nonTermDerivatives node with 
                | [| (mt,single) |] when (not (node.CanBeNullable || refEq c.False node || c.Solver.IsFull(mt))) ->
                    redundant.Add(node) |> ignore
                    path.Add(mt)
                    loop single
                | _ -> node
            let finalNode = loop (snd singlePath)
            Some (
                {|
                    TermPred = skipTerm
                    TermTransition = Algorithm.createStartsetDerivative (c,  skipTerm, node)
                    SkipDistanceSet = path
                    NonTermTransition = (c.Builder.mkOr [finalNode; initial])
                |}
            )
        | _ -> None
    | _ -> None
    
let ls = (hasLimitedSkip (m.ReverseTrueStarredPattern) (der2)).Value
// stdout.WriteLine ls.NonTermTransition
// stdout.WriteLine ls.SkipDistanceSet.Count


stdout.WriteLine ls.NonTermTransition
stdout.WriteLine ls.TermTransition

// ls.Count
// printders c [|ls|]

// stdout.WriteLine ls[0]
// ([^"']){0,30}["']








