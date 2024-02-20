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
open Sbre.Info
open Sbre.Optimizations
open System.Collections.Generic
open Sbre.Algorithm.RegexNode
open System.Runtime.Intrinsics


let input =
    "/mnt/g/repos/rebar/benchmarks/haystacks/opensubtitles/en-sampled.txt"
    |> File.readAllText

let pattern =
    """Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"""

// S J I P
// S -> 15
// J -> 11
// I -> ? (Ir -> 11) (In -> 18)
// P -> 18

let r = Sbre.Regex(pattern)
let m = r.TSetMatcher
let c = m.Cache

let rpat = r.TSetMatcher.RawPatternWithoutLookback
stdout.WriteLine rpat

let flen = Info.Node.getFixedLength rpat

let mkder =
    (fun (mt, node) ->
        let loc = Location.getNonInitial ()
        m.CreateDerivative(&loc, mt, node)
    )

let pretty derivs =
    derivs
    |> (Array.map (fun (mt, node) -> $"{c.PrettyPrintMinterm(mt), -13} ==> {node.ToString()}"))
    |> join "\n"
    |> (fun v -> "\n" + v)


stdout.WriteLine rpat // (Sherlock Holmes|Professor Moriarty|I(nspector Lestrade|rene Adler)|John Watson)

fsi.PrintWidth <- 2000

let possibleTransitions =
    rpat |> Optimizations.getImmediateDerivatives mkder c |> Seq.toArray

let trans1 = pretty possibleTransitions
let redundant = HashSet([ c.False ])

let nonRedundantTransitions =
    rpat |> Optimizations.getNonRedundantDerivatives mkder c redundant |> Seq.toArray

let trans1 = pretty nonRedundantTransitions

type LengthLookup = 
    | FixedLength of int
    | FixedLengthSetLookup of (Memory<TSet>*int)[]
    | MatchEnd

let rec getLengthMapping
    (acc: (TSet array * int) list)
    (remainingTransitions: (TSet list * TSet * RegexNode<TSet>)[])
    =
    let transitions =
        remainingTransitions
        |> Seq.map (fun (preceding, tset, derivative) ->
            let fixLengthOption = Node.getFixedLength derivative
            let newPrecedingList = preceding @ [ tset ]

            match fixLengthOption with
            | None ->
                // continue looking
                Result.Error(newPrecedingList, derivative)
            | Some(fixLength) -> Result.Ok(newPrecedingList, fixLength)
        )
        |> Seq.toArray

    let updatedAcc =
        transitions
        |> Seq.choose (fun res ->
            match res with
            | Error _ -> None
            | Ok(prec, len) -> Some(Seq.toArray prec, (len + prec.Length))
        )
        |> Seq.toList
        |> List.append acc
    
    try 
        transitions
        |> Seq.fold
            (fun (acc: (TSet array * int) list) res ->
                match res with
                | Error(prec, der) ->
                    let newTransitions =
                        der
                        |> getNonRedundantDerivatives mkder c redundant
                        |> Seq.map (fun (v1, v2) -> prec, v1, v2)
                        |> Seq.toArray

                    match newTransitions with
                    | [||] -> acc
                    | _ when prec.Length > 5 -> failwith "could not infer length prefix"
                    | _ -> getLengthMapping acc newTransitions
                | Ok(_) -> acc
            )
            updatedAcc
        |> (fun v -> 
        )
    with e -> 


// S J I P
// S -> 15
// J -> 11
// I -> ? (Ir -> 11) (In -> 18)
// P -> 18

let initial: (TSet list * TSet * RegexNode<TSet>) array = 
    nonRedundantTransitions
    |> Seq.map (fun (v1,v2) -> [],v1,v2)
    |> Seq.toArray

let lengthMapping =
    getLengthMapping [] initial

let readable = 
    lengthMapping
    |> List.map (fun (prefix, len) -> 
        let str = prefix |> Array.map c.PrettyPrintMinterm |> join ""
        str, len
    )
    



let prefixPretty1 = Optimizations.printPrefixSets c (prefixSets1)

// "["'];[!.?]"

let l(char: char) = Location.create $"%c{char}" 0
let lp(loc: Location) = c.MintermForLocation(loc)

let mkDer (c: char) (n: RegexNode<_>) =
    let loc = l c
    m.CreateDerivative(s, &loc, lp loc, n)

let state(n: RegexNode<_>) = m.GetOrCreateState(n)

let printders (c: RegexCache<_>) (ders: (TSet * RegexNode<TSet>)[]) =
    ders
    |> Array.map (fun (ts, n) -> $"\t{c.PrettyPrintMinterm(ts), -23}==>  {c.PrettyPrintNode(n)}")
    |> String.concat "\n"

let der1 = mkDer '"' m.ReverseTrueStarredPattern

stdout.WriteLine der1
(state der1).Flags

let der2 = mkDer '.' der1
stdout.WriteLine der2

(state der2).Flags // None

let ders2 = Optimizations.getNonRedundantDerivatives c (HashSet()) der2

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
let hasLimitedSkip (initial: RegexNode<_>) (node: RegexNode<_>) =
    let redundant = HashSet([ initial ])
    let skipTerm = m.GetOrCreateState(initial).Startset

    match node with
    | Or(nodes, info) ->
        let nonInitial = nodes |> Seq.where (fun v -> not (refEq v initial)) |> c.Builder.mkOr
        let nonInitialDerivs = Optimizations.getNonRedundantDerivatives c redundant nonInitial

        let nonInitialNonTerm =
            nonInitialDerivs |> Array.where (fun (mt, node) -> not (Solver.contains skipTerm mt))

        let nonTermDerivatives(node: RegexNode<TSet>) =
            let ders1 = Optimizations.getNonRedundantDerivatives c redundant node
            ders1 |> Array.where (fun (mt, node) -> not (Solver.contains skipTerm mt))

        match nonInitialNonTerm with
        | [| singlePath |] ->
            let path = ResizeArray()

            let rec loop(node: RegexNode<_>) =
                match nonTermDerivatives node with
                | [| (mt, single) |] when
                    (not (node.CanBeNullable || refEq c.False node || c.Solver.IsFull(mt)))
                    ->
                    redundant.Add(node) |> ignore
                    path.Add(mt)
                    loop single
                | _ -> node

            let finalNode = loop (snd singlePath)

            Some(
                {|
                    TermPred = skipTerm
                    TermTransition = Algorithm.createStartsetDerivative (c, skipTerm, node)
                    SkipDistanceSet = path
                    NonTermTransition = (c.Builder.mkOr [ finalNode; initial ])
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
