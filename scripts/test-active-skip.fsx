#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open System
open Sbre
open Sbre.Types
open Sbre.Pat

fsi.PrintWidth <- 2000
fsi.AddPrinter<RegexNode<uint64>>(fun v -> v.ToString() )

let mkder (m:RegexMatcher<_>) =
    (fun (mt, node) ->
        let loc = Location.getNonInitial ()
        m.CreateDerivative(&loc, mt, node)
    )

let printTransitions (regex:Sbre.Regex) node = 

    let pretty derivs =
        derivs
        |> (Array.map (fun (mt, node) -> $"{regex.TSetMatcher.Cache.PrettyPrintMinterm(mt), -13} ==> {node.ToString()}"))
        |> join "\n"
        |> (fun v -> "\n" + v)

    let possibleTransitions =
        node |> Optimizations.getImmediateDerivativesMerged (mkder regex.TSetMatcher) regex.TSetMatcher.Cache |> Seq.toArray

    pretty possibleTransitions


let r = Sbre.Regex(@"\s[a-zA-Z]{0,12}ing\s")
let m = r.TSetMatcher
let c = m.Cache

printTransitions r r.TSetMatcher.RawPattern
let initial = r.TSetMatcher.RawPattern // "\s([A-Za-z]){0,12}ing\s"
let der1 = mkder r.TSetMatcher (c.CharToMinterm(' '), initial) 
string der1 // // "([A-Za-z]){0,12}ing\s"

// i -> ([A-Za-z]){0,11}i)?ng\s"
// [A-Za-hj-z] -> ([A-Za-z]){0,11}ing\s
let der2 = mkder r.TSetMatcher (c.CharToMinterm('i'), der1) 
string der2 // (([A-Za-z]){0,11}i)?ng\s
der2.CanBeNullable // can not be successful match yet

let der3 = mkder r.TSetMatcher (c.CharToMinterm('n'), der2) 
let der4 = mkder r.TSetMatcher (c.CharToMinterm('g'), der3) 
string der2 // (([A-Za-z]){0,11}i)?ng\s
der2.CanBeNullable // can not be successful match yet

// ' ' i n g ' ' -> 

der4.CanBeNullable
let der5 = mkder r.TSetMatcher (c.CharToMinterm(' '), der4) 
// val der5: RegexNode<uint64> = ε
der5.IsAlwaysNullable // val it: bool = true , successful match

