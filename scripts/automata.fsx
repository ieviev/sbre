#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open System
open Sbre
open Sbre.Types
open Sbre.Pat
open System.Text.RuntimeRegexCopy.Symbolic

fsi.PrintWidth <- 2000
fsi.AddPrinter<RegexNode<uint64>>(fun v -> v.ToString() )
fsi.AddPrinter<BDD>(fun v -> v.ToString() )

let mkder (m:RegexMatcher<_>) =
    (fun (mt, node) ->
        let loc = Location.getNonInitial ()
        m.CreateDerivative(&loc, mt, node)
    )

let printTransitions (regex:Sbre.Regex) node = 

    let pretty derivs =
        derivs
        |> (Array.map (fun (mt, node) -> $"{regex.TSetMatcher.Cache.PrettyPrintMinterm(mt), -13} ==> {node.ToString()}"))
        |> String.concat "\n"
        |> (fun v -> "\n" + v)

    let possibleTransitions =
        node |> Optimizations.getImmediateDerivativesMerged (mkder regex.TSetMatcher) regex.TSetMatcher.Cache |> Seq.toArray

    pretty possibleTransitions


let getMovesFrom (regexEngine:Sbre.Regex) (node:RegexNode<_>) = 
    let possibleTransitions =
        node |> Optimizations.getImmediateDerivativesMerged (mkder regexEngine.TSetMatcher) regexEngine.TSetMatcher.Cache |> Seq.toArray
    let charsetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()
    let charClassToBdd (charClass) = 
        regexEngine.TSetMatcher.Cache.Solver.ConvertToBDD(charClass,charsetSolver)
    possibleTransitions
    |> Seq.map (fun (charClass,transition) ->
        // print char class to string
        let charString = regexEngine.TSetMatcher.Cache.PrettyPrintMinterm(charClass)
        charString,charClassToBdd(charClass),transition
    )
    |> Seq.toArray


// let r = Sbre.Regex(@"\s[a-zA-Z]{0,12}ing\s")
let r = Sbre.Regex(
    String.concat "&" [
        ".*[ghijk]ere.*"
    ] )

let initialState = r.TSetMatcher.RawPattern
let movesFromInitial = getMovesFrom r initialState
let movesFromInitial2 = printTransitions r initialState
let m = r.TSetMatcher
let c = m.Cache
let (label,bdd,nextState) = movesFromInitial[2]
let ranges = BDDRangeConverter.ToRanges(bdd) 
let struct(start,endRange) = ranges[0] // (103u, 107u)
let someState = r.TSetMatcher.RawPattern
// automaton.IsFinalState(currentStateId) == someState.IsAlwaysNullable
// BDDRangeConverter.ToRanges(bdd) == move.Label.ToRanges()

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

