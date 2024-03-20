// module Program = let [<EntryPoint>] main _ = 0

module Program

open System
open System.Globalization
open Sbre
open Sbre.Benchmarks
open Sbre.Parser
open Sbre.Test

//
// let suite = Paper.SampleRegexes()
// for pat in suite.Patterns do
//     suite.Pattern <- pat
//     suite.Setup()
//     for i = 1 to 1000 do
//         suite.Sbre() |> ignore


// let suite = Paper.Rebar10()
// for pat in suite.Patterns do
//     suite.Pattern <- pat
//     suite.Setup()
//     for i = 1 to 1000 do
//         suite.Sbre() |> ignore

let suite = Paper.Rebar3()
for pat in suite.Patterns do
    suite.Pattern <- pat
    suite.Setup()
    for i = 1 to 1000 do
        suite.Sbre() |> ignore



// let pref = Optimizations.Prefix2()
// pref.FirstSetIndexOfChars()

// let pref = Learning.Learning4()
// pref.Pattern <- pref.Patterns |> Seq.head
// pref.Setup()
// for i = 1 to 50 do
//     pref.Sbre() |> ignore
// //


//
// let r = Sbre.Regex(@"(?<=or=\{.*)(?<=\W)(~(.*and.*)&[A-Z][\w-{}\\' ,]+)(?=.*\},)(?=\W)")
//
// let ms = r.Matches(bibtexEntry) |> Seq.toArray
//
// assert (ms.Length = 3)
// let asd = 1
// let b = 1
// let pref = Paper.Rebar14()
// let pref = Paper.Rebar10()
// pref.Pattern <- pref.Patterns |> Seq.head
// pref.Setup()
// // for i = 1 to 500 do
// for i = 1 to 3000 do
//     pref.Sbre() |> ignore

// let pref = _06_MatchTests.``lookback 2``()
// let pref = _02_NodeTests.``very large pat 01``()
// let pref = BenchmarkTests.``learning sample 1``()
// let pref = BenchmarkTests.leipzig_count()
// let pref = _06_MatchTests.``lookback 3``()
// let pref = _06_MatchTests.``lookback 1``()
let ab = 1
// "(?=(.*):⊤*&):⊤*))"
// "((⊤*(6|8(.*))?&(⊤*(0(|6|[48]|8())?)"
// "(.*φ&.*&~(.*):.*)&φ.*)(?=():⊤*&.*):⊤*))"
// "(.*φ&.*&~(.*):.*)&φ.*)(?=():⊤*&.*):⊤*))"


//
// open FParsec
// // let parse1 = CharParsers.run Parser.Syntax.r_regex "ab(cd|ef)"
//
// // let pattern = """~(\T*(\n\T*){2})&\T*g\T*&~(\T*")&[A-Za-z]{5}\T*"""
// // let pattern = """[A-Za-z]{5}\T*"""
// let pattern = """static &.*(?=.*\s[A-Z]\w+\()&(?<!//.*).*&.*(?=~(\T*(\n\T*){6})\})"""
// let parse1 = CharParsers.run Parser.Syntax.r_regex pattern
// // let parse1 = CharParsers.run Parser.Syntax.r_regex "ab→cd"
// // let parse1 = CharParsers.run Parser.Syntax.r_regex "^.*$"
//
// stdout.WriteLine pattern
// match parse1 with
// | ParserResult.Success(res,_,position) ->
//     stdout.WriteLine $"%A{res}"
//     stdout.WriteLine (Pattern.toString res)
// | _ ->
//     stdout.WriteLine (string parse1)



    // let pattern = ".*A.*→.*B.*"





