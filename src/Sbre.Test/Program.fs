﻿// module Program = let [<EntryPoint>] main _ = 0

module Program

open System
open System.Globalization
open Sbre
open Sbre.Benchmarks
open Sbre.Test


// let suite = Paper.SampleRegexes()
// for pat in suite.Patterns do
//     suite.Pattern <- pat
//     suite.Setup()
//     for i = 1 to 1000 do
//         suite.Sbre() |> ignore


// let pref = Optimizations.Prefix2()
// pref.FirstSetIndexOfChars()

let pref = Learning.Learning4()
pref.Pattern <- pref.Patterns |> Seq.head
pref.Setup()
for i = 1 to 50 do
    pref.Sbre() |> ignore
// //

//

// let b = 1
// let pref = Learning.Learning3()
// pref.Pattern <- pref.Patterns |> Seq.head
// pref.Setup()
// for i = 1 to 500 do
//     pref.Sbre() |> ignore
//
// let bibtexEntry =
//     @"@article{de2000thyroid,
//   title={Thyroid cancer in French Polynesia between 1985 and 1995: influence of atmospheric nuclear bomb tests performed at Mururoa and Fangataufa between 1966 and 1974},
//   author={De Vathaire, Florent and Le Vu, B{\'e}atrice and Challeton-de Vathaire, C{\'e}cile},
//   journal={Cancer Causes \& Control},
//   volume={11},
//   number={1},
//   pages={59--63},
//   year={2000},
//   publisher={Springer}
// }"
//
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
// let pref = _06_MatchTests.``lookback 3``()
// let pref = _06_MatchTests.``lookback 1``()
let ab = 1
// "(?=(.*):⊤*&):⊤*))"
// "((⊤*(6|8(.*))?&(⊤*(0(|6|[48]|8())?)"
// "(.*φ&.*&~(.*):.*)&φ.*)(?=():⊤*&.*):⊤*))"
// "(.*φ&.*&~(.*):.*)&φ.*)(?=():⊤*&.*):⊤*))"