// module Program = let [<EntryPoint>] main _ = 0

module Program

open System
open System.Globalization
open Sbre
open Sbre.Benchmarks
open Sbre.Test
open System



// let regex = Regex(@"(abc)+")
// let a = Regex(@"abc(abc)*")
// let ab = Regex(@"abcabc")
// let regex = Regex(@"(?<=_.*)(abc)+(?=.*_)")
// let pat = Regex(@"ab")
//
// let matcher = regex.TSetMatcher
// // let formalLlmatch = matcher.llmatch("__abc_abc_")
// let formalLlmatch = matcher.llmatch("__abcabc_abc_")
//

// let suite = Paper.Twain_1()
// suite.Setup()


// let suite = Paper.SampleRegexes()
// for pat in suite.Patterns do
//     suite.Pattern <- pat
//     suite.Setup()
//     for i = 1 to 1000 do
//         suite.Sbre() |> ignore


// let data =
//     "/home/ian/f/ttu/sbre-optimization/src/Sbre.Benchmarks/data/sherlock.txt"
//     |> System.IO.File.ReadAllText
//
// let r = Regex(@"\w+nn\W")


// let count = r.Count(data)

// let mts = r.TSetMatcher.Cache.Minterms()
// let searchvalues =
//     mts
//     |> Array.map r.TSetMatcher.Cache.MintermSearchValues

let dbg1 =1

// let pref = Optimizations.Prefix2()
// pref.FirstSetIndexOfChars()

let pref = Optimizations.PrefixCharsetSearch()

let switch = true

if switch then
     let r = pref.Original()
     // let ra = pref.Weighted()
     // let raa = pref.Weighted2()
     // let raaa = pref.Weighted3()
     let raaaa = pref.Weighted4()
     ()
else
     for i = 1 to 2000000 do
          // let r = pref.Weighted()
          let r2 = pref.Weighted2()
          // let r3 = pref.Weighted3()
          ()

let dbg = 1

