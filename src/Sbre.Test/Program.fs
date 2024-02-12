// module Program = let [<EntryPoint>] main _ = 0

module Program

open Sbre
open Sbre.Benchmarks
open Sbre.Test

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


// let pref = Optimizations.Prefix2()
// pref.FirstSetIndexOfChars()

// let pref = Learning.Learning1()
// pref.Pattern <- pref.Patterns |> Seq.head
// pref.Setup()
// for i = 1 to 50 do
//     pref.Sbre() |> ignore
//




let pref = Learning.Learning3()
pref.Pattern <- pref.Patterns |> Seq.head
pref.Setup()
for i = 1 to 500 do
    pref.Sbre() |> ignore








