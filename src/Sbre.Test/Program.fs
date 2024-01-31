﻿// module Program = let [<EntryPoint>] main _ = 0

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

let suite = Paper.SampleRegexes()
suite.Setup()

for i = 1 to 10 do
    suite.Sbre() |> ignore


