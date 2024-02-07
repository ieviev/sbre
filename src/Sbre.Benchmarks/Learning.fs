module Sbre.Benchmarks.Learning

open BenchmarkDotNet.Attributes
open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Optimizations
open System
open Sbre.Pat
open Sbre.Types
open FSharp.Data

// let [<Literal>] LearningSampleCongressBills = __SOURCE_DIRECTORY__ + @"/data/CongressBills-Date-short.json"
let [<Literal>] LearningSampleCongressBills = @"/home/ian/f/ieviev/sbre/src/Sbre.Benchmarks/data/CongressBills-Date-short.json"
type Provider = FSharp.Data.JsonProvider<LearningSampleCongressBills>
let ctx = Provider.GetSample()




[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Learning1() =
    inherit Jobs.TestSbreAllPatternsMatchOnly(
        [
            @"(?<=\s.*).*(?=.*(\n|\.|,|\s))&(?<=\s|(\s|\n)).*&.*(?=(\n|\.|,|\s)|.)&\b.*\b"
        ],
        ctx.Examples[10].String
    )


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Learning2() =
    inherit Jobs.TestSbreAllPatternsMatchOnly(
        [
            // @"(?<=(\s\s|e\s|n\s).*).*(?=.*(\n\n|\.\n|,\s|\s\())&(?<=(\s\s|e\s|n\s)|[a-z,\)]?\s(\s\n|\s\s)?).*&.*(?=(\n\n|\.\n|,\s|\s\()|.{2})&\b.*\b"
            @"(?<=(\s\s|e\s|n\s).*).*(?=.*(\n\n|\.\n|,\s|\s\())&(?<=(\s\s|e\s|n\s)|[a-z,\)]?\s(\s\n|\s\s)?).*&.*(?=(\n\n|\.\n|,\s|\s\()|.{2})&\b.*\b&.{8}"
        ],
        ctx.Examples[10].String
    )


// [<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
// [<ShortRunJob>]
// type Prefix2() =
//     // [HF][ui][cn][kn]
//     inherit SetsPrefix("Huck|Finn")
