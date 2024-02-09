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
let [<Literal>] LearningSampleCongressBills = __SOURCE_DIRECTORY__ + @"/data/CongressBills-Date-short.json"
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

let bibtexEntry =
    @"@article{de2000thyroid,
  title={Thyroid cancer in French Polynesia between 1985 and 1995: influence of atmospheric nuclear bomb tests performed at Mururoa and Fangataufa between 1966 and 1974},
  author={De Vathaire, Florent and Le Vu, B{\'e}atrice and Challeton-de Vathaire, C{\'e}cile},
  journal={Cancer Causes \& Control},
  volume={11},
  number={1},
  pages={59--63},
  year={2000},
  publisher={Springer}
}"

let allBibtexEntries =
    System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "/data/bibtexAuthors.txt")

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Learning3() =
    inherit Jobs.TestSbreAllPatternsMatchOnly(
        [
            // @"(?<=(\s\s|e\s|n\s).*).*(?=.*(\n\n|\.\n|,\s|\s\())&(?<=(\s\s|e\s|n\s)|[a-z,\)]?\s(\s\n|\s\s)?).*&.*(?=(\n\n|\.\n|,\s|\s\()|.{2})&\b.*\b"
            """(?<=or=\{.*)\b(~(.*and.*)&\S[\w-{}\\' ,]+\S)\b(?=.*\},)"""
        ],
        bibtexEntry
    )

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Learning4() =
    inherit Jobs.TestSbreAllPatternsMatchOnly(
        [
            // @"(?<=(\s\s|e\s|n\s).*).*(?=.*(\n\n|\.\n|,\s|\s\())&(?<=(\s\s|e\s|n\s)|[a-z,\)]?\s(\s\n|\s\s)?).*&.*(?=(\n\n|\.\n|,\s|\s\()|.{2})&\b.*\b"
            @"(?<=or=(\{|.*\W))(~(.*and.*)&\S[\w-{}\\' ,]+\w)(?=(\W.*|)\},)"
        ],
        allBibtexEntries
    )


