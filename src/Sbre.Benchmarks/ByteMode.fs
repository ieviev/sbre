module Sbre.Benchmarks.ByteMode

open BenchmarkDotNet.Attributes
open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Optimizations
open System
open Sbre.Pat
open Sbre.Types
open FSharp.Data

let sherlockText = "/mnt/ice/repos/rebar/benchmarks/haystacks/opensubtitles/en-sampled.txt"

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Bytes1() =
    inherit Jobs.TestSbreByte(
        [
            // "Sherlock Holmes"
            // """(?i)Sherlock Holmes"""
            // "Шерлок Холмс"
            // "Шерлок Холмс"
            // "Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"
            // "(?i)Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"
        ],
        __SOURCE_DIRECTORY__ + "/data/en-sampled.txt",
        SbreOptions.HighThroughputAscii
    )

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Bytes2() =
    inherit Jobs.TestSbreLarge(
        [
            @"(?<=<title>).*(?=</title>)"
        ],
        "/mnt/sdc4/data/enwiki/enwiki-20240320-pages-meta-current1.xml-p1p41242",
        SbreOptions.HighThroughputAscii
    )
