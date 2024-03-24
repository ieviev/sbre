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
        ["Sherlock Holmes"],
        __SOURCE_DIRECTORY__ + "/data/en-sampled.txt",
        SbreOptions.HighThroughputDefaults
    )

