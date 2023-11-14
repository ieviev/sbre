﻿// For more information see https://aka.ms/fsharp-console-apps

open System
open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Loggers
open BenchmarkDotNet.Running
open Sbre
open Sbre.Benchmarks
open Sbre.Benchmarks.Jobs
open Sbre.Algorithm

module private Helpers =
    // let sample_mariomka = __SOURCE_DIRECTORY__ + "/data/mariomka-benchmark.txt" |> File.ReadAllText
    // let sample_mtwain = __SOURCE_DIRECTORY__ + "/data/mtent12.txt" |> File.ReadAllText
    let sample_inputText = __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> File.ReadAllText
    let pattern_mariomkauri = """[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?"""
    let pattern_mariomkaemail = """[\w\.+-]+@[\w\.-]+\.[\w\.-]+"""
    let pattern_mtwain1 = """Twain"""
    let pattern_mtwain2 = """.{2,4}(Tom|Sawyer|Huckleberry|Finn)"""
    // ------
    let pattern = pattern_mtwain1


open Helpers

let config =
    DefaultConfig.Instance
        .WithSummaryStyle(DefaultConfig.Instance.SummaryStyle.WithMaxParameterColumnWidth(60))

let dbgSample() =
    let reg = System.Text.RegularExpressions.Regex("Twain")
    for i = 0 to 1000 do
        reg.Matches(sample_inputText).Count |> ignore



let dbgSbre() =

    let t = Paper.DebugSbre3()
    t.Pattern <-
        t.Patterns
        |> Seq.head
    t.Setup()
    // for i = 0 to 1 do
    // for i = 0 to 1000 do
    for i = 0 to 400 do
        t.MatchWithConj() |> ignore





[<EntryPoint>]
let main argv =

#if DEBUG
    dbgSbre()
#endif
    // dbgSbre()
    // dbgSample()


    match Environment.GetCommandLineArgs() |> Seq.last with
    | "outer-none" -> BenchmarkRunner.Run(typeof<ParagraphOuter.None1>,config) |> ignore
    | "outer-nonb" -> BenchmarkRunner.Run(typeof<ParagraphOuter.NonBack1>,config) |> ignore
    | "outer-sbre" -> BenchmarkRunner.Run(typeof<ParagraphOuter.Sbre1>,config) |> ignore
    // ----
    | "inner-none-2" -> BenchmarkRunner.Run(typeof<ParagraphInner.None_2Words>,config) |> ignore
    | "inner-none" -> BenchmarkRunner.Run(typeof<ParagraphInner.None_All>,config) |> ignore
    | "inner-nonb-3" -> BenchmarkRunner.Run(typeof<ParagraphInner.NonBacktracking_3>,config) |> ignore
    | "inner-sbre-2" -> BenchmarkRunner.Run(typeof<ParagraphInner.Sbre_2>,config) |> ignore
    | "inner-sbre" -> BenchmarkRunner.Run(typeof<ParagraphInner.Sbre_All>,config) |> ignore

    // --

    | "full-nonb-3" -> BenchmarkRunner.Run(typeof<ParagraphFull.NonBacktracking_3>,config) |> ignore
    | "full-none-3" -> BenchmarkRunner.Run(typeof<ParagraphFull.None_3>,config) |> ignore
    | "full-comp-4" -> BenchmarkRunner.Run(typeof<ParagraphFull.Compiled_4>,config) |> ignore
    | "full-sbre-3" -> BenchmarkRunner.Run(typeof<ParagraphFull.Sbre_Combined_3>,config) |> ignore
    | "full-1" -> BenchmarkRunner.Run(typeof<ParagraphFull.DebugSbre>,config) |> ignore
    // --
    | "debug-sbre" -> BenchmarkRunner.Run(typeof<ParagraphFull.DebugSbre>,config) |> ignore
    | "debug-sbre2" -> BenchmarkRunner.Run(typeof<ParagraphFull.DebugSbre2>,config) |> ignore
    | "debug-runtime" -> BenchmarkRunner.Run(typeof<ParagraphFull.DebugRuntime>,config) |> ignore
    | "debug-all" -> BenchmarkRunner.Run(typeof<ParagraphFull.DebugAll>,config) |> ignore
    // --

    // ----
    | "paper-short-1" -> BenchmarkRunner.Run(typeof<Paper.ParagraphShort1Word>,config) |> ignore
    | "paper-conj-1" -> BenchmarkRunner.Run(typeof<Paper.ParagraphConjunction1>,config) |> ignore
    | "paper-complex-1" -> BenchmarkRunner.Run(typeof<Paper.ParagraphComplexRegex1>,config) |> ignore
    | "paper-inner-1" -> BenchmarkRunner.Run(typeof<Paper.ParagraphInnerMatch1>,config) |> ignore
    | "paper-basic-1" -> BenchmarkRunner.Run(typeof<Paper.Basic1>,config) |> ignore
    | "paper-basic-2" -> BenchmarkRunner.Run(typeof<Paper.Basic2>,config) |> ignore
    | "paper-basic-3" -> BenchmarkRunner.Run(typeof<Paper.Basic3>,config) |> ignore
    // ---
    | "all-w1" -> BenchmarkRunner.Run(typeof<Paper.WordsLine1>,config) |> ignore
    | "all-w2" -> BenchmarkRunner.Run(typeof<Paper.WordsLine2>,config) |> ignore
    | "all-w3" -> BenchmarkRunner.Run(typeof<Paper.WordsLine3>,config) |> ignore
    | "all-w4" -> BenchmarkRunner.Run(typeof<Paper.WordsLine4>,config) |> ignore
    // | "all-w5" -> BenchmarkRunner.Run(typeof<Paper.WordsLine5>,config) |> ignore
    // | "all-l1" -> BenchmarkRunner.Run(typeof<Paper.LWord1>,config) |> ignore
    | "all-r1" -> BenchmarkRunner.Run(typeof<Paper.PatternLine1>,config) |> ignore
    // | "all-r3" -> BenchmarkRunner.Run(typeof<Paper.RegexLine3>,config) |> ignore
    | "dbg" -> BenchmarkRunner.Run(typeof<Paper.DebugSbre3>,config) |> ignore
    | "dbg-mt" -> BenchmarkRunner.Run(typeof<Jobs.Minterms>,config) |> ignore
    // ------------

    // standard benchmarks
    | "twain-1" -> BenchmarkRunner.Run(typeof<Paper.Twain_1>,config) |> ignore
    | "twain" -> BenchmarkRunner.Run(typeof<Paper.TwainRegexes>,config) |> ignore


    // words in paragraph
    | "paper-pg-1" -> BenchmarkRunner.Run(typeof<Paper.ParagraphLong1Word>,config) |> ignore
    | "paper-pg-2" -> BenchmarkRunner.Run(typeof<Paper.ParagraphLong2Word>,config) |> ignore
    | "paper-pg-3" -> BenchmarkRunner.Run(typeof<Paper.ParagraphLong3Word>,config) |> ignore
    | "paper-pg-4" -> BenchmarkRunner.Run(typeof<Paper.ParagraphLong4Word>,config) |> ignore
    | "paper-pg-all" -> BenchmarkRunner.Run(typeof<Paper.ParagraphRegexes>,config) |> ignore

    // words in line (no change in first (?))
    | "paper-line-1" -> BenchmarkRunner.Run(typeof<Paper.WordsLine1>,config) |> ignore
    | "paper-line-2" -> BenchmarkRunner.Run(typeof<Paper.WordsLine2>,config) |> ignore
    | "paper-line-3" -> BenchmarkRunner.Run(typeof<Paper.WordsLine3>,config) |> ignore
    | "paper-line-4" -> BenchmarkRunner.Run(typeof<Paper.WordsLine4>,config) |> ignore
    | "paper-line-5" -> BenchmarkRunner.Run(typeof<Paper.WordsLine5>,config) |> ignore
    | "paper-line-6" -> BenchmarkRunner.Run(typeof<Paper.WordsLine6>,config) |> ignore
    | "paper-line-7" -> BenchmarkRunner.Run(typeof<Paper.WordsLine7>,config) |> ignore
    | "paper-line-all" -> BenchmarkRunner.Run(typeof<Paper.LineRegexes>,config) |> ignore

    // patterns in line (no change in first (?))
    | "paper-pat-1" -> BenchmarkRunner.Run(typeof<Paper.PatternLine1>,config) |> ignore
    // | "paper-line-2" -> BenchmarkRunner.Run(typeof<Paper.WordsLine2>,config) |> ignore
    // | "paper-line-3" -> BenchmarkRunner.Run(typeof<Paper.WordsLine3>,config) |> ignore
    // | "paper-line-4" -> BenchmarkRunner.Run(typeof<Paper.WordsLine4>,config) |> ignore
    // | "paper-line-5" -> BenchmarkRunner.Run(typeof<Paper.WordsLine5>,config) |> ignore
    // | "paper-line-6" -> BenchmarkRunner.Run(typeof<Paper.WordsLine6>,config) |> ignore

    // lines
    | "paper-lines-1" -> BenchmarkRunner.Run(typeof<Paper.Lines1>,config) |> ignore
    | "paper-lines-2" -> BenchmarkRunner.Run(typeof<Paper.Lines2>,config) |> ignore




    | _ ->
        ()
        // failwith "todo: invalid benchmark"

    // let r = BenchmarkRunner.Run(typeof<PasswordMatching_1>)
    // twain
    // let results = BenchmarkRunner.Run(typeof<Twain>)

    0