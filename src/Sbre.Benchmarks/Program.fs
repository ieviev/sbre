// For more information see https://aka.ms/fsharp-console-apps

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
open Sbre.Benchmarks.FullMtwain
open Sbre.Benchmarks.LongParagraph
open Sbre.Regex

module private Helpers =
    // let sample_mariomka = __SOURCE_DIRECTORY__ + "/data/mariomka-benchmark.txt" |> File.ReadAllText
    // let sample_mtwain = __SOURCE_DIRECTORY__ + "/data/mtent12.txt" |> File.ReadAllText
    let sample_inputText = __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> File.ReadAllText
    let pattern_mariomkauri = """[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?"""
    let pattern_mariomkaemail = """[\w\.+-]+@[\w\.-]+\.[\w\.-]+"""
    let pattern_mtwain1 = """Twain"""
    let pattern_mtwain2 = """.{2,4}(Tom|Sawyer|Huckleberry|Finn)"""

    // ------
    // let pattern = """["'][^"']{0,30}[?!\.]["']"""
    // let pattern = pattern_mtwain1
    // let pattern = "[a-z]shing"
    let pattern = pattern_mtwain1

    // let input = sample_mtwain

open Helpers

type Allocations() =

    let mutable matcher = Matcher(pattern)
    let input = "Twain"
    let mutable startLocation = Sbre.Pat.Location.create input 0
    let mutable location1 = Sbre.Pat.Location.create input 1
    let mutable location2 = Sbre.Pat.Location.create input 2
    let cache = matcher.Cache
    let dotstarNode = matcher.DotStarredPattern
    // let der1 = Regex.createDerivative(cache,startLocation,cache.MintermForLocation(startLocation) , dotstarNode)
    // let der2 = Regex.createDerivative(cache,location1,cache.MintermForLocation(location1), der1)

    [<Benchmark>]
    member x.FullMatchTwain() = matcher.FindMatchEnd(input) |> ignore

let config =
    DefaultConfig.Instance
        .WithSummaryStyle(DefaultConfig.Instance.SummaryStyle.WithMaxParameterColumnWidth(60))

let dbgSample() =

    let shortSample = Helpers.sample_inputText

    let t2 =
        // Matcher(permuteConj [ "compilation"; "smaller" ]).Match(shortSample)
        Matcher(permuteConj [ "compilation"; "smaller" ]).MatchPositions(shortSample)
        |> Seq.toArray
    ()



[<EntryPoint>]
let main argv =

    // dbgSample()
    // let t = LongParagraph9000_3()
    // t.SBRE() |> ignore



    // let t = FullMtwain_3()

    // let m =  Matcher(@"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*")

    let t = ParagraphFull.Sbre_Debug()
    t.Pattern <-
        t.Patterns
        |> Seq.head
    t.Setup()
    for i = 0 to 9 do
        t.MatchWithConj() |> ignore


    //
    // let ds =
    //     m.MatchPositions(Helpers.sample_inputText)
    //     |> Seq.toArray
    // let vasd = 1
    //
    // let v =
    //     for i = 0 to 10 do t.SBRE() |> ignore


    if Environment.GetCommandLineArgs() |> Seq.last = "test" then
        let t = FullMtwain_3()
        t.SBRE() |> ignore

    if Environment.GetCommandLineArgs() |> Seq.last = "pg" then
        // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_3>)
        let r = BenchmarkRunner.Run(typeof<LongParagraph9000_3>)
        ()

    if Environment.GetCommandLineArgs() |> Seq.last = "mtwain3" then
        let _ = BenchmarkRunner.Run(typeof<FullMtwain_3>,config)
        ()


    if Environment.GetCommandLineArgs() |> Seq.last = "full" then
        // let r = BenchmarkRunner.Run(typeof<ParagraphOuter.None1>,config)
        let r = BenchmarkRunner.Run(typeof<ParagraphOuter.Sbre1>,config)
        // let r = BenchmarkRunner.Run(typeof<FullMtwain_3>)
        // let r = BenchmarkRunner.Run(typeof<FullMtwain_5>)
        // let r = BenchmarkRunner.Run(typeof<FullMtwain_6>)
        ()

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
    | "full-1" -> BenchmarkRunner.Run(typeof<ParagraphFull.Sbre_Debug>,config) |> ignore
    | "debug-sbre" -> BenchmarkRunner.Run(typeof<ParagraphFull.Sbre_Debug>,config) |> ignore


    | _ ->
        ()
        // failwith "todo: invalid benchmark"

    // let r = BenchmarkRunner.Run(typeof<PasswordMatching_1>)

    // let r = BenchmarkRunner.Run(typeof<FullMtwain_1>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_2>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_3>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_4>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_5>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_6>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_7>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_8>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_9>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_10>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_11>)
    // let r = BenchmarkRunner.Run(typeof<FullMtwain_12>)


    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_1>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_2>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_3>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_4>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_5>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_6>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_7>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_8>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_9>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_10>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_11>)
    // let r = BenchmarkRunner.Run(typeof<LongParagraph9000_12>)


    // let conj_vs_alt_2 = BenchmarkRunner.Run(typeof<ConjVsAlt2>)
    // let conj_vs_alt_3 = BenchmarkRunner.Run(typeof<ConjVsAlt3>)
    // let conj_vs_alt_4 = BenchmarkRunner.Run(typeof<ConjVsAlt4>)
    // let conj_vs_alt_5 = BenchmarkRunner.Run(typeof<ConjVsAlt5>)
    // let r = BenchmarkRunner.Run(typeof<ConjVsAlt6>)
    // let r = BenchmarkRunner.Run(typeof<ConjVsAlt7>)

    // let paragraph2 = BenchmarkRunner.Run(typeof<ConjunctionParagraph2>)
    // let paragraph3 = BenchmarkRunner.Run(typeof<ConjunctionParagraph3>)
    // let paragraph4 = BenchmarkRunner.Run(typeof<ConjunctionParagraph4>)
    // let paragraph6 = BenchmarkRunner.Run(typeof<ConjunctionParagraph6>)


    // let conjunctionResults = BenchmarkRunner.Run(typeof<ConjunctionFull2>)
    // let conjunctionResults = BenchmarkRunner.Run(typeof<ConjunctionFull3>)
    // let conjunctionResults = BenchmarkRunner.Run(typeof<ConjunctionFull4>)
    // let conjunctionResults = BenchmarkRunner.Run(typeof<ConjunctionFull6>)
    // let conjunctionResults = BenchmarkRunner.Run(typeof<ConjunctionFull7>)

    // let debug = 1
    // let switcher = BenchmarkSwitcher.FromAssembly(typeof<Benches>.Assembly)
    // let config = BenchmarkDotNet.Configs.DefaultConfig.Instance

    // let quickConf =
    //     Job("Allocations")
    //         .WithWarmupCount(1)
    //         .WithIterationCount(1)
    //         .WithLaunchCount(1)
    //         // .WithStrategy(RunStrategy.Monitoring)
    // let fastRunConfig =
    //     ManualConfig()
    //         .AddDiagnoser(MemoryDiagnoser.Default)
    //         .AddColumnProvider(DefaultColumnProviders.Instance)
    //         .AddJob(quickConf)
    //         .AddLogger(ConsoleLogger())
    //         .WithOptions(ConfigOptions.DisableOptimizationsValidator)
    //
    // let results = BenchmarkRunner.Run<ConjunctionFull2>(fastRunConfig)
    // let results = BenchmarkRunner.Run<Allocations>(fastRunConfig)

    // let fastRunConfig =
    //     BenchmarkDotNet.Configs.ManualConfig()
    //         .AddDiagnoser(MemoryDiagnoser.Default)
    //         .AddColumnProvider(DefaultColumnProviders.Instance)
    //         .AddJob(quickConf)
    //         .AddLogger(ConsoleLogger())
    //         .WithOptions(ConfigOptions.DisableOptimizationsValidator)

    //config
    // let results = BenchmarkRunner.Run(typeof<Benches>)

    // twain
    // let results = BenchmarkRunner.Run(typeof<Twain>)

    0