module Sbre.Benchmarks.Jobs

open System
open BenchmarkDotNet.Attributes
open Sbre

[<MemoryDiagnoser(false)>]
[<ShortRunJob()>]
[<AbstractClass>]
[<HideColumns([|""|])>]
type OnlyC_None(patterns: string list, input:string) =
    let inputText = input
        // __SOURCE_DIRECTORY__ + "/data/input-text.txt"
        // |> System.IO.File.ReadAllText

    member val EngineNonBack : System.Text.RegularExpressions.Regex = null with get,set
    member val EngineNone : System.Text.RegularExpressions.Regex = null with get,set
    member val EngineCompiled : System.Text.RegularExpressions.Regex = null with get,set
    member this.Patterns : System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern : string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.EngineNone <- System.Text.RegularExpressions.Regex(
            this.Pattern,
            options=System.Text.RegularExpressions.RegexOptions.None,
            matchTimeout=TimeSpan.FromMilliseconds(10_000.))

    [<Benchmark>]
    member x.C_None() = x.EngineNone.Match(inputText)


[<MemoryDiagnoser(false)>]
[<ShortRunJob()>]
[<AbstractClass>]
[<HideColumns([|""|])>]
type OnlyC_NonBacktracking(patterns: string list) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText =
        __SOURCE_DIRECTORY__ + "/data/input-text.txt"
        |> System.IO.File.ReadAllText

    member val EngineNonBack : System.Text.RegularExpressions.Regex = null with get,set
    member val EngineNone : System.Text.RegularExpressions.Regex = null with get,set
    member val EngineCompiled : System.Text.RegularExpressions.Regex = null with get,set
    member this.Patterns : System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern : string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.EngineNonBack <- System.Text.RegularExpressions.Regex(
            this.Pattern,
            options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
            matchTimeout=TimeSpan.FromMilliseconds(10_000.))

    [<Benchmark>]
    member x.C_NonBacktracking() = x.EngineNonBack.Match(inputText)


[<MemoryDiagnoser(false)>]
[<ShortRunJob()>]
[<AbstractClass>]
// [<HideColumns([|""|])>]
type OnlySbre(patterns: string list, input:string) =
    let inputText = input

    member val EngineSbre : Sbre.Matcher = Unchecked.defaultof<_> with get,set
    member this.Patterns : System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern : string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.EngineSbre <- Matcher(this.Pattern)

    [<Benchmark>]
    member x.Sbre() = x.EngineSbre.Match(inputText)


[<MemoryDiagnoser(false)>]
[<ShortRunJob()>]
type TestLineSplit() =
    let inputText =
        __SOURCE_DIRECTORY__ + "/data/input-text.txt"
        |> System.IO.File.ReadAllText


    [<Benchmark>]
    member x.String_Split() = inputText.Split("\n\n")

    [<Benchmark>]
    member x.String_IndexOf() = inputText.Split("\n\n")



[<MemoryDiagnoser>]
[<ShortRunJob>]
[<AbstractClass>]
type TestAllRuntime(patterns: string list, input:string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input

    member val EngineNonBack : System.Text.RegularExpressions.Regex = null with get,set
    member val EngineNone : System.Text.RegularExpressions.Regex = null with get,set
    member val EngineCompiled : System.Text.RegularExpressions.Regex = null with get,set
    member this.Patterns : System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern : string = "" with get, set


    [<GlobalSetup>]
    member this.Setup() =
        this.EngineNone <- System.Text.RegularExpressions.Regex(
            this.Pattern,
            options=System.Text.RegularExpressions.RegexOptions.None,
            matchTimeout=TimeSpan.FromMilliseconds(10_000.))
        this.EngineCompiled <- System.Text.RegularExpressions.Regex(
            this.Pattern,
            options=System.Text.RegularExpressions.RegexOptions.Compiled,
            matchTimeout=TimeSpan.FromMilliseconds(10_000.))
        this.EngineNonBack <- System.Text.RegularExpressions.Regex(
            this.Pattern,
            options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
            matchTimeout=TimeSpan.FromMilliseconds(10_000.))

    [<Benchmark>]
    member x.C_None() = x.EngineNone.Match(inputText)