module Sbre.Benchmarks.ByteMode

open System.IO.MemoryMappedFiles
open System.Xml
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
            // @"Шерлок Холмс|Джон Уотсон|Ирен Адлер|инспектор Лестрейд|профессор Мориарти"


            "/mnt/ice/repos/rebar/benchmarks/regexes/dictionary/english/length-15.txt"
            |> System.IO.File.ReadAllText
            |> (fun v -> v.Trim().Split("\n"))
            |> String.concat "|"

        ],
        // __SOURCE_DIRECTORY__ + "/data/en-sampled.txt",
        // @"/mnt/ice/repos/rebar/benchmarks/haystacks/opensubtitles/ru-sampled.txt",
        @"/mnt/ice/repos/rebar/benchmarks/haystacks/opensubtitles/en-medium.txt",
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

[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<HideColumns([| "" |])>]
type Mmap1() =
    // let filePath = "/mnt/sdc4/data/etwiki/etwiki-20240320-pages-articles-multistream.xml"
    let filePath = "/mnt/sdc4/data/enwiki/enwiki-20240320-stub-meta-current.xml"
    // let bytes = System.IO.File.ReadAllBytes(filePath)
    let options = SbreOptions.HighThroughputAscii
    do options.StreamBufferSize <- pown 2 16
    let mmap =
        filePath
        |> System.IO.MemoryMappedFiles.MemoryMappedFile.CreateFromFile

    member val Pattern = @"(?<=<title>).*Estonia.*(?=</title>)"

    // member val CompiledEngine: Sbre.RegexMatcher<TSet> = Unchecked.defaultof<_> with get, set
    member val Sbre: Sbre.Regex = Unchecked.defaultof<_> with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let regex = Regex(this.Pattern,options)
        this.Sbre <- regex
        ()

    // [<Benchmark(Description = "SbreInMemory")>]
    // member this.SbreInMemory() =
    //     let mutable count = 0
    //     use matches = this.Sbre.MatchPositions(bytes)
    //     for m in matches do
    //         count <- count + 1



    [<Benchmark(Description = "SbreStream-38GB")>]
    member this.SbreStream() =
        let mutable count = 0
        // let mmap = filePath |> MemoryMappedFile.CreateFromFile
        use viewStream = mmap.CreateViewStream()
        use matches =
            // Sbre.Regex(@"(?<=<title>).*Estonia.*(?=</title>)",options)
            this.Sbre
                .MatchPositions(viewStream)
        for m in matches do
            count <- count + 1

    //
    // [<Benchmark(Description = "XmlReader")>]
    // member this.XmlReader() =
    //     let mutable count = 0
    //     use sr = XmlReader.Create(filePath)
    //     while sr.Read() do
    //         match sr.NodeType, sr.Name with
    //         | XmlNodeType.Element, "title" ->
    //             let title = sr.ReadElementContentAsString()
    //             if title.Contains("Estonia") then
    //                 count <- count + 1
    //         | _ -> ()
