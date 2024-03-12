[<Xunit.Collection("Sequential")>]
module Sbre.Test.BenchmarkTests

open System.IO
open Sbre
open Sbre.Benchmarks.Jobs
open Xunit
open Common
let twainPatterns = [
    @"Twain"
    @"(?i)Twain"
    @"[a-z]shing"
    @"Huck[a-zA-Z]+|Saw[a-zA-Z]+"
    // @"\b\w+nn\b"
    @"[a-q][^u-z]{13}x"
    @"Tom|Sawyer|Huckleberry|Finn"
    @"(?i)Tom|Sawyer|Huckleberry|Finn"
    @".{0,2}(Tom|Sawyer|Huckleberry|Finn)"
    @".{2,4}(Tom|Sawyer|Huckleberry|Finn)"
    @"Tom.{10,25}river|river.{10,25}Tom"
    @"[a-zA-Z]+ing"
    @"\s[a-zA-Z]{0,12}ing\s"
    @"([A-Za-z]awyer|[A-Za-z]inn)\s"
    @"[""'][^""']{0,30}[?!\.][""']"
]


let twain_input =
    File.ReadAllText(__SOURCE_DIRECTORY__ + "/data/input-text.txt")
    |> (fun v -> v[..500000])



let twain_20k = twain_input[..19999] // 10k chars limit






#if DEBUG

[<Fact>]
let twain_counts_0() =
    let idx = 0
    assertEqual
        (System.Text.RegularExpressions.Regex(twainPatterns[idx]).Count(twain_input))
        (Sbre.Regex(twainPatterns[idx]).Count(twain_input))

[<Fact>]
let twain_counts_1() =
    let idx = 1
    assertEqual
        (System.Text.RegularExpressions.Regex(twainPatterns[idx]).Count(twain_input))
        (Sbre.Regex(twainPatterns[idx]).Count(twain_input))

[<Fact>]
let twain_counts_2() =
    let idx = 2
    let r1 = (System.Text.RegularExpressions.Regex(twainPatterns[idx]).Matches(twain_input))
    let r2 = (Sbre.Regex(twainPatterns[idx]).Matches(twain_input)) |> Seq.toArray
    assertEqual r1.Count r2.Length



[<Fact>]
let twain_counts_3() =
    let idx = 3
    assertEqual
        (System.Text.RegularExpressions.Regex(twainPatterns[idx]).Count(twain_input))
        (Sbre.Regex(twainPatterns[idx]).Count(twain_input))

// blowup
// [<Fact>]
// let twain_counts_4() =
//     let idx = 4
//     let pat = twainPatterns[idx]
//     assertEqual
//         (System.Text.RegularExpressions.Regex(twainPatterns[idx]).Count(twain_input))
//         (Sbre.Regex(twainPatterns[idx]).Count(twain_input))

// [<Fact>]
// let twain_counts_5() =
//     let idx = 5
//     assertEqual
//         (System.Text.RegularExpressions.Regex(twainPatterns[idx]).Count(twain_input))
//         (Sbre.Regex(twainPatterns[idx]).Count(twain_input))

[<Fact>]
let twain_counts_6() =
    let idx = 6
    assertEqual
        (System.Text.RegularExpressions.Regex(twainPatterns[idx]).Count(twain_input))
        (Sbre.Regex(twainPatterns[idx]).Count(twain_input))

[<Fact>]
let twain_ranges_1() =
    let pat = @"[""'][^""']{0,30}[?!\.][""']"

    let matches1 =
        (System.Text.RegularExpressions.Regex(pat).Matches(twain_input) )
    let matches2 = (Sbre.Regex(pat).Matches(twain_input)) |> Seq.toArray

    assertEqual matches1.Count matches2.Length

    assertAllEqual
        (matches1|> Seq.map (fun v -> struct (v.Index,v.Length)))
        (matches2 |> Seq.map (fun v -> struct (v.Index,v.Length)))


[<Fact>]
let twain_ranges_2() =
    let pat = @"\s[a-zA-Z]{0,12}ing\s"

    let slice = twain_input[12400..13000]
    let matches1 =
        (System.Text.RegularExpressions.Regex(pat).Matches(slice) )
    let matches2 = (Sbre.Regex(pat).Matches(slice)) |> Seq.toArray

    assertEqual matches1.Count matches2.Length

    assertAllEqual
        (matches1|> Seq.map (fun v -> struct (v.Index,v.Length)))
        (matches2 |> Seq.map (fun v -> struct (v.Index,v.Length)))


[<Fact>]
let twain_ranges_3() =
    let pat = @"\s([A-Za-z]awyer|[A-Za-z]inn)\s"

    let matches1 =
        (System.Text.RegularExpressions.Regex(pat).Matches(twain_input) )
    let matches2 = (Sbre.Regex(pat).Matches(twain_input)) |> Seq.toArray

    assertEqual matches1.Count matches2.Length

    assertAllEqual
        (matches1|> Seq.map (fun v -> struct (v.Index,v.Length)))
        (matches2 |> Seq.map (fun v -> struct (v.Index,v.Length)))


[<Fact>]
let twain_ranges_4() =
    let pat = """["'][^"']{0,30}[?!\.]["']]"""

    let matches1 =
        (System.Text.RegularExpressions.Regex(pat).Matches(twain_input) )
    let matches2 = (Sbre.Regex(pat).Matches(twain_input)) |> Seq.toArray

    assertEqual matches1.Count matches2.Length

    assertAllEqual
        (matches1|> Seq.map (fun v -> struct (v.Index,v.Length)))
        (matches2 |> Seq.map (fun v -> struct (v.Index,v.Length)))

[<Fact>]
let twain_ranges_5() =
    let pat = """.{2,4}(Tom|Sawyer|Huckleberry|Finn)"""

    let matches1 =
        (System.Text.RegularExpressions.Regex(pat).Matches(twain_input) )
    let matches2 = (Sbre.Regex(pat).Matches(twain_input)) |> Seq.toArray

    assertEqual matches1.Count matches2.Length

    assertAllEqual
        (matches1|> Seq.map (fun v -> struct (v.Index,v.Length)))
        (matches2 |> Seq.map (fun v -> struct (v.Index,v.Length)))


//
[<Fact>]
let ``lines-have-there``() =
    let m = Regex(@".*have.*&.*there.*&.*")
    let r = m.Matches(twain_input[..120_000]) |> Seq.toArray
    // 411

    // Assert.Equal(426, r.Length) // full text
    Assert.Equal(4, r.Length)
    Assert.Equal(r[0].Index, 63592)
    Assert.Equal(r[1].Index, 109000)
    Assert.Equal(r[2].Index, 112959)

    Assert.Equal(r[0].Length, 73)
    Assert.Equal(r[1].Length, 40)
    Assert.Equal(r[2].Length, 50)

[<Fact>]
let ``line test 2``() =
    let m = Regex(@".* t[a-z]*e .*&.* a[a-z]*d .*")
    let r = m.Matches(twain_input[..15_000]) |> Seq.toArray
    // 411

    Assert.Equal(2, r.Length)
    Assert.Equal(r[0].Index, 113)
    Assert.Equal(r[0].Length, 64)
    Assert.Equal(r[1].Index, 14710)
    Assert.Equal(r[1].Length, 71)


// let rebar_input_5k =
//     "/mnt/g/repos/rebar/benchmarks/haystacks/opensubtitles/en-sampled.txt"
//     |> File.ReadLines
//     |> Seq.take 5000
//     |> String.concat "\n"

// [<Fact>]
// let rebar_counts_1() =
//     assertEqual 1833 (Sbre.Regex("[A-Za-z]{8,13}").Count(rebar_input_5k))




#endif


// let [<Literal>] SampleFile = "/home/ian/f/ttu/iti0303-regexp-text-extraction/ExtractText/data/training-samples/CongressBills-Date.json"
// type CongressProvider = FSharp.Data.JsonProvider<SampleFile>
// let ctx_congress = CongressProvider.GetSample()


// [<Fact>]
// let ``learning sample 1``() =
//     if true then () else
//     let r = Regex(@"(?<=( |`|\-|\n|3)⊤*).*&\w.*&.*\w")
//     // let r = Regex(@"(?<=( |`|\-|\n|3).*).*&\w.*\w")
//     // let r = Regex(@"(?<=( |`|\-|\n|3).*).*&\w.*\w")

//     for sample in ctx_congress.Examples do
//         let ms = r.Matches(sample.String)
//         ()
//     ()
//     // Assert.Equal(15, r.Length)



