[<Xunit.Collection("Sequential")>]
module Sbre.Test.BenchmarkTests

open System
open System.IO
open System.Text.RegularExpressions
open Sbre
open Sbre.Benchmarks.Jobs
open Xunit




let twainPatterns = [
    @"Twain"
    @"(?i)Twain"
    @"[a-z]shing"
    @"Huck[a-zA-Z]+|Saw[a-zA-Z]+"
    @"\b\w+nn\b"
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


let twain_20k = twain_input[..19999] // 10k chars limit

//
// [<Fact>]
// let ``loop subsumption test``() =
//     let m = Matcher(Permutations.permuteConjInParagraph [ @"[a-zA-Z]{0,12}ing\s";])
//     let r = m.Matches(paragraph) |> Seq.toArray
//     Assert.Equal(r.Length,9)


[<Fact>]
let ``paragraphs-huck``() =
    let m = Regex(@"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*")
    let r = m.Matches(twain_input) |> Seq.toArray
    // 411

    Assert.Equal(411, r.Length)
    Assert.Equal(r[0].Index, 3635093)
    Assert.Equal(r[1].Index, 3735436)
    Assert.Equal(r[2].Index, 3807398)

    Assert.Equal(r[0].Length, 845)
    Assert.Equal(r[1].Length, 362)
    Assert.Equal(r[2].Length, 1023)

