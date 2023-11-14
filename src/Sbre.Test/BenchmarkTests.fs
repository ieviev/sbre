[<Xunit.Collection("Sequential")>]
module Sbre.Test.BenchmarkTests

open System
open System.IO
open System.Text.RegularExpressions
open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Pat
open Sbre.Types
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
    let m = Regex(@"~(⊤*\n\n⊤*)&⊤*Huck⊤*")
    let r1 = m.Matches(twain_input) |> Seq.toArray
    let r2 =
        Benchmarks.Jobs.twoStepSearch ["Huck"] twain_input
        |> Seq.toArray

    [|
        Assert.Equal(411, r2.Length)
        Assert.Equal(3635093, r2[0].Index)
        Assert.Equal(845,r2[0].Length)
        Assert.Equal(3735436, r2[1].Index)
        Assert.Equal(362,r2[1].Length)
    |]
    |> Array.iter id


    Assert.Equal(r2.Length, r1.Length)
    Assert.Equal(r2[0].Index, r1[0].Index)
    Assert.Equal(r2[0].Length,r1[0].Length)
    Assert.Equal(r2[1].Index, r1[1].Index)
    Assert.Equal(r2[1].Length,r1[1].Length)


[<Fact>]
let ``paragraphs-huck-2``() =
    let m = Regex(@"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*")
    let r1 = m.Matches(twain_input) |> Seq.toArray
    let r2 =
        Benchmarks.Jobs.twoStepSearch ["Huck"] twain_input
        |> Seq.toArray

    [|
        Assert.Equal(411, r2.Length)
        Assert.Equal(3635093, r2[0].Index)
        Assert.Equal(845,r2[0].Length)
        Assert.Equal(3735436, r2[1].Index)
        Assert.Equal(362,r2[1].Length)
    |]
    |> Array.iter id

    Assert.Equal(r2.Length, r1.Length)
    Assert.Equal(r2[0].Index, r1[0].Index)
    Assert.Equal(r2[0].Length,r1[0].Length - 1)
    Assert.Equal(r2[1].Index, r1[1].Index)
    Assert.Equal(r2[1].Length,r1[1].Length - 1)

// ["Huck"; "could"; "here"  ]

[<Fact>]
let ``paragraphs-huck-could``() =
    let m = Regex(@"~(⊤*\n\n⊤*)&⊤*Huck⊤*&⊤*could⊤*")
    let r1 = m.Matches(twain_input) |> Seq.toArray
    let r2 =
        Benchmarks.Jobs.twoStepSearch ["Huck";"could"] twain_input
        |> Seq.toArray

    Assert.Equal(54, r2.Length)
    Assert.Equal(3808421, r2[0].Index)
    Assert.Equal(688,r2[0].Length)
    Assert.Equal(3840813, r2[1].Index)
    Assert.Equal(1962,r2[1].Length)

    Assert.Equal(r2.Length, r1.Length)
    Assert.Equal(r2[0].Index, r1[0].Index)
    Assert.Equal(r2[0].Length,r1[0].Length)
    Assert.Equal(r2[1].Index, r1[1].Index)
    Assert.Equal(r2[1].Length,r1[1].Length)




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
let ``which could test``() =
    let m = Regex(Permutations.permuteConjInLine ["w[a-z]*h ";"c[a-z]*d "])
    let r = m.Matches(twain_input[..100_000]) |> Seq.toArray

    Assert.Equal(24, r.Length)
    Assert.Equal(r[0].Index, 17155)
    Assert.Equal(r[0].Length, 46)
    Assert.Equal(r[1].Index, 32338)
    Assert.Equal(r[1].Length, 71)


// ["w[a-z]+h";"c[a-z]+d"]




[<Fact>]
let ``line test 1``() =
    let m = Regex(@".* the .*&.* and .*")
    let r = m.Matches(twain_input[..15_000]) |> Seq.toArray
    // 411

    Assert.Equal(2, r.Length)
    Assert.Equal(r[0].Index, 113)
    Assert.Equal(r[0].Length, 64)
    Assert.Equal(r[1].Index, 14710)
    Assert.Equal(r[1].Length, 71)

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


[<Fact>]
let ``line 4 words test 2``() =
    let m = Regex(@".*t.*hat.*&.*a.*nd.*&.*t.*he.*&.*w.*as.*")
    let r = m.Matches(twain_input[..100_000]) |> Seq.toArray

    Assert.Equal(15, r.Length)


[<Fact>]
let ``line 4 words test alt``() =
    let m = Regex(Permutations.permuteAltInLine ["which";"could"])
    let r = m.Matches(twain_input[..500_000]) |> Seq.toArray

    Assert.Equal(10, r.Length)

// [<Fact>]
// let ``last character out of reach``() =
//     let m = Regex(Permutations.permuteAltInLine [@"\n.{0,20}"])
//     let r = m.Matches(twain_input) |> Seq.toArray
//
//     Assert.Equal(23636, r.Length)
//



// [<Fact>]
// let ``skip position test 3``() =
//     let matcher = Regex(@".*Huck.*Finn.*")
//     let text = twain_input
//     let loc = Location.create text 0
//
//     let prefix = matcher.Cache.Builder.GetPrefixCached(matcher.RawPattern)
//     let result =
//         match prefix with
//         | InitialStartset.MintermArrayPrefix(prefix=arr; loopTerminator=term) ->
//             // matcher.Cache.TryNextStartsetLocationArrayWithLoopTerminator(loc, arr,term)
//             matcher.Cache.TryNextStartsetLocationArray(&loc, arr)
//         | _ -> failwith "todo"
//
//     let mutable pos2 = 0
//     let result2 =
//         matcher.Cache.Optimizations.TryFindNextStartingPositionLeftToRight(
//             text,&pos2,pos2)
//     let v = 1
//
//     Assert.Equal(8, pos2)








