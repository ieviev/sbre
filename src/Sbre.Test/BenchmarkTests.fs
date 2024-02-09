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
    assertAllEqual
        (System.Text.RegularExpressions.Regex(pat).Matches(twain_input) |> Seq.map (fun v -> struct (v.Index,v.Length)))
        (Sbre.Regex(pat).Matches(twain_input) |> Seq.map (fun v -> struct (v.Index,v.Length)))


// [<Fact>]
// let ``paragraphs-huck``() =
//     let m = Regex(@"~(⊤*\n\n⊤*)&⊤*Huck⊤*")
//     let r1 = m.Matches(twain_input) |> Seq.toArray
//     let r2 =
//         Benchmarks.Jobs.twoStepSearch ["Huck"] twain_input
//         |> Seq.toArray
//
//     [|
//         Assert.Equal(411, r2.Length)
//         Assert.Equal(3635093, r2[0].Index)
//         Assert.Equal(845,r2[0].Length)
//         Assert.Equal(3735436, r2[1].Index)
//         Assert.Equal(362,r2[1].Length)
//     |]
//     |> Array.iter id
//
//
//     Assert.Equal(r2.Length, r1.Length)
//     Assert.Equal(r2[0].Index, r1[0].Index)
//     Assert.Equal(r2[0].Length,r1[0].Length)
//     Assert.Equal(r2[1].Index, r1[1].Index)
//     Assert.Equal(r2[1].Length,r1[1].Length)
//
//
// [<Fact>]
// let ``paragraphs-huck-2``() =
//     let m = Regex(@"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*")
//     let r1 = m.Matches(twain_input) |> Seq.toArray
//     let r2 =
//         Benchmarks.Jobs.twoStepSearch ["Huck"] twain_input
//         |> Seq.toArray
//
//     [|
//         Assert.Equal(411, r2.Length)
//         Assert.Equal(3635093, r2[0].Index)
//         Assert.Equal(845,r2[0].Length)
//         Assert.Equal(3735436, r2[1].Index)
//         Assert.Equal(362,r2[1].Length)
//     |]
//     |> Array.iter id
//
//     Assert.Equal(r2.Length, r1.Length)
//     Assert.Equal(r2[0].Index, r1[0].Index)
//     Assert.Equal(r2[0].Length,r1[0].Length - 1)
//     Assert.Equal(r2[1].Index, r1[1].Index)
//     Assert.Equal(r2[1].Length,r1[1].Length - 1)
//
// // ["Huck"; "could"; "here"  ]
//
// [<Fact>]
// let ``paragraphs-huck-could``() =
//     let m = Regex(@"~(⊤*\n\n⊤*)&⊤*Huck⊤*&⊤*could⊤*")
//     let r1 = m.Matches(twain_input) |> Seq.toArray
//     let r2 =
//         Benchmarks.Jobs.twoStepSearch ["Huck";"could"] twain_input
//         |> Seq.toArray
//
//     Assert.Equal(54, r2.Length)
//     Assert.Equal(3808421, r2[0].Index)
//     Assert.Equal(688,r2[0].Length)
//     Assert.Equal(3840813, r2[1].Index)
//     Assert.Equal(1962,r2[1].Length)
//
//     Assert.Equal(r2.Length, r1.Length)
//     Assert.Equal(r2[0].Index, r1[0].Index)
//     Assert.Equal(r2[0].Length,r1[0].Length)
//     Assert.Equal(r2[1].Index, r1[1].Index)
//     Assert.Equal(r2[1].Length,r1[1].Length)
//
//
//
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

//
// [<Fact>]
// let ``which could test``() =
//     let m = Regex(Permutations.permuteConjInLine ["w[a-z]*h ";"c[a-z]*d "])
//     let r = m.Matches(twain_input[..100_000]) |> Seq.toArray
//
//     Assert.Equal(24, r.Length)
//     Assert.Equal(r[0].Index, 17155)
//     Assert.Equal(r[0].Length, 46)
//     Assert.Equal(r[1].Index, 32338)
//     Assert.Equal(r[1].Length, 71)


// ["w[a-z]+h";"c[a-z]+d"]



//
// [<Fact>]
// let ``line test 1``() =
//     let m = Regex(@".* the .*&.* and .*")
//     let r = m.Matches(twain_input[..15_000]) |> Seq.toArray
//     // 411
//
//     Assert.Equal(2, r.Length)
//     Assert.Equal(r[0].Index, 113)
//     Assert.Equal(r[0].Length, 64)
//     Assert.Equal(r[1].Index, 14710)
//     Assert.Equal(r[1].Length, 71)


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


// [<Fact>]
// let ``line 4 words test 2``() =
//     let m = Regex(@".*that.*&.*and.*&.*the.*&.*was.*")
//     let r = m.Matches(twain_input[..100_000]) |> Seq.toArray
//
//     Assert.Equal(5, r.Length)
//
//
// [<Fact>]
// let ``line 4 words test alt``() =
//     let m = Regex(Permutations.permuteAltInLine ["which";"could"])
//     let r = m.Matches(twain_input[..500_000]) |> Seq.toArray
//     Assert.Equal(10, r.Length)
//
//
// [<Fact>]
// let ``line 4 words test partial alt``() =
//     let m = Regex(Permutations.permuteAltInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"])
//     let r = m.Matches(twain_input[..100_000]) |> Seq.toArray
//
//     Assert.Equal(12, r.Length)
//
//
// [<Fact>]
// let ``line 4 words test partial conj``() =
//     let m = Regex(Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"])
//     let r = m.Matches(twain_input[..100_000]) |> Seq.toArray
//     Assert.Equal(15, r.Length)
//
//


#endif