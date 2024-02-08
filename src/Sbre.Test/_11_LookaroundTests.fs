[<Xunit.Collection("Sequential")>]
module Sbre.Test._11_LookaroundTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Types
open Xunit
open Common

#if DEBUG


let getDfaMatcher (pat:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    let mutable _toplevelOr = matcher.TrueStarredPattern
    matcher


let getMatcher (pat:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    matcher


[<Fact>]
let ``lookahead nulls 1``() =
    assertNullablePositions "bb(?=aa)" "__bbaa__" [
        2
    ]

[<Fact>]
let ``lookahead left-to-right 1``() =
    assertMatchEnd "bb(?=aa)" "__bbaa__" 2 4



[<Fact>]
let ``lookahead full 1``() =
    assertAllLLmatchTexts "bb(?=aa)" "__bbaa__" [
        "bb"
    ]

[<Fact>]
let ``lookback nulls 1``() =
    assertNullablePositions "(?<=aa)bb" "__aabb__" [
        4
    ]


[<Fact>]
let ``lookback full 1``() =
    assertAllLLmatchTexts "(?<=aa)bb" "__aabb__" [
        "bb"
    ]


[<Fact>]
let ``anchor bol 1``() =
    _04_DerivativeTests.testPartDerivatives
        (@"^\d+", "123", [ @"φ*" ])

[<Fact>]
let ``anchor eol 1``() =
    _04_DerivativeTests.testPartDerivatives
        (@"1$", "1", [ @"((?=\n)|(?!⊤))"; @"((?!⊤)|(?=\n))" ])

// [<Fact>]
// let ``anchor eol 2``() =
//     let matcher = Regex(@"\d$")
//     let ism = matcher.IsMatch("1")
//     Assert.True(ism)


// [<Fact>]
// let ``anchor null 1``() =
//     let matcher = Regex("^1$")
//     let ism = matcher.IsMatch("111")
//     Assert.False(ism)


[<Fact>]
let ``neg lookahead 1``() =
    assertNullablePositions "bb(?!aa)" "__bb__" [ 2 ]

[<Fact>]
let ``neg lookahead 2``() =
    assertNullablePositions "bb(?!aa)" "__bbaa" [ ]

[<Fact>]
let ``neg lookahead 3``() =
    assertNullablePositions "bb(?!aa)" "__bb" [ 2 ]

[<Fact>]
let ``neg lookahead 4``() =
    assertNullablePositions "bb(?!aa)" "__bb_" [ 2 ]

// [<Fact>]
// let ``anchor null 2``() =
//     let matcher = Regex("^\\d$")
//     let ism = matcher.IsMatch("324")
//     Assert.False(ism)

//
// [<Fact>]
// let ``csa der 01`` () =
//     let matcher = getDfaMatcher "a{1,3}b"
//     let w  =1
//     Assert.Equal(4, 2)
//
//
//
// [<Fact>]
// let ``csa 01`` () =
//     let endPos = dfaFindMatchEnd "a{1,3}b" "baaabc"
//     Assert.Equal(4, endPos)
//




// T*.*a{3,7}b





#endif