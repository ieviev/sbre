[<Xunit.Collection("Sequential")>]
module Sbre.Test._12_NegLookaroundTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Types
open Xunit
open Common

#if DEBUG

[<Fact>]
let ``der neg anchor 1``() = _04_DerivativeTests.testRevDerivative (@"(?!b)","b",[ @"⊥"; ])

[<Fact>]
let ``der neg anchor 2``() = _04_DerivativeTests.testRevDerivative (@"(?!b)","a",[ @"ε"; ])

[<Fact>]
let ``der neg anchor 3``() = _04_DerivativeTests.testRevDerivative (@"bb(?!b)","b",[ @"⊥"; ])

[<Fact>]
let ``der neg anchor 4``() = _04_DerivativeTests.testRevDerivative (@"bb(?!a)","b",[ @"b"; ])

[<Fact>]
let ``der neg anchor lb 1``() = _04_DerivativeTests.testPartDerivative (@"(?<!a)b", "ab", "⊥")

[<Fact>]
let ``der neg anchor lb 2``() = _04_DerivativeTests.testPartDerivative (@"(?<!a)b", "bb", "ε")




[<Fact>]
let ``lookarounds test 2``() =
    let matcher = Regex("""1(?! Sep)""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.False(ism)


[<Fact>]
let ``lookarounds test 3``() =
    let matcher = Regex("""^(1(?! (Sep))).*$""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.False(ism)

[<Fact>]
let ``lookarounds test 4``() =
    let matcher = Regex("""^(1(?= (Sep))).*$""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.True(ism)


[<Fact>]
let ``lookarounds test 5``() =
    assertNoMatch """.*(?<=aaa)""" "aa"

[<Fact>]
let ``inverted startset test 1``() =
    assertFirstMatchText @"..(?<=A.*)" "Aa" "Aa"

[<Fact>]
let ``derivative lookaround 1.3``() = assertRawDerivative @"(?=1)11" "11" ["1"]




//
// [<Fact>]
// let ``neg simple 1``() = assertNullablePositions "(?!a)" "b" [ 1; 0 ]
//
// [<Fact>]
// let ``neg simple 1.1``() = assertNullablePositions "(?<!a)" "b" [ 1; 0 ]
//
// [<Fact>]
// let ``neg simple 1.2``() = assertNullablePositions "(?<!a)" "a" [ 0 ]
//
//
// [<Fact>]
// let ``neg simple 2``() = assertNullablePositions "b(?!a)" "bb" [ 1; 0 ]
//
// [<Fact>]
// let ``neg simple 3``() = assertNullablePositions "b(?!aa)" "bb" [ 1; 0 ]
//
// [<Fact>]
// let ``neg simple 4``() = assertNullablePositions "b(?!aaa)" "bb" [ 1; 0 ]
//
//
// [<Fact>]
// let ``neg simple 5``() = assertNullablePositions "b(?!a)" "ba" [ ]

//
// [<Fact>]
// let ``neg lookahead 1``() =
//     assertNullablePositions "bb(?!aa)" "__bb__" [ 2 ]
//
// [<Fact>] // difficult case, can start match from bb|aa
// let ``neg lookahead 2``() =
//     assertNullablePositions "bb(?!aa)" "__bbaa" [ ]
//
// [<Fact>]
// let ``neg lookahead 3``() =
//     assertNullablePositions "bb(?!aa)" "__bb" [ 2 ]
//
// [<Fact>]
// let ``neg lookahead 4``() =
//     assertNullablePositions "bb(?!aa)" "__bb_" [ 2 ]
//

// [<Fact>] // difficult case, can start match from bb|aa
// let ``neg lookahead 2``() =
//     assertNullablePositions "bb(?!aa)" "__bbaa" [ ]




#endif