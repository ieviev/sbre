[<Xunit.Collection("Sequential")>]
module Sbre.Test._11_LookaroundTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Types
open Xunit
open Common

#if DEBUG




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


//

[<Fact>]
let ``neg lookahead 1``() =
    assertNullablePositions "bb(?!aa)" "__bb__" [ 2 ]

[<Fact>] // difficult case, can start match from bb|aa
let ``neg lookahead 2``() =
    assertNullablePositions "bb(?!aa)" "__bbaa" [ ]

[<Fact>]
let ``neg lookahead 3``() =
    assertNullablePositions "bb(?!aa)" "__bb" [ 2 ]

[<Fact>]
let ``neg lookahead 4``() =
    assertNullablePositions "bb(?!aa)" "__bb_" [ 2 ]


[<Fact>]
let ``neg simple 1``() =
    assertNullablePositions "b(?!a)" "b" [ 0 ]


[<Fact>]
let ``neg simple 2``() = assertNullablePositions "b(?!a)" "bb" [ 1; 0 ]

[<Fact>]
let ``neg simple 3``() = assertNullablePositions "b(?!aa)" "bb" [ 1; 0 ]

[<Fact>]
let ``neg simple 4``() = assertNullablePositions "b(?!aaa)" "bb" [ 1; 0 ]


[<Fact>]
let ``neg simple 5``() = assertNullablePositions "b(?!a)" "ba" [ ]


[<Fact>]
let ``neg states 1``() =
    assertRevStates "b(?!a)" "bb" [
        [ @"⊤*(?<=((~(a)&\z⊤)|\z))b" ]
        [ "b(?!a)" ]
        [ "⊤*(?<=((⊤&~(a))|\z))b" ]
    ]




// [<Fact>] // difficult case, can start match from bb|aa
// let ``neg lookahead 2``() =
//     assertNullablePositions "bb(?!aa)" "__bbaa" [ ]

// [<Fact>]
// let ``lookahead comparison 1``() =
//     // let _ = getAllLLmatches "aa(?!bb)" "aabb_aa_aab" [5,2; 8,2]
//     let expected = [5,2; 8,2] // expected for "aa(?!bb)"
//     // |bb - not null, b|b - not null, bb| - null,
//     // let matches = getAllLLmatches "aa(?=~(bb))" "aabb_aa_aab" |> matchPosToTuples
//     // let matches = getAllLLmatches "aa(?=~(⊤*))" "aabb_aa_aab" |> matchPosToTuples
//     let matches = getAllLLmatches "aa(?=~(|b|bb))" "aabb_aa_aab" |> matchPosToTuples
//     assertAllEqual expected matches
//




#endif