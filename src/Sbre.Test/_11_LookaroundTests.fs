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
let ``a pos simple 1.1``() = assertNullablePositions "(?=b)" "b" [ 0 ]

[<Fact>]
let ``a pos simple 1.2``() = assertNullablePositions "a(?=b)" "_ab_ab_" [ 4; 1 ]

[<Fact>]
let ``a pos simple 1.3``() = assertNullablePositions "a(?=b_)" "_ab_ab_" [ 4; 1 ]

[<Fact>]
let ``a pos simple 1.4``() = assertNullablePositions ".*(?=b_)" "_ab_ab_" (List.rev [ 0 .. 5 ])

[<Fact>]
let ``a pos simple 1.6``() = assertNullablePositions "(ab)+(?=_)" "_ab_ab_" [4;1]

[<Fact>]
let ``a pos simple 1.7``() = assertNullablePositions "bb(?=aa)" "__bbaa__" [ 2 ]


[<Fact>]
let ``a pos simple 2.1a``() = assertMatchEnd "(?=a)" "a" 0 0

[<Fact>]
let ``a pos simple 2.1b``() = assertMatchEnd "a(?=a)" "aa" 0 1

[<Fact>]
let ``a pos simple 2.1c``() = assertMatchEnd "bb(?=a)" "__bbaaa__" 2 4

[<Fact>]
let ``a pos simple 2.2``() = assertMatchEnd "bb(?=aa)" "__bbaa__" 2 4

[<Fact>]
let ``a pos simple 2.3``() = assertMatchEnd "bb(?=aaa)" "__bbaaa__" 2 4

[<Fact>]
let ``a pos simple 2.4``() = assertMatchEnd "bb(?=aaaa)" "__bbaaaa__" 2 4

[<Fact>]
let ``a pos simple 2.5``() = assertMatchEnd "bb(?=aaaaa)" "__bbaaaaa__" 2 4


[<Fact>]
let ``a pos simple 3.1``() = assertAllLLmatchTexts "a(?=b)" "_ab_ab_" [ "a"; "a" ]

[<Fact>]
let ``a pos simple 3.2``() = assertAllLLmatchTexts "bb(?=aa)" "__bbaa__" [ "bb" ]



[<Fact>]
let ``b pos simple 1.1``() = assertNullablePositions "(?<=b)" "b" [ 1 ]

[<Fact>]
let ``b pos simple 1.2``() = assertNullablePositions "(?<=bb)" "bb" [ 2 ]

[<Fact>]
let ``b pos simple 1.3``() = assertNullablePositions "(?<=bbb)" "bbb" [ 3 ]

[<Fact>]
let ``b pos simple 1.4``() = assertNullablePositions "(?<=bbbb)" "bbbb" [ 4 ]

[<Fact>]
let ``b pos simple 1.5``() = assertNullablePositions "(?<=b)b" "bb" [ 1 ]

[<Fact>]
let ``b pos simple 1.6``() = assertNullablePositions "(?<=bb)b" "bbb" [ 2 ]

[<Fact>]
let ``b pos simple 1.7a``() = assertNullablePositions "(?<=b)a" "ba" [ 1 ]

[<Fact>]
let ``b pos simple 1.7b``() = assertNullablePositions "(?<=b)a" "bba" [ 2 ]

[<Fact>]
let ``b pos simple 1.7c``() = assertNullablePositions "(?<=b)a" "bbbba" [ 4 ]

[<Fact>]
let ``b pos simple 1.7c dbg``() = assertAllDerivatives "(?<=b)a" "bbbba" [
    ["((?=b)|⊤*a(?=b))"]
    ["((?=ε)[1]|⊤*a(?=b))"]
    [""]
]



[<Fact>]
let ``b pos simple 1.8``() = assertNullablePositions "(?<=bb)a" "bbbba" [ 4 ]

[<Fact>]
let ``b pos simple 1.9``() = assertNullablePositions "(?<=bbb)a" "bbbba" [ 4 ]



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
let ``multiple nullables 1``() =
    assertNullablePositions "(?<=c.*)(ab){1,3}" "c_ababab" [
        6; 4; 2
    ]

[<Fact>]
let ``multiple nullables 2``() =
    assertNullablePositions "(ab){1,3}(?=.*c)" "__ababab_c" [
        6; 4; 2
    ]


//
// [<Fact>]
// let ``anchor bol 1``() =
//     _04_DerivativeTests.testPartDerivatives
//         (@"^\d+", "123", [ @"φ*" ])
//
// [<Fact>]
// let ``anchor eol 1``() =
//     _04_DerivativeTests.testPartDerivatives
//         (@"1$", "1", [ @"((?=\n)|(?!⊤))"; @"((?!⊤)|(?=\n))" ])


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