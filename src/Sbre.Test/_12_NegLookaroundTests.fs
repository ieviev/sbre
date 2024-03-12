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
let ``der neg anchor 1``() = _04_DerivativeTests.testRevDerivative (@"(?!b)","b",[
    @"⊥"
    @"(?<=~((ε|⊤*b)))" // TODO: unsure
    @"(?<=~((⊤*b|ε)))"
    @"(?<=~((⊤*b)?))"
    @"(?<=~((⊤*a)?))b"
])

[<Fact>]
let ``der neg anchor 2``() = _04_DerivativeTests.testRevDerivative (@"(?!b)","a",[
    // @"ε"
    @"(?<=~(⊤*b))"
    @"((?<=~(⊤*a))b)?"
])

[<Fact>]
let ``der neg anchor 3``() = _04_DerivativeTests.testRevDerivative (@"bb(?!b)","b",[
    @"(b|(?<=~((⊤*b|ε)))bb)"
    @"(b|(?<=~((ε|⊤*b)))bb)"
    @"((?<=~((⊤*b|ε)))bb|b)"
    @"((?<=~((ε|⊤*b)))bb|b)"
    @"(b|(?<=~((⊤*b)?))bb)"
    @"((?<=~((⊤*b)?))bb|b)"
    @"((?<=~((⊤*b)?))b)?b"
])

// [<Fact>]
// let ``der neg anchor 4``() = _04_DerivativeTests.testRevDerivative (@"bb(?!a)","b",[ @"b"; ])

[<Fact>]
let ``der neg anchor lb 1``() = _04_DerivativeTests.testPartDerivatives (@"(?<!a)b", "ab", [
    "⊥"
    @"(?<=~((⊤*a)?))b"
])

[<Fact>]
let ``der neg anchor lb 2``() = _04_DerivativeTests.testPartDerivatives (@"(?<!a)b", "bb", [
    "ε" // subsumed
    "(?=ε)" // subsumed
    @"((?<=~(⊤*a))b)?"
])



[<Fact>]
let ``c ahead 1.1``() = assertRawDerivative @"(?=1)11" "11" [
    "1"
]




[<Fact>]
let ``lookarounds test 2``() =
    let matcher = Regex("""1(?! Sep)""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.False(ism)


[<Fact>]
let ``lookarounds test 2.1``() =
    let matcher = Regex("""^(1(?! (Sep)))$""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.False(ism)

[<Fact>]
let ``lookarounds test 2.2``() =
    let matcher = Regex("""^(a(?!b))$""")
    let ism = matcher.IsMatch("ab")
    Assert.False(ism)

[<Fact>]
let ``lookarounds test 2.3``() =
    let matcher = Regex("""^(a(?!b)).*$""")
    let ism = matcher.IsMatch("ab")
    Assert.False(ism)
    // printAllDerivatives """^(a(?!b)).*$""" "ab" []

[<Fact>]
let ``lookarounds test 2.4``() =
    let matcher = Regex("""1\b-""")
    let ism = matcher.IsMatch("1-")
    Assert.True(ism)

[<Fact>]
let ``lookarounds test 2.5``() =
    let matcher = Regex("""1\b-""")
    let ism = matcher.IsMatch("1-")
    Assert.True(ism)



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
let ``d rewritten test 1.1``() =
    assertFirstMatchText @".(?<=a)" "aaa" "a"

[<Fact>]
let ``d rewritten test 1.2``() =
    assertFirstMatchText @".*(?<=a)" "aaa" "aaa"

// [<Fact>]
// let ``d rewritten test 1.3``() =
//     assertFirstMatchText @".*(?<=b)" "aaa" "aaa"

[<Fact>]
let ``d rewritten test 2.1``() =
    assertFirstMatchText  @"1(?=[012])\d" "11" "11"






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

let testSameAsRuntime = _07_ComparisonTests.testSameAsRuntime

[<Fact>]
let ``regex with label 1``() =
    let pattern = """(?<Time>^(?:0?[1-9]:[0-5]|1(?=[012])\d:[0-5])\d(?:[ap]m)?)"""
    let input = "12:00am"
    testSameAsRuntime pattern input


[<Fact>]
let ``regex with label 2``() =
    let pattern = """^(?:0?[1-9]:[0-5]|1(?=[012])\d:[0-5])\d(?:[ap]m)?"""
    let input = "12:00am"
    testSameAsRuntime pattern input

[<Fact>]
let ``same as runtime 1``() =
    let pattern = """^((31(?!\ (Feb(ruary)?|Apr(il)?|June?|(Sep(?=\b|t)t?|Nov)(ember)?)))|((30|29)(?!\ Feb(ruary)?))|(29(?=\ Feb(ruary)?\ (((1[6-9]|[2-9][0-9])(0[48]|[2468][048]|[13579][26])|((16|[2468][048]|[3579][26])00)))))|(0?[1-9])|1[0-9]|2[0-8])\ (Jan(uary)?|Feb(ruary)?|Ma(r(ch)?|y)|Apr(il)?|Ju((ly?)|(ne?))|Aug(ust)?|Oct(ober)?|(Sep(?=\b|t)t?|Nov|Dec)(ember)?)\ ((1[6-9]|[2-9][0-9])[0-9]{2})$"""
    let input = "31 September 2003"
    testSameAsRuntime pattern input

[<Fact>]
let ``same as runtime 2``() =
    let pattern = """^(1(?= ((Sept?)(em)?)) Sept? 1)$"""
    let input = "1 Sept 1"
    testSameAsRuntime pattern input



[<Fact>]
let ``same as runtime 6``() =
    let pattern = """^(1(?! ((Sep(?=\b|t)t?|Nov)(ember)?))).*$"""
    let input = "31 September 2003"
    testSameAsRuntime pattern input



// [<Fact>]
// let ``web app test 7``() =
//     let result = getAllLLmatches """(?<=\n\n|\A)~(⊤*\n\n⊤*)(?=\n\n|\Z)&(~(⊤*charity⊤*)|(⊤*honor⊤*))""" _06_MatchTests.webappsample7
//     Assert.Equal([(0, 18)], result |> Seq.map (fun v -> v.Index,v.Length) )
//



[<Fact>]
let ``nested not 1``() =
    // let pattern = """(?!^0*$)(?!^0*\.0*$)^\d{1,5}(\.\d{1,3})?$"""
    let pattern = """~(^0*$)&~(^0*\.0*$)&^\d{1,5}(\.\d{1,3})?$"""
    let input = "12345.123"
    assertFirstMatchText pattern input "12345.123"




[<Fact>]
let ``ranges 1``() = assertAllLLmatches @"(?<=\d)a" "1a__a__a" [  1,1; ]

[<Fact>]
let ``ranges 2.1``() = assertAllLLmatches @"(?<!\d)a" "1a__a__a" [  4,1;  7,1 ]


[<Fact>] 
let ``ranges 2.2``() = assertAllLLmatches @"(?<!\d)a" " a" [  1,1; ]











#endif