[<Xunit.Collection("Sequential")>]
module Sbre.Test._05_SemanticsTests

#if DEBUG
open Sbre
open Sbre.Algorithm
open Sbre.Pat
open Sbre.Types
open Xunit
open Common
open Pat


[<Fact>]
let ``nullables 1``() =
    assertNullablePositions "(ab)+" "__abab__ab__" [ 8; 4; 2 ]

[<Fact>]
let ``llmatches 1``() =
    assertAllLLmatches "(ab)+" "__abab__ab__" [ 2, 4; 8, 2 ]

 // todo: this one might need an external oracle
[<Fact>]
let ``intersection 1``() =
    assertFirstMatchText @".*(?=.*E)&~(.*and.*)" @"___and__E" "___an"

[<Fact>]
let ``until 1``() =
    assertFirstMatchText @"a~(⊤*e⊤*)"  "abcdefghijklmnop" "abcd"



[<Fact>] // semantic difference for performance!
let ``constrained test 1.1 - inner lookarounds are constrained``() =
    let pattern = """\d(?=.*a)\d\d"""
    let input = "123___a"
    assertNoMatch pattern input

[<Fact>]
let ``constrained test 1.2 - inner lookarounds find a match in range``() =
    let pattern = """\d(?=.*a)\d.\d"""
    let input = "12a3___"
    assertFirstMatchText pattern input "12a3"
    // printAllDerivatives pattern input []

[<Fact>]
let ``constrained test 2.1 - intersections constrain match``() =
    let pattern = """.*(?=.*def)&.*def"""
    let input = "abcdefdef"
    assertFirstMatchText pattern input "abcdef"

[<Fact>]
let ``constrained test 2.2 - impossible match``() =
    let pattern = """.*(?=.*def)&.*def"""
    let input = "abcdef"
    assertNoMatch pattern input

[<Fact>]
let ``constrained test 2.3 - intersection range is pre-constrained``() =
    let pattern = """.*(?=.*def)&.*de"""
    let input = "abcdefdef"
    assertFirstMatchText pattern input "abcde"

[<Fact>]
let ``constrained test 2.4 - suffixes work as usual``() =
    let pattern = """(.*a.*&.*c.*)(?=.*def)"""
    let input = "abcdef"
    assertFirstMatchText pattern input "abc"


[<Fact>]
let ``constrained test 3.1 - union does not constrain lookarounds``() =
    let pattern = """ab(?=.*def)|de(?=.*f)"""
    let input = "abcdef"
    assertAllLLmatchTexts pattern input [ "ab" ; "de" ]


[<Fact>]
let ``constrained test 3.2 - suffixes can be merged``() =
    let pattern = """a.+(?=.*f)(?=.*e)(?=c)"""
    let input = "abcdef"
    assertAllLLmatchTexts pattern input [ "ab" ; ]

[<Fact>]
let ``constrained test 3.3 - prefixes can be merged``() =
    let pattern = """(?<=f)(?<=e.*)(?<=c.*).+"""
    let input = "abcdefghij"
    assertAllLLmatchTexts pattern input [ "ghij" ]
    // printAllDerivatives pattern input []

[<Fact>]
let ``non-space char`` () =
    assertNullablePositions @".*(?=.*-)&\S.*\S" @"-aaaa-" [
        yield! List.rev [ 0..3 ]
    ]


[<Fact>]
let ``unsupported test 1``() =
    let success =
        try
            assertFirstMatchText @"..(?<=a.*)" "aa" "aa"
            true
        with e ->
            false
    assertFalse success

[<Fact>]
let ``unsupported test 2``() =
    let success =
        try
            assertNoMatch """.*(?<=aaa)""" "aa"
            true
        with e ->
            false
    success



[<Fact>] 
let ``unsupported test 3.1``() = assertAllLLmatches @"(?<!\d)a" " a" [  1,1; ]



#endif
