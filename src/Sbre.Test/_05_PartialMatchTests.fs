[<Xunit.Collection("Sequential")>]
module Sbre.Test._05_PartialMatchTests

#if DEBUG
open Sbre
open Sbre.Algorithm
open Sbre.Pat
open Sbre.Types
open Xunit
open Common
open Pat


[<Fact>]
let ``firstNullable 1``() = assertFirstNullablePos @".*(?=-)" @"aa-" 2

[<Fact>]
let ``firstNullable 2``() =
    assertFirstNullablePos @".*(?=.*-)&\S.*\S" @"-aaaa-" 2 // match end = 5

[<Fact>]
let ``firstNullable 3``() = assertFirstNullablePos @".*b" " aaab " 5

[<Fact>]
let ``firstNullable 4``() = assertFirstNullablePos @".*b|a" " aaab " 2 // match end = 5

[<Fact>]
let ``firstNullable 5``() = assertFirstNullablePos @"a+" " aaa " 2 // end 4

[<Fact>]
let ``firstNullable 6``() = assertFirstNullablePos @"a*" " aaa " 0


[<Fact>]
let ``allNullablePositions 1``() = assertNullablePositions "(ab)+" "__abab__ab__" [8; 4; 2]



#endif
