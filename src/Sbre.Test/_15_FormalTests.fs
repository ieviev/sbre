[<Xunit.Collection("Sequential")>]
module Sbre.Test._15_FormalTests

open System.Buffers
open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Info
open Sbre.Optimizations
open Sbre.Types
open Xunit
open Common

#if DEBUG

[<Fact>] // oracle has T in 2 and 5 (?=a)
let ``onlylookaround 1``() = assertNullablePositions "a" "bba_ba" [ 5; 2 ] // "bb|a_b|a"

[<Fact>]
let ``onlymatch 1``() = assertNullablePositions "b" "bba_ba" [ 4; 1 ; 0 ] // "|b|ba_|ba"

[<Fact>]
let ``matchends 1``() = assertAllLLmatches "b" "bba_ba" [  0,1; 1,1 ;4,1; ] // "|b|ba_|ba"

// potential matches end at (0,1)=1, (1,1) = 2, (4,1) = 5
// oracle removes (0,1) because it is not T
// the rest are full matches

[<Fact>]
let ``fullmatch 1``() = assertNullablePositions "b(?=a)" "bba_ba" [ 4; 1 ] // "b|ba_|ba"







#endif