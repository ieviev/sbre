[<Xunit.Collection("Sequential")>]
module Sbre.Test._03_StartsetTests


#if DEBUG


open System
open System.Collections
open System.Collections.Generic
open System.Globalization
open System.Runtime.InteropServices
open System.Text
open System.Text.RegularExpressions
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre
open Sbre.Pat
open Sbre.Test
open Sbre.Pat.Extensions
open Xunit

module Helpers =
    let charSetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()
    let bddBuilder = SymbolicRegexBuilder<BDD>(charSetSolver, charSetSolver)


[<Fact>]
let ``startset generation 1``() =
    let matcher = Matcher(@"⊤*Huck⊤*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("H", ss1pretty)


[<Fact>]
let ``startset generation 2``() =

    let matcher = Matcher(@"⊤*(Huck|Finn)⊤*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("[FH]", ss1pretty)



[<Fact>]
let ``startset generation 3``() =

    let matcher = Matcher(@"\n\n~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*&⊤*Paris⊤*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[\nEKP]", ss1pretty)



[<Fact>]
let ``startset generation 4``() =
    let matcher = Matcher(@"(b|)*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[]", ss1pretty)


[<Fact>]
let ``startset generation 5``() =
    let matcher = Matcher(@"(a|ab)*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@".", ss1pretty)



[<Fact>]
let ``startset generation 6``() =

    let matcher = Matcher(@"⊤*Finn⊤*&⊤*Huck⊤*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("[FH]", ss1pretty)


[<Fact>]
let ``startset generation 7``() =

    let matcher = Matcher(@"(.+\n)+\n")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (matcher.DotStarredPattern)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    let ss2 = Info.Startset.inferStartset2 (c.Solver) (matcher.DotStarredPattern)
    let ss2pretty = c.PrettyPrintMinterm(ss2)
    Assert.Equal(@"[^\n]", ss1pretty)
    Assert.Equal(@".", ss2pretty) // FULL SET




[<Fact>]
let ``reverse startset generation 1``() =
    let matcher = Matcher(@"\n\n~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*&⊤*Paris⊤*")
    let c = matcher.Cache
    let patstr = matcher.ReversePattern.ToStringHelper()
    let ss1 = Info.Startset.inferStartset (c.Solver) (matcher.ReversePattern)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[\nghs]", ss1pretty)

[<Fact>]
let ``startset2 generation 1``() =

    let matcher = Matcher(@"⊤*English⊤*&⊤*King⊤*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset2 (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("[in]", ss1pretty)


// [<Fact>]
// let ``startset2 generation 2``() =
//
//     let matcher = Matcher(@"(.*a&~(.*b.*))")
//     let c = matcher.Cache
//     let ss1 = Info.Startset.inferStartset2 (c.Solver) (c.InitialPatternWithoutDotstar)
//     let ss1pretty = c.PrettyPrintMinterm(ss1)
//     Assert.Equal("[FH]", ss1pretty)
//
//



//TODO:
// [<Fact>]
// let ``startset2 generation 2`` () =
//
//     let matcher = Matcher(@"⊤*(Finn|Huck)⊤*")
//     let c = matcher.Cache
//     let ss1 = Info.Startset.inferStartset2 (c.Solver) (c.InitialPatternWithoutDotstar)
//     let ss1pretty = c.PrettyPrintMinterm(ss1)
//     Assert.Equal("[in]", ss1pretty)


[<Fact>]
let ``startsetChars of bdd 1``() =
    let matcher = Matcher(@"⊤*Finn⊤*&⊤*Huck⊤*")
    let bdds = matcher.Cache.MintermBdds()
    let startsetChars =
        bdds[1..] |> Array.map (fun v -> StartsetHelpers.bddToStartsetChars (v))

    let charArrays =
        startsetChars |> Array.map (fun v -> v.Chars |> String) |> String.concat ","
    // let ss1 = Info.Startset.inferStartset2 (c.Solver) (c.InitialPatternWithoutDotstar)
    // let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.StrictEqual(charArrays, "F,H,c,i,k,n,u" )


[<Fact>]
let ``startsetChars of bdd 2 - merged span``() =
    let matcher = Matcher(@"⊤*Finn⊤*&⊤*Huck⊤*")
    let c = matcher.Cache
    let bdds = c.MintermBdds()

    let startsetsArray = StartsetHelpers.startsetsFromMintermArray(bdds)
    let uintminterms = c.Minterms()

    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)

    let mergedIndexOf =
        StartsetHelpers.getMergedIndexOfSpan(startsetsArray, c.Minterms(),  ss1)

    let mergedStr =
        String(mergedIndexOf.ToArray())

    Assert.Equal("FH",mergedStr)



[<Fact>]
let ``startset concat reversed``() =
    let matcher = Matcher(@".*EHT&.*EVIF.*")
    let c = matcher.Cache

    let ss2 = Info.Startset.inferStartset2 (c.Solver) (matcher.RawPattern)
    let ss2pretty = c.PrettyPrintMinterm(ss2)
    Assert.Equal(ss2pretty, @".") // important that .* is not optimized away
    // [HV]



[<Fact>]
let ``startset concat reversed 2``() =
    let matcher = Matcher(@"(.*EHT&.*EVIF.*)")
    let c = matcher.Cache

    let ss2 = Info.Startset.inferStartset2 (c.Solver) (matcher.RawPattern)
    let ss2pretty = c.PrettyPrintMinterm(ss2)
    Assert.Equal(ss2pretty, @".") // important that .* is not optimized away
    // [HV]



// var from = span0.Length;
//
// var resultSpan = result.AsSpan();
// span0.CopyTo(result);
//             var from = span0.Length;
//             span1.CopyTo(resultSpan.Slice(from));
//             from += span1.Length;
// span2.CopyTo(resultSpan.Slice(from));
//             from += span2.Length;
//             span3.CopyTo(resultSpan.Slice(from));

#endif
