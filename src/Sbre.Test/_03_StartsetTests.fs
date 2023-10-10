[<Xunit.Collection("Sequential")>]
module Sbre.Test._03_StartsetTests

#if DEBUG

open System
open System.Collections
open System.Collections.Generic
open System.Globalization
open System.Text.RegularExpressions
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre
open Sbre.Test
open Xunit

module Helpers =
    let charSetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()
    let bddBuilder = SymbolicRegexBuilder<BDD>(charSetSolver,charSetSolver)


[<Fact>]
let ``startset generation 1`` () =
    let matcher = Matcher(@"⊤*Huck⊤*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("H", ss1pretty)


[<Fact>]
let ``startset generation 2`` () =

    let matcher = Matcher(@"⊤*(Huck|Finn)⊤*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("[FH]", ss1pretty)



[<Fact>]
let ``startset generation 3`` () =

    let matcher = Matcher(@"\n\n~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*&⊤*Paris⊤*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[\nEKP]", ss1pretty)



[<Fact>]
let ``startset generation 4`` () =
    let matcher = Matcher(@"(b|)*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[]", ss1pretty)


[<Fact>]
let ``startset generation 5`` () =
    let matcher = Matcher(@"(a|ab)*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@".", ss1pretty)

[<Fact>]
let ``reverse startset generation 1`` () =
    let matcher = Matcher(@"\n\n~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*&⊤*Paris⊤*")
    let c = matcher.Cache
    let patstr = matcher.ReversePattern.ToStringHelper()
    let ss1 = Info.Startset.inferStartset (c.Solver) (matcher.ReversePattern)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[\nghs]", ss1pretty)

[<Fact>]
let ``startset2 generation 1`` () =

    let matcher = Matcher(@"⊤*English⊤*&⊤*King⊤*")
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset2 (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("[in]", ss1pretty)


//TODO:
// [<Fact>]
// let ``startset2 generation 2`` () =
//
//     let matcher = Matcher(@"⊤*(Finn|Huck)⊤*")
//     let c = matcher.Cache
//     let ss1 = Info.Startset.inferStartset2 (c.Solver) (c.InitialPatternWithoutDotstar)
//     let ss1pretty = c.PrettyPrintMinterm(ss1)
//     Assert.Equal("[in]", ss1pretty)



#endif