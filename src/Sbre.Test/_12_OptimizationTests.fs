[<Xunit.Collection("Sequential")>]
module Sbre.Test._12_OptimizationTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Types
open Xunit
open Common

#if DEBUG

[<Fact>]
let ``calc reverse prefix 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let prefix =
        Optimizations.calcPrefixSets matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("n;i;a;w;T", prefixString)


[<Fact>]
let ``apply prefix 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let prefix =
        Optimizations.calcPrefixSets matcher.Cache matcher.ReversePattern
    let applied = Optimizations.applyPrefixSets matcher.Cache matcher.ReverseTrueStarredPattern prefix
    assertNodeOneOf applied [
        @"(ε|⊤*niawT)"
        @"(⊤*niawT|ε)"
    ]

[<Fact>]
let ``initialOptimizations 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let optimizations = Optimizations.findInitialOptimizations matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    | Optimizations.InitialOptimizations.ReverseSetsPrefix(prefix, transitionNode) ->
        Assert.True(prefix.Length = 5)
        assertNodeOneOf transitionNode [
            @"(ε|⊤*niawT)"
            @"(⊤*niawT|ε)"
        ]
    | _ -> failwith "invalid optimization result"




#endif