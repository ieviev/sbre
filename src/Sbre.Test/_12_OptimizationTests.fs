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
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let prefix =
        Optimizations.calcPrefixSets getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("n;i;a;w;T", prefixString)


[<Fact>]
let ``calc reverse prefix 2``() =
    let regex = Regex("⊤*A⊤*&⊤*B")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let prefix =
        Optimizations.calcPrefixSets getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("B", prefixString)

[<Fact>]
let ``calc potential start 1``() =
    let regex = Regex("Tom|Sawyer|Huckleberry|Finn")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let prefix =
        Optimizations.calcPotentialMatchStart getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("[mnry];[enor];[Tiry]", prefixString)


[<Fact>]
let ``apply prefix 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let prefix =
        Optimizations.calcPrefixSets getflags matcher.Cache matcher.ReversePattern
    let applied = Optimizations.applyPrefixSets matcher.Cache matcher.ReverseTrueStarredPattern prefix
    assertNodeOneOf applied [
        @"(ε|⊤*niawT)"
        @"(⊤*niawT|ε)"
    ]

[<Fact>]
let ``initialOptimizations 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let optimizations =
        Optimizations.findInitialOptimizations
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    | Optimizations.InitialOptimizations.ReverseSetsPrefix(prefix, transitionNode) ->
        Assert.True(prefix.Length = 5)
    | _ -> failwith "invalid optimization result"

[<Fact>]
let ``initialOptimizations 2``() =
    let regex = Regex("Tom|Sawyer|Huckleberry|Finn")
    let matcher = regex.TSetMatcher
    let optimizations =
        Optimizations.findInitialOptimizations
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    | Optimizations.InitialOptimizations.PotentialStartPrefix(prefix) ->
        Assert.True(prefix.Length = 3)
    | _ -> failwith "invalid optimization result"




#endif