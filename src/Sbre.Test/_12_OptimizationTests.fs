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
let ``fixed length 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let prefixLen = Optimizations.getFixedLength matcher.ReversePattern
    Assert.Equal(Some 5, prefixLen)

[<Fact>]
let ``fixed length 2``() =
    let regex = Regex("[a-q][^u-z]{13}x")
    let matcher = regex.TSetMatcher
    let prefixLen = Optimizations.getFixedLength matcher.ReversePattern
    Assert.Equal(Some 15, prefixLen)








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
let ``calc reverse prefix 3``() =
    let regex = Regex(@"⊤*Huck⊤*")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let prefix =
        Optimizations.calcPrefixSets getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    // todo: should be kcuH
    Assert.Equal("k;c;u;H", prefixString)


[<Fact>]
let ``calc reverse prefix 4``() =
    let regex = Regex(@"~(⊤*\n\n⊤*)&⊤*Huck⊤*")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let prefix =
        Optimizations.calcPrefixSets getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("k;c;u;H", prefixString)


[<Fact>]
let ``calc reverse prefix 5``() =
    let regex = Regex(@"~(.*11.*)&[az1]{8,}")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let prefix =
        Optimizations.calcPrefixSets getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("[1az]", prefixString)


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
    | Optimizations.InitialOptimizations.StringPrefix(prefix, transitionNode) ->
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


[<Fact>]
let ``initialOptimizations 3``() =
    let regex = Regex("..g")
    let matcher = regex.TSetMatcher
    let optimizations =
        Optimizations.findInitialOptimizations
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    // | Optimizations.InitialOptimizations.ReverseStringPrefix(prefix,_) ->
    //     Assert.Equal(1,prefix.Length)
    | Optimizations.InitialOptimizations.SetsPrefix(prefix,_) ->
        Assert.Equal(3,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 4``() =
    let regex = Regex("[a-z]shing")
    let matcher = regex.TSetMatcher
    let optimizations =
        Optimizations.findInitialOptimizations
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    | Optimizations.InitialOptimizations.StringPrefix(prefix,_) ->
        Assert.Equal(5,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 5``() =
    let regex = Regex(".*t.*hat.*&.*a.*nd.*&.*t.*he.*&.*w.*as.*")
    let matcher = regex.TSetMatcher
    let optimizations =
        Optimizations.findInitialOptimizations
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    | Optimizations.InitialOptimizations.PotentialStartPrefix(prefix) ->
        Assert.Equal(2,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 6``() =
    let regex = Regex("Huck[a-zA-Z]+|Saw[a-zA-Z]+")
    let matcher = regex.TSetMatcher
    let optimizations =
        Optimizations.findInitialOptimizations
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    | Optimizations.InitialOptimizations.PotentialStartPrefix(prefix) ->
        let prefixString = Optimizations.printPrefixSets matcher.Cache (prefix.ToArray() |> Seq.toList)
        Assert.Equal("[A-Za-z];[kw];[ac];[Su]", prefixString)
    | _ -> failwith "invalid optimization result"




#endif