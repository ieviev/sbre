[<Xunit.Collection("Sequential")>]
module Sbre.Test._13_OptimizationTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Info
open Sbre.Optimizations
open Sbre.Types
open Xunit
open Common

#if DEBUG


[<Fact>]
let ``fixed length 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let prefixLen = Node.getFixedLength matcher.ReversePattern
    Assert.Equal(Some 5, prefixLen)

[<Fact>]
let ``fixed length 2``() =
    let regex = Regex("[a-q][^u-z]{13}x")
    let matcher = regex.TSetMatcher
    let prefixLen = Node.getFixedLength matcher.ReversePattern
    Assert.Equal(Some 15, prefixLen)

[<Fact>]
let ``fixed length 3``() =
    let regex = Regex("""\b1\b""")
    let matcher = regex.TSetMatcher
    let prefixLen = Node.getFixedLength matcher.ReversePattern
    Assert.Equal(Some 1, prefixLen)





[<Fact>]
let ``calc reverse prefix 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("n;i;a;w;T", prefixString)




[<Fact>]
let ``calc reverse prefix 2``() =
    let regex = Regex("⊤*A⊤*&⊤*B")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("B", prefixString)


[<Fact>]
let ``calc reverse prefix 3``() =
    let regex = Regex(@"⊤*Huck⊤*")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    // todo: should be kcuH
    Assert.Equal("k;c;u;H", prefixString)


[<Fact>]
let ``calc reverse prefix 4``() =
    let regex = Regex(@"~(⊤*\n\n⊤*)&⊤*Huck⊤*")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("k;c;u;H", prefixString)


[<Fact>]
let ``calc reverse prefix 5``() =
    let regex = Regex(@"~(.*11.*)&[az1]{8,}")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("[1az]", prefixString)


[<Fact>]
let ``calc potential start 1``() =
    let regex = Regex("Tom|Sawyer|Huckleberry|Finn")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPotentialMatchStart getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("[mnry];[enor];[Tiry]", prefixString)


[<Fact>]
let ``apply prefix 1``() =
    let applied = Common.applyPrefix "Twain"
    assertNodeOneOf applied [
        @"(ε|⊤*niawT)"
        @"(⊤*niawT|ε)"
    ]

[<Fact>]
let ``initialOptimizations 1``() =
    let optimizations = getInitOptimizations "Twain"
    match optimizations with
    | Optimizations.InitialOptimizations.StringPrefix(prefix, transitionNode) ->
        Assert.True(prefix.Length = 5)
    | _ -> failwith "invalid optimization result"

[<Fact>]
let ``initialOptimizations 2``() =
    let optimizations = getInitOptimizations "Tom|Sawyer|Huckleberry|Finn"
    match optimizations with
    | Optimizations.InitialOptimizations.PotentialStartPrefix(prefix) ->
        Assert.True(prefix.Length = 3)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 3``() =

    let optimizations = getInitOptimizations "..g"
    match optimizations with
    // | Optimizations.InitialOptimizations.ReverseStringPrefix(prefix,_) ->
    //     Assert.Equal(1,prefix.Length)
    | Optimizations.InitialOptimizations.SetsPrefix(prefix,_) ->
        Assert.Equal(3,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 4``() =
    let optimizations = getInitOptimizations "[a-z]shing"
    match optimizations with
    | Optimizations.InitialOptimizations.StringPrefix(prefix,_) ->
        Assert.Equal(5,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 5``() =
    let optimizations = getInitOptimizations ".*t.*hat.*&.*a.*nd.*&.*t.*he.*&.*w.*as.*"
    match optimizations with
    | Optimizations.InitialOptimizations.PotentialStartPrefix(prefix) ->
        Assert.Equal(2,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 6``() =
    assertPotentialPrefix "Huck[a-zA-Z]+|Saw[a-zA-Z]+" "[A-Za-z];[kw];[ac];[Su]"


[<Fact>]
let ``initialOptimizations 7``() =
    assertPotentialPrefix "Tom|Sawyer|Huckleberry|Finn" "[mnry];[enor];[Tiry]"

[<Fact>]
let ``initialOptimizations 8``() =
    assertPotentialPrefix "\s([A-Za-z]awyer|[A-Za-z]inn)\s" "\s;[nr];[en];[iy];[A-Za-z];\s"


[<Fact>]
let ``initialOptimizations 9``() =
    assertPotentialPrefix @"\b\w+nn\b" "\s;[nr];[en];[iy];[A-Za-z];\s"




// [<Fact>]
// let ``activeOptimizations 1``() =
//     let regex = Regex("""["'][^"']{0,30}[?!\.]["']""")
//     let matcher = regex.TSetMatcher
//     let c = matcher.Cache
//     let der1 = Algorithm.createStartsetDerivative(c, c.CharToMinterm('"'), matcher.ReverseTrueStarredPattern)
//     let der2 = Algorithm.createStartsetDerivative(c, c.CharToMinterm('.'), der1)
//     let optimizations =
//         Optimizations.tryGetLimitedSkip
//             (fun node -> matcher.GetOrCreateState(node).Id)
//             (fun node -> matcher.GetOrCreateState(node).Startset)
//             matcher.Cache matcher.ReverseTrueStarredPattern der2
//     match optimizations with
//     | Some (Optimizations.ActiveBranchOptimizations.LimitedSkip(distance=_)) ->
//         ()
//         // let prefixString = Optimizations.printPrefixSets matcher.Cache (prefix.ToArray() |> Seq.toList)
//         // Assert.Equal("\s;[nr];[en];[iy];[A-Za-z];\s", prefixString)
//     | _ -> failwith "invalid optimization result"
//




#endif