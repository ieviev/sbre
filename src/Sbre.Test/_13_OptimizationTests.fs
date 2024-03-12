[<Xunit.Collection("Sequential")>]
module Sbre.Test._13_OptimizationTests

open System.Buffers
open System.Runtime.Intrinsics.X86
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
    assertOptimizationPrefixSets "⊤*A⊤*&⊤*B" "B"

[<Fact>]
let ``calc reverse prefix 3``() =
    assertOptimizationPrefixSets @"⊤*Huck⊤*" "k;c;u;H"


[<Fact>]
let ``calc reverse prefix 4``() =
    assertOptimizationPrefixSets @"~(⊤*\n\n⊤*)&⊤*Huck⊤*" "k;c;u;H"


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
let ``initialOptimizations 01``() =
    let optimizations = getInitOptimizations "Twain"
    match optimizations with
    | Optimizations.InitialOptimizations.StringPrefix(prefix, transitionNode) ->
        Assert.True(prefix.Length = 5)
    | _ -> failwith "invalid optimization result"

[<Fact>]
let ``initialOptimizations 02``() =
    assertPrefixLength "Tom|Sawyer|Huckleberry|Finn" 3



[<Fact>]
let ``initialOptimizations 03``() =
    assertPrefixLength "..g" 3


[<Fact>]
let ``initialOptimizations 04``() =
    let optimizations = getInitOptimizations "[a-z]shing"
    match optimizations with
    | Optimizations.InitialOptimizations.StringPrefix(prefix,_) ->
        Assert.Equal(5,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 05``() =
    assertPrefixLength ".*t.*hat.*&.*a.*nd.*&.*t.*he.*&.*w.*as.*" 4


[<Fact>]
let ``initialOptimizations 06``() =
    assertPotentialStart "Huck[a-zA-Z]+|Saw[a-zA-Z]+" "[A-Za-z];[kw];[ac];[Su]"

[<Fact>]
let ``initialOptimizations 07``() =
    assertPotentialStart "Tom|Sawyer|Huckleberry|Finn" "[mnry];[enor];[Tiry]"

// Twain
// (?i)Twain

[<Fact>]
let ``initialOptimizations 08``() =
    assertPotentialStart "\s([A-Za-z]awyer|[A-Za-z]inn)\s" "\s;[nr];[en];[iy];[A-Za-z];φ"


[<Fact>]
let ``initialOptimizations 09``() =
    assertSetsPrefix @"\b\w+nn\b" @"\W;n;n;φ"

[<Fact>]
let ``initialOptimizations 10``() =
    assertSetsPrefix @"(?<=\W)hello(?=\W)" @"\W;o;l;l;e;h;\W"

[<Fact>]
let ``initialOptimizations 11``() =
    assertPotentialStart
        @"@( |)G( |)R( |)[a,A,@,(\/\\))]" @"[(),/@A\\a];[ R];[ GR];[ @G]"


[<Fact>]
let ``initialOptimizations 12``() =
    assertPotentialStart @"a( |)b( |)c( |)d" @"d;[ c];[ bc];[ ab]"

[<Fact>]
let ``initialOptimizations 13``() =
    assertStringPrefix """.*(?=aaa)""" "aaa"

[<Fact>]
let ``initialOptimizations 14``() =
    assertPotentialStart @"(?<=a.*)(\bx)(?=.*c)" @"c;.;.;."


[<Fact>]
let ``initialOptimizations 15``() =
    assertPrefixLength @"(?:[A-Z][a-z]+\s*){10,100}" 20

[<Fact>]
let ``initialOptimizations 16``() =
    assertSetsPrefix @"\w+@\w+" @"\w;@;\w"

[<Fact>]
let ``initialOptimizations 17``() =
    assertSetsPrefix @"\w+\s+Holmes\s+\w+" @"φ;φ"


[<Fact>]
let ``initialOptimizations 18``() =
    assertPotentialStart
        @"Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"
        @"[enrsy];[deot];[almrs];[adlrt];[Aaiot];[ HWrs];[ eo];[LMkn];[ ceh];[or];[IJlo]"

// Pr[o]fess[o]r Moriarty
// ___[or]____[ eo]

[<Fact>]
let ``initialOptimizations 19``() =
    assertPotentialStart
        @"(?i)Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"
        @"[ENRSYenrsy];[DEOTdeot];[ALMRSalmrs];[ADLRTadlrt];[AIOTaiot];[ HRSWhrsw];[ EOeo];[K-Nk-n\u212A];[ CEHceh];[ORor];[IJLOijlo]"

[<Fact>]
let ``initialOptimizations 20``() =
    assertPotentialStart
        @"(?:[A-Z][a-z]+\s*){10,100}"
        @"φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ;φ" // ? maybe vector256 to see if all equal


[<Fact>]
let ``initialOptimizations 21``() =
    assertStringPrefix
        @"(?<=Context1~(\T*\n\n\T*))(get, set)"
        @"get, set"


[<Fact>]
let ``initialOptimizations 22``() =
    assertPotentialStart
        @"(?<=( |`|\-|\n|3).*).*&\w.*&.*\w"
        @"φ;." // important to avoid state space blowup

[<Fact>]
let ``initialOptimizations 23``() =
    assertSetsPrefix
        @"(?<=( |`|\-|\n|3).*).*&\w.*&.*\w&\w{4,}"
        @"φ;φ;φ;φ" // important to avoid state space blowup


[<Fact>]
let ``initialOptimizations 24``() =
    assertSetsPrefix
        @"c...&...s"
        @"s;.;.;c" // important to avoid state space blowup





[<Fact>]
let ``activeOptimizations 1``() =
    let regex = Regex("""["'][^"']{0,30}[?!\.]["']""")
    let matcher = regex.TSetMatcher
    let c = matcher.Cache
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let der1 = getder(c.CharToMinterm('"'), matcher.ReverseTrueStarredPattern)
    let der2 = getder(c.CharToMinterm('.'), der1)
    let optimizations =
        Optimizations.tryGetLimitedSkip
            getder
            (fun node -> matcher.GetOrCreateState(node).Flags)
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Startset)
            matcher.Cache matcher.ReverseTrueStarredPattern der2
    match optimizations with
    | Some (Optimizations.ActiveBranchOptimizations.LimitedSkip(distance=n; termPred = termPred)) ->
        // let tp = termPred // Any2CharSearchValues`1, Count = 2, Values = ""'"
        // assertEqual 30 n
        // assertEqual 31 n
        assertTrue (n >= 30)

    | _ -> failwith "invalid optimization result"



// a.*b.*c

// a(b|[^\n])(c|[^\n])

// a_____________________________________________________b______________________________________________c


[<Fact>]
let ``apply prefix 1``() =
    let applied = Common.applyPrefix """Twain"""
    assertNodeOneOf applied [
        @"(ε|⊤*niawT)"
        @"(⊤*niawT|ε)"
        @"(⊤*niawT)?"
    ]





// [<Fact>]
// let ``apply prefix 2``() =
//     let applied = Common.applyPrefix @".*(?=aaa)"
//
//     assertNodeOneOf applied [
//
//         @"((?<=aa)|⊤*(?<=aaa)|ε|(?<=a)).*"
//     ]





#endif