[<Xunit.Collection("Sequential")>]
module Sbre.Test._11_CountingSetTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Types
open Xunit
open Common

#if DEBUG

let dfaFindMatchEnd (pat:string) (input:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    let cache = matcher.Cache
    failwith "todo"
    let mutable _toplevelOr = matcher.RawPattern
    let mutable loc = Pat.Location.create input 0
    let mutable startState = matcher.GetInitialStateId(&loc).Id
    matcher.DfaEndPosition(&loc, startState, RegexSearchMode.MatchEnd)

let dfaFindAllEnds (pat:string) (input:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    matcher.DfaMatchEnds(input)

let getDfaMatcher (pat:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    let mutable _toplevelOr = matcher.InitialPattern
    matcher


let getMatcher (pat:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    matcher




[<Fact>]
let ``flags 1``() =
    let regex = Regex("a{1,3}b")
    let matcher = regex.TSetMatcher
    let state = RegexState(matcher.Cache.NumOfMinterms())
    let d1 = getNodeDerivative(regex, state, matcher.InitialPattern,"a")
    let flags = d1.GetFlags()
    Assert.Equal(RegexNodeFlags.HasCounterFlag, flags)


[<Fact>]
let ``csa der 01`` () =
    let matcher = getDfaMatcher "a{1,3}b"
    let w  =1
    Assert.Equal(4, 2)



[<Fact>]
let ``csa 01`` () =
    let endPos = dfaFindMatchEnd "a{1,3}b" "baaabc"
    Assert.Equal(4, endPos)





// T*.*a{3,7}b



[<Fact>]
let ``counter match 1``() = assertDfaMatchEnds "..a" "_a__" []

[<Fact>]
let ``counter match 2``() = assertDfaMatchEnds "..a" "__a__" [3]

[<Fact>]
let ``counter match 3``() = assertDfaMatchEnds "..a" "__aaa__" [3]

[<Fact>]
let ``counter match 4``() = assertDfaMatchEnds "~(.*\d\d.*)" "__11__" [3; 6]


[<Fact>]
let ``counter match 5``() = assertDfaMatchEnds "~(.*\d\d.*)" "__11__" [3; 6]


[<Fact>]
let ``counter match 6``() = assertDfaReversePos "~(⊤*\d\d⊤*)" "Aa1" 0

[<Fact>]
let ``counter match 7``() = assertDfaMatches "~(⊤*\d\d⊤*)" "Aa11aBaAA" [(0,3); 3,6]





#endif