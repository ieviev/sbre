[<Xunit.Collection("Sequential")>]
module Sbre.Test._11_CountingSetTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Types
open Xunit

#if DEBUG

let dfaFindMatchEnd (pat:string) (input:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    let cache = matcher.Cache
    let mutable _toplevelOr = matcher.InitialPattern
    let mutable loc = Pat.Location.create input 0
    matcher.DfaEndPosition(cache, &loc, &_toplevelOr)

let dfaFindAllEnds (pat:string) (input:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    matcher.DfaMatchEnds(input)


let getMatcher (pat:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    matcher


[<Fact>]
let ``csa 01`` () =
    let endPos = dfaFindMatchEnd "a{1,3}b" "aaab"
    Assert.Equal(4, endPos)




#endif