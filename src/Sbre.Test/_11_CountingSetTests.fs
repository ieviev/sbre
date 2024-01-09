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
let ``csa der 01`` () =
    let matcher = getDfaMatcher "a{1,3}b"
    let w  =1
    Assert.Equal(4, 2)



[<Fact>]
let ``csa 01`` () =
    let endPos = dfaFindMatchEnd "a{1,3}b" "baaabc"
    Assert.Equal(4, endPos)




// T*.*a{3,7}b


#endif