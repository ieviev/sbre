[<Xunit.Collection("Sequential")>]
module Sbre.Test._09_DfaNodeTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Types
open Xunit

#if DEBUG

let getDfaMatcherAndDerivative (pat:string) (input:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    let cache = matcher.Cache
    let mutable _toplevelOr = matcher.TrueStarredPattern
    let mutable loc = Pat.Location.create input 0
    let mutable stateId = matcher.GetOrCreateState(_toplevelOr).Id
    let current = matcher.GetStateAndFlagsById(stateId)
    let success = matcher.TakeTransition(current.Flags, &stateId, &loc)
    let results = matcher.GetStateAndFlagsById(stateId)
    matcher, results


let getMatcher (pat:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<TSet>
    matcher

let assertPatternIn (expectedResults:string list) (state:MatchState<'t>) =
    let node = state.Node
    let nodestr = node.ToString()
    Assert.Contains(nodestr , expectedResults)

// [<Fact>]
// let ``dfa derivative 01`` () =
//     let matcher, (state) = getDfaMatcherAndDerivative "abcd" "abcde"
//     assertPatternIn [ "(bcd|⊤*abcd)"; "(⊤*abcd|bcd)" ] state



#if TODO

[<Fact>]
let ``dfa end 03`` () =
    let endPos =
        dfaFindMatchEnd
            (Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"])
            BenchmarkTests.twain_input[..100_000]
    Assert.Equal(34728, endPos)

[<Fact>]
let ``dfa end 04 - unroll loop`` () =
    let endPos =
        dfaFindMatchEnd
            "[a-q][^u-z]{13}x"
            BenchmarkTests.twain_input[..20000]
    Assert.Equal(11549, endPos)


// [a-q][^u-z]{13}x



[<Fact>]
let ``dfa all ends 01`` () =
    let ends =
        dfaFindAllEnds
            (Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"])
            BenchmarkTests.twain_input[..100_000]
    let expectedEnds = [|34728; 48443; 51765; 56178; 59246|]
    Assert.Equal<int>(expectedEnds, (ends.ToArray()[..4]))


[<Fact>]
let ``dfa all ends 02`` () =
    let ends =
        dfaFindAllEnds
            @"Huck[a-zA-Z]+|Saw[a-zA-Z]+"
            BenchmarkTests.twain_input[15671655..] //[..100_000]
    let expectedEnds = [|40003|]
    Assert.Equal<int>(expectedEnds, (ends.ToArray()[..4]))



[<Fact>]
let ``dfa all ends 03 - blowup`` () =
    let ends =
        dfaFindAllEnds
            "[a-q][^u-z]{13}x"
            BenchmarkTests.twain_input
    let expectedEnds = [|11549; 12956; 14987; 19303; 23996|]
    Assert.Equal<int>(expectedEnds, (ends.ToArray()[..4]))




[<Fact>]
let ``dfa count 01`` () =
    let m = getMatcher @"Huck[a-zA-Z]+|Saw[a-zA-Z]+"
    let c = m.DfaCount BenchmarkTests.twain_input //[..100_000]
    Assert.Equal<int>(1, c)


[<Fact>]
let ``dfa all ends equal 01`` () =
    let matcher = getMatcher (Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"])
    let input = BenchmarkTests.twain_input[..100_000]
    let n1 = matcher.DfaCount(input)
    let n2 = matcher.Count(input)
    Assert.Equal<int>(n2, n1)


[<Fact>]
let ``dfa all ends equal 02`` () =
    let matcher = getMatcher (Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"])
    let input = BenchmarkTests.twain_input[..100_000]
    let n1 = matcher.DfaMatchEnds(input) |> Seq.cast
    let n2 = matcher.MatchPositions(input) |> Seq.map (fun v -> v.Index + v.Length)
    Assert.Equal<int>(n2, n1)

#endif
















#endif