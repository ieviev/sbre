[<Xunit.Collection("Sequential")>]
module Sbre.Test._09_DfaTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Types
open Xunit

let dfaFindMatchEnd (pat:string) (input:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<uint64>
    let cache = matcher.Cache
    let initial = matcher.RawPattern
    let mutable _toplevelOr = matcher.Cache.False
    let mutable loc = Pat.Location.create input 0
    matcher.DfaEndPosition(cache, &loc, initial,  &_toplevelOr)

let dfaFindAllEnds (pat:string) (input:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<uint64>
    matcher.DfaMatchEnds(input)


let getMatcher (pat:string) =
    let regex = Regex(pat)
    let matcher = regex.Matcher :?> RegexMatcher<uint64>
    matcher


[<Fact>]
let ``dfa end 01`` () =
    let endPos = dfaFindMatchEnd "abcd" "abcde"
    Assert.Equal(4, endPos)


[<Fact>]
let ``dfa end 02`` () =
    let endPos = dfaFindMatchEnd @".*(?=.*-)&\S.*\S" @"-aaaa-"
    Assert.Equal(5, endPos)


[<Fact>]
let ``dfa end 03`` () =
    let endPos =
        dfaFindMatchEnd
            (Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"])
            BenchmarkTests.twain_input[..100_000]
    Assert.Equal(34728, endPos)

[<Fact>]
let ``dfa all ends 01`` () =
    let ends =
        dfaFindAllEnds
            (Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"])
            BenchmarkTests.twain_input[..100_000]
    let expectedEnds = [|34728; 48443; 51765; 56178; 59246|]
    Assert.Equal<int>(expectedEnds, (ends.ToArray()[..4]))


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


















