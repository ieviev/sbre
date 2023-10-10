module Sbre.Benchmarks.PasswordMatching

open System
open System.IO
open System.Text.RuntimeRegexCopy
open BenchmarkDotNet.Attributes
open Sbre

let passwordText =
    __SOURCE_DIRECTORY__ + "/data/hidden-passwords-7kB.txt"
    |> File.ReadAllText

let permutePasswd (words: string list) =
    let permutations =
        String.concat "" [
            for word in words do
                yield $"(?=.{{0,15}}{word})"
        ]
    $"{permutations}.{{8,16}}"

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type PasswordMatching_1() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let inputText = """Lorem Ipsum is simply dummy text of the printing and typesetting industry.
Lorem Ipsum has been the Aa11aBaAA standard dfgdfgr since the 1500s,
when an unknown versions of Lorem Ipsum."""

    let passwordRegex = @".{6,12}&.*[A-Z].*&.*[a-z].*&.*[0-1].*"
    // // let input = "4."
    let matcher = Matcher(passwordRegex)

    // [<Benchmark>] member x.backtracking_lookahead() = r_back_lookahead.Match(passwordText)
    [<Benchmark>] member x.extended_conj_neg() = matcher.MatchText(inputText)

