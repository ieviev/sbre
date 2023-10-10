module Sbre.Benchmarks.FullMtwain

open System
open System.IO
open System.Text.RuntimeRegexCopy
open BenchmarkDotNet.Attributes
open Sbre

let paragraph = File.ReadAllText (__SOURCE_DIRECTORY__ + "/data/mtwain-fixed-newlines.txt")

let permuteWithLookaround (words: string list) =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    let prefix = @"\n\n((?!\n\n)[\s\S])*?"
    let suffix = @"((?!\n\n)[\s\S])*?\n\n"
    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                yield (String.concat @"((?!\n\n)[\s\S])*?" permutation)
        ]
    $"{prefix}({altpermutations}){suffix}"


let permuteWithLoop (words: string list) =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    let prefix = @"\n\n((.+\n)+?"
    let suffix = @"(.+\n)+?)\n"
    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                yield
                    permutation
                    |> List.map (fun v -> $".*{v}.*")
                    |> String.concat @"(.+\n)+?"
        ]
    $"{prefix}({altpermutations}){suffix}"


let permuteAlt (words: string list) =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    let prefix = @"[\s\S]*"
    let suffix = @"[\s\S]*"
    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                yield (String.concat @"[\s\S]*" permutation)
        ]
    $"{prefix}({altpermutations}){suffix}"


let permuteConj (words: string list) =
    let prefix = @"\n\n~(⊤*\n\n⊤*)\n"
    let permutations =
        String.concat "&" [
            for word in words do
                yield $"⊤*{word}⊤*"
        ]
    $"{prefix}&{permutations}"

let words = ["King";"Paris";"English";"would";"rise";"struck"; "council"; "march"; "war"; "May"; "Orleans"; "work"]


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_1() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_lookaround = permuteWithLookaround words[..0]
    let pat_loop = permuteWithLoop words[..0]
    let pat_conj_neg = permuteConj words[..0]
    let r_back_lookahead = System.Text.RegularExpressions.Regex(
        pat_lookaround,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_back_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_nonback_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_SBRE = Matcher(pat_conj_neg)

    [<Benchmark>] member x.backtracking_loop() = r_back_loop.Match(paragraph)
    [<Benchmark>] member x.backtracking_lookahead() = r_back_lookahead.Match(paragraph)
    [<Benchmark>] member x.nonbacktracking_loop() = r_nonback_loop.Match(paragraph)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_2() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_lookaround = permuteWithLookaround words[..1]
    let pat_loop = permuteWithLoop words[..1]
    let pat_conj_neg = permuteConj words[..1]

    let r_back_lookahead = System.Text.RegularExpressions.Regex(
        pat_lookaround,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(10_000.))
    let r_back_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(10_000.))
    let r_nonback_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
        matchTimeout=TimeSpan.FromMilliseconds(10_000.))
    let r_SBRE = Matcher(pat_conj_neg)

    [<Benchmark>] member x.backtracking_loop() = r_back_loop.Match(paragraph)
    [<Benchmark>] member x.backtracking_lookahead() = r_back_lookahead.Match(paragraph)
    [<Benchmark>] member x.nonbacktracking_loop() = r_nonback_loop.Match(paragraph)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)



[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_3() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_lookaround = permuteWithLookaround words[..2]
    let pat_loop = permuteWithLoop words[..2]
    let pat_conj_neg = permuteConj words[..2]

    let r_back_lookahead = System.Text.RegularExpressions.Regex(
        pat_lookaround,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(10_000.))
    let r_back_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(10_000.))
    let r_nonback_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
        matchTimeout=TimeSpan.FromMilliseconds(10_000.))
    let r_SBRE = Matcher(pat_conj_neg)

    // [<Benchmark>] member x.backtracking_loop() = r_back_loop.Match(paragraph)
    // [<Benchmark>] member x.backtracking_lookahead() = r_back_lookahead.Match(paragraph)
    // [<Benchmark>] member x.nonbacktracking_loop() = r_nonback_loop.Match(paragraph)

    // [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)
    [<Benchmark>] member x.SBRE() = r_SBRE.FindMatchEnd(paragraph)


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_4() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_lookaround = permuteWithLookaround words[..3]
    let pat_loop = permuteWithLoop words[..3]
    let pat_conj_neg = permuteConj words[..3]
    let r_back_lookahead = System.Text.RegularExpressions.Regex(
        pat_lookaround,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_back_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_nonback_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_SBRE = Matcher(pat_conj_neg)

    // [<Benchmark>] member x.backtracking_loop() = r_back_loop.Match(paragraph)
    // [<Benchmark>] member x.backtracking_lookahead() = r_back_lookahead.Match(paragraph)
    // [<Benchmark>] member x.nonbacktracking_loop() = r_nonback_loop.Match(paragraph)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_5() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_lookaround = permuteWithLookaround words[..4]
    let pat_loop = permuteWithLoop words[..4]
    let pat_conj_neg = permuteConj words[..4]

    let r_back_lookahead = System.Text.RegularExpressions.Regex(
        pat_lookaround,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_back_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_nonback_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_SBRE = Matcher(pat_conj_neg)

    // [<Benchmark>] member x.backtracking_loop() = r_back_loop.Match(paragraph)
    // [<Benchmark>] member x.backtracking_lookahead() = r_back_lookahead.Match(paragraph)
    // [<Benchmark>] member x.nonbacktracking_loop() = r_nonback_loop.Match(paragraph)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_6() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_lookaround = permuteWithLookaround words[..5]
    let pat_loop = permuteWithLoop words[..5]
    let pat_conj_neg = permuteConj words[..5]
    let r_nonback_loop = System.Text.RegularExpressions.Regex(
        pat_loop,
        options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_SBRE = Matcher(pat_conj_neg)

    // [<Benchmark>] member x.nonbacktracking_loop() = r_nonback_loop.Match(paragraph)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)



[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_7() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_lookaround = permuteWithLookaround words[..6]
    let pat_loop = permuteWithLoop words[..6]
    let pat_conj_neg = permuteConj words[..6]

    // wont even compile in this time
    // let r_nonback_loop = System.Text.RegularExpressions.Regex(
    //     pat_loop,
    //     options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
    //     matchTimeout=TimeSpan.FromMilliseconds(500_000.))
    let r_SBRE = Matcher(pat_conj_neg)

    // [<Benchmark>] member x.nonbacktracking_loop() = r_nonback_loop.Match(paragraph)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_8() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_lookaround = permuteWithLookaround words[..7]
    let pat_loop = permuteWithLoop words[..7]
    let pat_conj_neg = permuteConj words[..7]

    // let r_nonback_loop = System.Text.RegularExpressions.Regex(
    //     pat_loop,
    //     options=System.Text.RegularExpressions.RegexOptions.NonBacktracking,
    //     matchTimeout=TimeSpan.FromMilliseconds(30_000.))
    let r_SBRE = Matcher(pat_conj_neg)

    // [<Benchmark>] member x.nonbacktracking_loop() = r_nonback_loop.Match(paragraph)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_9() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_lookaround = permuteWithLookaround words[..8]
    let pat_conj_neg = permuteConj words[..8]
    let r_back_lookahead = System.Text.RegularExpressions.Regex(
        pat_lookaround,
        options=System.Text.RegularExpressions.RegexOptions.None,
        matchTimeout=TimeSpan.FromMilliseconds(30_000.))

    let r_SBRE = Matcher(pat_conj_neg)

    [<Benchmark>] member x.backtracking_lookahead() = r_back_lookahead.Match(paragraph)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_10() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_conj_neg = permuteConj words[..9]
    let r_SBRE = Matcher(pat_conj_neg)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_11() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_conj_neg = permuteConj words[..10]
    let r_SBRE = Matcher(pat_conj_neg)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type FullMtwain_12() =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)

    let pat_conj_neg = permuteConj words[..11]
    let r_SBRE = Matcher(pat_conj_neg)
    [<Benchmark>] member x.SBRE() = r_SBRE.MatchText(paragraph)

