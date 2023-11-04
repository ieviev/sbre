module Sbre.Benchmarks.Jobs

open System
open System.Text.RuntimeRegexCopy
open BenchmarkDotNet.Attributes
open Sbre


module Permutations =

    /// a;b -> a[\s\S]*b|b[\s\S]*a
    let permuteSimpleAlt(words: string list) =
        let rec distribute e =
            function
            | [] -> [ [ e ] ]
            | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

        let rec permute =
            function
            | [] -> [ [] ]
            | e :: xs -> List.collect (distribute e) (permute xs)

        let altpermutations =
            String.concat "|" [
                for permutation in permute words do
                    yield (String.concat @"[\s\S]*" permutation)
            ]

        $"{altpermutations}"

    /// a;b -> ⊤*a⊤*&⊤*b⊤*
    let permuteSimpleConj(words: string list) =
        let permutations =
            String.concat "&" [
                for word in words do
                    yield $"⊤*{word}⊤*"
            ]

        permutations


    let TS = $"⊤*"

    let permuteConjInParagraph(words: string list) =
        let paragraphScope = $@"~({TS}\n\n{TS})\n"

        let permutations =
            String.concat "&" [
                for word in words do
                    yield $"{TS}{word}{TS}"
            ]

        $"{paragraphScope}&{permutations}"


    // combines line loop and alternations to a single regex
    let permuteAltInParagraph(words: string list) =
        let rec distribute e =
            function
            | [] -> [ [ e ] ]
            | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

        let rec permute =
            function
            | [] -> [ [] ]
            | e :: xs -> List.collect (distribute e) (permute xs)

        let prefix = @"(?:.+\n)*?" // standard line loop
        let suffix = @"(?:.+\n)*?\n" // wrong

        let altpermutations =
            String.concat "|" [
                for permutation in permute words do
                    yield
                        permutation |> List.map (fun v -> $".*{v}.*") |> String.concat @"(?:.+\n)*?"
            ]

        $"{prefix}(?:{altpermutations}){suffix}"



[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type OnlyC_None(patterns: string list, input: string) =
    let inputText = input
    // __SOURCE_DIRECTORY__ + "/data/input-text.txt"
    // |> System.IO.File.ReadAllText

    member val EngineNonBack: System.Text.RegularExpressions.Regex = null with get, set
    member val EngineNone: System.Text.RegularExpressions.Regex = null with get, set
    member val EngineCompiled: System.Text.RegularExpressions.Regex = null with get, set
    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.EngineNone <-
            System.Text.RegularExpressions.Regex(
                this.Pattern,
                options = System.Text.RegularExpressions.RegexOptions.None,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

    [<Benchmark>]
    member x.C_None() =
        let matches = x.EngineNone.Matches(inputText)
        matches.Count


[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type OnlyC_NonBacktracking(patterns: string list, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input

    member val EngineNonBack: System.Text.RegularExpressions.Regex = null with get, set
    member val EngineNone: System.Text.RegularExpressions.Regex = null with get, set
    member val EngineCompiled: System.Text.RegularExpressions.Regex = null with get, set
    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.EngineNonBack <-
            System.Text.RegularExpressions.Regex(
                this.Pattern,
                options = System.Text.RegularExpressions.RegexOptions.NonBacktracking,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

    [<Benchmark>]
    member x.C_NonBacktracking() =
        let matches = x.EngineNonBack.Matches(inputText)
        matches.Count


[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
type OnlySbre(patterns: string list, input: string) =
    let inputText = input

    member val EngineSbre: Sbre.Matcher = Unchecked.defaultof<_> with get, set
    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() = this.EngineSbre <- Matcher(this.Pattern)

    [<Benchmark>]
    member x.Sbre() =
        let matches = x.EngineSbre.MatchPositions(inputText)
        matches |> Seq.length
// let matches = x.EngineSbre.Match(inputText)
// matches.Success


[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
type TestLineSplit() =
    let inputText =
        __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText


    [<Benchmark>]
    member x.String_Split() = inputText.Split("\n\n")

    [<Benchmark>]
    member x.String_IndexOf() = inputText.Split("\n\n")



[<MemoryDiagnoser>]
[<ShortRunJob>]
[<AbstractClass>]
type TestAllRuntime(patterns: string list, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input

    member val EngineNonBack: System.Text.RegularExpressions.Regex = null with get, set
    member val EngineNone: System.Text.RegularExpressions.Regex = null with get, set
    member val EngineCompiled: System.Text.RegularExpressions.Regex = null with get, set
    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set


    [<GlobalSetup>]
    member this.Setup() =
        this.EngineNone <-
            System.Text.RegularExpressions.Regex(
                this.Pattern,
                options = System.Text.RegularExpressions.RegexOptions.None,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

        this.EngineCompiled <-
            System.Text.RegularExpressions.Regex(
                this.Pattern,
                options = System.Text.RegularExpressions.RegexOptions.Compiled,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

        this.EngineNonBack <-
            System.Text.RegularExpressions.Regex(
                this.Pattern,
                options = System.Text.RegularExpressions.RegexOptions.NonBacktracking,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

    [<Benchmark>]
    member x.C_None() =
        let matches = x.EngineNone.Matches(inputText)
        matches.Count






[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type RuntimeInnerParagraph
    (words: string list, input: string, regexOptions: System.Text.RegularExpressions.RegexOptions) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input

    member val MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] = null with get, set
    member val SingleAlternationRegex: System.Text.RegularExpressions.Regex = null with get, set

    // member this.Patterns : System.Collections.Generic.IEnumerable<string> = patterns
    // [<ParamsSource("Patterns")>]
    // member val Pattern : string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let regexes = [|
            for word in words do
                yield
                    System.Text.RegularExpressions.Regex(
                        word,
                        options = regexOptions,
                        matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                    )
        |]

        this.MultipleIsMatchRegexes <- regexes

        this.SingleAlternationRegex <-
            System.Text.RegularExpressions.Regex(
                Permutations.permuteSimpleAlt (words),
                options = regexOptions,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

    [<Benchmark>]
    member x.MultipleIsMatches() =
        x.MultipleIsMatchRegexes |> Array.forall (fun regex -> regex.IsMatch(inputText))

    [<Benchmark>]
    member x.SingleAlternation() = x.SingleAlternationRegex.IsMatch(inputText)



[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
type SbreInnerParagraph(words: string list, input: string) =
    let inputText = input

    member val MultipleIsMatchRegexes: Matcher[] = null with get, set
    member val SingleAlternationRegex: Matcher = Unchecked.defaultof<_> with get, set
    member val SingleConjunctionRegex: Matcher = Unchecked.defaultof<_> with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.MultipleIsMatchRegexes <- [|
            for word in words do
                yield Matcher(word)
        |]

        this.SingleAlternationRegex <- Matcher(Permutations.permuteSimpleAlt words)
        this.SingleConjunctionRegex <- Matcher(Permutations.permuteSimpleConj words)

    [<Benchmark>]
    member x.MultipleIsMatches() =
        x.MultipleIsMatchRegexes |> Array.forall (fun regex -> regex.IsMatch(inputText))

    [<Benchmark>]
    member x.SingleAlternation() = x.SingleAlternationRegex.IsMatch(inputText)

    [<Benchmark>]
    member x.SingleConjunction() = x.SingleConjunctionRegex.IsMatch(inputText)





[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type RuntimeFullSearch
    (words: string list, input: string, regexOptions: System.Text.RegularExpressions.RegexOptions) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    let paragraphRegex = @"(?:.+\n)+\n" // absolute fastest paragraph search but skips last one
    // let paragraphRegex = @"(?:.+(?:\n|\z))+(?:\n|\z)" // fastest correct paragraph search
    let singleStepRegex = Permutations.permuteAltInParagraph words


    member val ParagraphSearchRegex: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, regexOptions) with get, set

    member val MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] = null with get, set
    member val SingleStepRegex: System.Text.RegularExpressions.Regex = null with get, set

    // member this.Patterns : System.Collections.Generic.IEnumerable<string> = patterns
    // [<ParamsSource("Patterns")>]
    // member val Pattern : string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let regexes = [|
            for word in words do
                yield
                    System.Text.RegularExpressions.Regex(
                        word,
                        options = regexOptions,
                        matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                    )
        |]

        this.MultipleIsMatchRegexes <- regexes

        this.SingleStepRegex <-
            System.Text.RegularExpressions.Regex(
                singleStepRegex,
                options = regexOptions,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

    [<Benchmark>]
    member this.TwoStepSearch() =
        let results = ResizeArray()
        let inputSpan = inputText.AsSpan()

        let mutable entireParagraphIsMatch = true
        let mutable e = this.ParagraphSearchRegex.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            entireParagraphIsMatch <- true

            let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
            // run multiple ismatch regexes on each paragraph
            for reg in this.MultipleIsMatchRegexes do
                if not (reg.IsMatch(paragraphSpan)) then
                    entireParagraphIsMatch <- false

            if entireParagraphIsMatch then
                results.Add(struct (e.Current.Index, e.Current.Length))


        results

    // [<Benchmark>] // single regex with line loop and alternations
    // member this.SingleRegexSearch() =
    //     let result = this.SingleStepRegex.Matches(inputText)
    //     result.Count



// [<MemoryDiagnoser(false)>]
// [<ShortRunJob>]
// [<AbstractClass>]
// [<HideColumns([| "" |])>]
// type RuntimeSingleStepSearch
//     (words: string list, input: string, regexOptions: System.Text.RegularExpressions.RegexOptions) =
//     do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
//     let inputText = input
//     let singleStepRegex = Permutations.permuteAltInParagraph words
//
//     member val SearchRegex: System.Text.RegularExpressions.Regex =
//         System.Text.RegularExpressions.Regex(paragraphRegex, regexOptions) with get, set
//
//     member val MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] = null with get, set
//     member val SingleAlternationRegex: System.Text.RegularExpressions.Regex = null with get, set
//
//     // member this.Patterns : System.Collections.Generic.IEnumerable<string> = patterns
//     // [<ParamsSource("Patterns")>]
//     // member val Pattern : string = "" with get, set
//
//     [<GlobalSetup>]
//     member this.Setup() =
//         let regexes = [|
//             for word in words do
//                 yield
//                     System.Text.RegularExpressions.Regex(
//                         word,
//                         options = regexOptions,
//                         matchTimeout = TimeSpan.FromMilliseconds(10_000.)
//                     )
//         |]
//
//         this.MultipleIsMatchRegexes <- regexes
//
//         this.SingleAlternationRegex <-
//             System.Text.RegularExpressions.Regex(
//                 Permutations.permuteSimpleAlt (words),
//                 options = regexOptions,
//                 matchTimeout = TimeSpan.FromMilliseconds(10_000.)
//             )
//
//     [<Benchmark>]
//     member this.MultipleIsMatches() =
//         let mutable e = this.SearchRegex.EnumerateMatches(inputText)
//         let results = ResizeArray()
//
//         while e.MoveNext() do
//             let currentString = inputText[e.Current.Index .. e.Current.Index + e.Current.Length]
//
//             match
//                 this.MultipleIsMatchRegexes
//                 |> Array.forall (fun regex -> regex.IsMatch(currentString))
//             with
//             | true -> results.Add(struct (e.Current.Index, e.Current.Length))
//             | false -> ()
//
//         results


[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
type SbreCombinedSearch(words: string list, input: string) =
    let inputText = input
    let combinedRegex = Permutations.permuteConjInParagraph words

    member val CombinedRegex: Matcher = Unchecked.defaultof<_> with get, set

    [<GlobalSetup>]
    member this.Setup() = this.CombinedRegex <- Matcher(combinedRegex)


    [<Benchmark>]
    member this.MatchWithConj() =
        this.CombinedRegex.MatchPositions(inputText) |> Seq.toArray



[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
type SbreDebugSearch(patterns: string list, input: string) =
    let inputText = input


    member val CombinedRegex: Matcher = Unchecked.defaultof<_> with get, set

    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let combinedRegex = this.Pattern
        this.CombinedRegex <- Matcher(combinedRegex)

    [<Benchmark>]
    member this.MatchWithConj() =
        // this.CombinedRegex.MatchPositions(inputText) |> Seq.toArray
        this.CombinedRegex.CountMatches(inputText)





[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type AllRegexesInParagraph(regexes: string list, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    // let paragraphRegex = @"(?:.+\n)+\n" // absolute fastest paragraph search but skips last one
    let paragraphRegex = @"(?:.+(?:\n|\z))+(?:\n|\z)" // fastest correct paragraph search
    let singleStepRegex = Permutations.permuteAltInParagraph regexes
    let conjunctionRegex = Permutations.permuteConjInParagraph regexes
    let opts_None = Text.RegularExpressions.RegexOptions.None
    let opts_NonBacktracking = Text.RegularExpressions.RegexOptions.NonBacktracking
    let opts_Compiled = Text.RegularExpressions.RegexOptions.Compiled

    member val ConjunctionRegex: Matcher = Unchecked.defaultof<_> with get, set
    member val None_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_None) with get, set

    member val None_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set

    member val None_SingleStepRegex: System.Text.RegularExpressions.Regex = null with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.None_MultipleIsMatchRegexes <- [|
            for word in regexes do
                yield
                    System.Text.RegularExpressions.Regex(
                        word,
                        options = opts_None,
                        matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                    )
        |]

        this.None_SingleStepRegex <-
            System.Text.RegularExpressions.Regex(
                singleStepRegex,
                options = opts_None,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )
        this.ConjunctionRegex <- Matcher(conjunctionRegex)

        stdout.WriteLine "REGEXES:"
        stdout.WriteLine $"{conjunctionRegex}"
        stdout.WriteLine $"{singleStepRegex}"



    [<Benchmark>]
    member this.None_TwoStep() =
        let results = ResizeArray()
        let inputSpan = inputText.AsSpan()

        let mutable entireParagraphIsMatch = true
        let mutable e = this.None_Paragraph.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            entireParagraphIsMatch <- true
            let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
            // run multiple ismatch regexes on each paragraph
            for reg in this.None_MultipleIsMatchRegexes do
                if not (reg.IsMatch(paragraphSpan)) then
                    entireParagraphIsMatch <- false

            if entireParagraphIsMatch then
                results.Add(struct (e.Current.Index, e.Current.Length))


        results

    // [<Benchmark>] // single regex with line loop and alternations
    // member this.None_SingleRegex() =
    //     let result = this.None_SingleStepRegex.Matches(inputText)
    //     result.Count

    [<Benchmark>]
    member this.Sbre_Conjunction() =
        // this.ConjunctionRegex.MatchPositions(inputText) |> Seq.toArray
        this.ConjunctionRegex.CountMatches(inputText)