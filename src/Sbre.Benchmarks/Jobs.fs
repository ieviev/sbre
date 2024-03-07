module Sbre.Benchmarks.Jobs

open System
// open System.Text.RuntimeRegexCopy
open System.Threading
open BenchmarkDotNet.Attributes
open Sbre
open Sbre.Pat
open Sbre.Types



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
        let paragraphScope = $@"~({TS}\n\n{TS})"

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

    let permuteAltInLine(words: string list) =
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
                    let inner = (String.concat @".*" permutation)
                    yield $".*{inner}.*"
            ]

        $"{altpermutations}"

    let permuteConjInLine(words: string list) =
        words |> List.map (fun v -> $".*{v}.*") |> String.concat "&"

    let permuteConjInLine2(words: string list) =
        words
        |> List.map (fun v -> $"⊤*{v}⊤*")
        |> List.append [".*"]
        |> String.concat "&"

    let permuteLookaheadInLine(words: string list) =
        words |> List.map (fun v -> $"(?=.*{v})") |> String.concat "" |> (fun v -> v + ".*")



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

    member val EngineSbre: Sbre.Regex = Unchecked.defaultof<_> with get, set
    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() = this.EngineSbre <- Regex(this.Pattern)

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

    member val MultipleIsMatchRegexes: Regex[] = null with get, set
    member val SingleAlternationRegex: Regex = Unchecked.defaultof<_> with get, set
    member val SingleConjunctionRegex: Regex = Unchecked.defaultof<_> with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.MultipleIsMatchRegexes <- [|
            for word in words do
                yield Regex(word)
        |]

        this.SingleAlternationRegex <- Regex(Permutations.permuteSimpleAlt words)
        this.SingleConjunctionRegex <- Regex(Permutations.permuteSimpleConj words)

    [<Benchmark>]
    member x.MultipleIsMatches() =
        x.MultipleIsMatchRegexes |> Array.forall (fun regex -> regex.IsMatch(inputText))

    [<Benchmark>]
    member x.SingleAlternation() = x.SingleAlternationRegex.IsMatch(inputText)

    [<Benchmark>]
    member x.SingleConjunction() = x.SingleConjunctionRegex.IsMatch(inputText)




let twoStepSearch (patterns:string list) (input:string) =
    let results = ResizeArray()
    let inputText = input
    let paragraphRegex = @"(?:.+\n)+\n"
    let inputSpan = inputText.AsSpan()
    let opts  = System.Text.RegularExpressions.RegexOptions.None
    let pgregex = System.Text.RegularExpressions.Regex(paragraphRegex, opts)
    let regexes = [|
        for word in patterns do
            yield
                System.Text.RegularExpressions.Regex(
                    word,
                    options = opts,
                    matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                )
    |]

    let mutable entireParagraphIsMatch = true
    let mutable e = pgregex.EnumerateMatches(inputText)

    // enumerate paragraphs during match
    while e.MoveNext() do
        entireParagraphIsMatch <- true
        let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
        // run multiple ismatch regexes on each paragraph
        for reg in regexes do
            if not (reg.IsMatch(paragraphSpan)) then
                entireParagraphIsMatch <- false

        if entireParagraphIsMatch then
            results.Add({ Index = e.Current.Index - 1; Length = e.Current.Length })

    results



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

    member val CombinedRegex: Regex = Unchecked.defaultof<_> with get, set

    [<GlobalSetup>]
    member this.Setup() = this.CombinedRegex <- Regex(combinedRegex)


    [<Benchmark>]
    member this.MatchWithConj() =
        this.CombinedRegex.MatchPositions(inputText) |> Seq.length



[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
type SbreDebugSearch(patterns: string list, input: string) =
    let inputText = input


    member val RegexEngine: Regex = Unchecked.defaultof<_> with get, set
    member val Matcher: RegexMatcher<TSet> = Unchecked.defaultof<_> with get, set

    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let combinedRegex = this.Pattern
        this.RegexEngine <- Regex(combinedRegex)
        this.Matcher <- this.RegexEngine.Matcher :?> RegexMatcher<TSet>

    // [<Benchmark>]
    // member this.NoDfaSbre() =
    //     this.RegexEngine.Count(inputText)

    [<Benchmark>]
    member this.DfaSbre() =
        this.Matcher.Count(inputText)


[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
type Minterms() =
    let input =
        __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText


    member val Matcher: Regex = Unchecked.defaultof<_> with get, set
    // member val Cache: RegexCache<uint64> = Unchecked.defaultof<_> with get, set
    // member val Minterms:  uint64 array = Unchecked.defaultof<_> with get, set
    // member val Classifier: MintermClassifier = Unchecked.defaultof<_> with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.Matcher <- Regex(@"~(⊤*\n\n⊤*)&⊤*Huck⊤*")
        // this.Cache <- this.Matcher.Cache
        // this.Minterms <- this.Matcher.Cache.Minterms()

    // [<Benchmark>]
    // member this.Minterm1() =
    //     let spn = input.AsSpan()
    //     for i = 0 to 100 do
    //         this.Cache.Classify(spn[i]) |> ignore

    //
    // [<Benchmark>]
    // member this.Minterm2() =
    //     let spn = input.AsSpan()
    //     for i = 0 to 100 do
    //         this.Cache.Classify2(spn[i]) |> ignore

    // [<Benchmark>]
    // member this.Elem1() =
    //     let spn = input.AsSpan()
    //     for i = 0 to 100 do
    //         let loc = this.Cache.Classify(spn[i])
    //         Solver.elemOfSet loc 11uL |> ignore

    // [<Benchmark>]
    // member this.Elem2() =
    //     let spn = input.AsSpan()
    //     for i = 0 to 100 do
    //         let loc = this.Cache.Classify(spn[i])
    //         this.Cache.IsValidPredicateUint64(11uL, loc) |> ignore

    // [<Benchmark>]
    // member this.Elem3() =
    //     let spn = input.AsSpan()
    //     for i = 0 to 100 do
    //         let loc = this.Cache.Classify(spn[i])
    //         this.Cache.IsValidPredicate(11uL, loc) |> ignore



[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type AllRegexesInParagraph(regexes: string list, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    let paragraphRegex = @"(?:.+(?:\n|\z))+(?:\n|\z)" // fastest correct paragraph search adds around 2ms
    let conjunctionRegex = Permutations.permuteConjInParagraph regexes
    let opts_None = Text.RegularExpressions.RegexOptions.None
    let opts_NonBacktracking = Text.RegularExpressions.RegexOptions.NonBacktracking
    let opts_Compiled = Text.RegularExpressions.RegexOptions.Compiled

    member val ConjunctionRegex: Regex = Unchecked.defaultof<_> with get, set

    member val None_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_None) with get, set

    member val NonBack_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_NonBacktracking) with get, set

    member val Compiled_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_Compiled) with get, set

    member val None_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set

    member val NonBacktrack_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set

    member val Compiled_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set

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

        this.Compiled_MultipleIsMatchRegexes <- [|
            for word in regexes do
                yield
                    System.Text.RegularExpressions.Regex(
                        word,
                        options = opts_Compiled,
                        matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                    )
        |]

        this.NonBacktrack_MultipleIsMatchRegexes <- [|
            for word in regexes do
                yield
                    System.Text.RegularExpressions.Regex(
                        word,
                        options = opts_NonBacktracking,
                        matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                    )
        |]

        this.ConjunctionRegex <- Regex(conjunctionRegex)


    [<Benchmark>]
    member this.None() =
        let mutable counter = 0
        let inputSpan = inputText.AsSpan()
        let mutable e = this.None_Paragraph.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            let mutable entireParagraphIsMatch = true
            let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
            let mutable i = 0
            // run multiple ismatch regexes on each paragraph
            while
                entireParagraphIsMatch
                && i < this.None_MultipleIsMatchRegexes.Length do
                let reg = this.None_MultipleIsMatchRegexes[i]
                if not (reg.IsMatch(paragraphSpan)) then
                    entireParagraphIsMatch <- false
                i <- i + 1
            if entireParagraphIsMatch then
                counter <- counter + 1
        counter

    [<Benchmark>]
    member this.NonBack() =
        let mutable counter = 0
        let inputSpan = inputText.AsSpan()
        let mutable e = this.NonBack_Paragraph.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            let mutable entireParagraphIsMatch = true
            let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
            let mutable i = 0
            // run multiple ismatch regexes on each paragraph
            while
                entireParagraphIsMatch
                && i < this.NonBacktrack_MultipleIsMatchRegexes.Length do
                let reg = this.NonBacktrack_MultipleIsMatchRegexes[i]
                if not (reg.IsMatch(paragraphSpan)) then
                    entireParagraphIsMatch <- false
                i <- i + 1
            if entireParagraphIsMatch then
                counter <- counter + 1
        counter

    [<Benchmark>]
    member this.Compiled() =
        let mutable counter = 0
        let inputSpan = inputText.AsSpan()
        let mutable e = this.Compiled_Paragraph.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            let mutable entireParagraphIsMatch = true
            let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
            let mutable i = 0
            // run multiple ismatch regexes on each paragraph
            while
                entireParagraphIsMatch
                && i < this.Compiled_MultipleIsMatchRegexes.Length do
                let reg = this.Compiled_MultipleIsMatchRegexes[i]
                if not (reg.IsMatch(paragraphSpan)) then
                    entireParagraphIsMatch <- false
                i <- i + 1
            if entireParagraphIsMatch then
                counter <- counter + 1
        counter

    [<Benchmark>]
    member this.Sbre_Neg_Conj() =
        this.ConjunctionRegex.Count(inputText)



[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type AllRegexesInParagraphSeparate
    (regexesForRuntime: string list, regexForSbre: string, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    // let paragraphRegex = @"(?:.+\n)+\n" // absolute fastest paragraph search but skips last one
    let paragraphRegex = @"(?:.+(?:\n|\z))+(?:\n|\z)" // fastest correct paragraph search adds around 2ms
    let singleStepRegex = Permutations.permuteAltInParagraph regexesForRuntime
    let conjunctionRegex = regexForSbre
    let opts_None = Text.RegularExpressions.RegexOptions.None
    let opts_NonBacktracking = Text.RegularExpressions.RegexOptions.NonBacktracking
    let opts_Compiled = Text.RegularExpressions.RegexOptions.Compiled

    member val ConjunctionRegex: Regex = Unchecked.defaultof<_> with get, set

    member val None_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_None) with get, set

    member val NonBack_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_NonBacktracking) with get, set

    member val Compiled_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_Compiled) with get, set

    member val None_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set

    member val NonBacktrack_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set

    member val Compiled_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set

    member val NonBacktrack_SingleStep: System.Text.RegularExpressions.Regex = null with get, set
    member val Sbre_SingleStepRegex: Regex = Unchecked.defaultof<_> with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.None_MultipleIsMatchRegexes <- [|
            for word in regexesForRuntime do
                yield
                    System.Text.RegularExpressions.Regex(
                        word,
                        options = opts_None,
                        matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                    )
        |]

        this.Compiled_MultipleIsMatchRegexes <- [|
            for word in regexesForRuntime do
                yield
                    System.Text.RegularExpressions.Regex(
                        word,
                        options = opts_Compiled,
                        matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                    )
        |]

        try
            this.NonBacktrack_MultipleIsMatchRegexes <- [|
                for word in regexesForRuntime do
                    yield
                        System.Text.RegularExpressions.Regex(
                            word,
                            options = opts_NonBacktracking,
                            matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                        )
            |]

            this.NonBacktrack_SingleStep <-
                System.Text.RegularExpressions.Regex(
                    singleStepRegex,
                    options = opts_NonBacktracking,
                    matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                )
        with e ->
            ()



        this.ConjunctionRegex <- Regex(conjunctionRegex)


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
                results.Add({ Index = e.Current.Index; Length = e.Current.Length })

        results

    [<Benchmark>]
    member this.NonBack_TwoStep() =
        let results = ResizeArray()
        let inputSpan = inputText.AsSpan()

        let mutable entireParagraphIsMatch = true
        let mutable e = this.NonBack_Paragraph.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            entireParagraphIsMatch <- true
            let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
            // run multiple ismatch regexes on each paragraph
            for reg in this.NonBacktrack_MultipleIsMatchRegexes do
                if not (reg.IsMatch(paragraphSpan)) then
                    entireParagraphIsMatch <- false

            if entireParagraphIsMatch then
                results.Add({ Index = e.Current.Index; Length = e.Current.Length })

        results

    [<Benchmark>]
    member this.Compiled_TwoStep() =
        let results = ResizeArray()
        let inputSpan = inputText.AsSpan()

        let mutable entireParagraphIsMatch = true
        let mutable e = this.Compiled_Paragraph.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            entireParagraphIsMatch <- true
            let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
            // run multiple ismatch regexes on each paragraph
            for reg in this.Compiled_MultipleIsMatchRegexes do
                if not (reg.IsMatch(paragraphSpan)) then
                    entireParagraphIsMatch <- false

            if entireParagraphIsMatch then
                results.Add({ Index = e.Current.Index; Length = e.Current.Length })

        results

    [<Benchmark>] // single regex with line loop and alternations
    member this.NonBack_OneStep() =
        let result = this.NonBacktrack_SingleStep.Matches(inputText)
        result.Count

    // [<Benchmark>]
    // member this.Sbre_SingleStep() =
    //     this.Sbre_SingleStepRegex.MatchPositions(inputText) |> Seq.length

    [<Benchmark>]
    member this.Sbre_Neg_Conj() =
        this.ConjunctionRegex.MatchPositions(inputText) |> Seq.length



[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type MatchInParagraphSeparate(regexForRuntime: string, regexForSbre: string, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    // let paragraphRegex = @"(?:.+\n)+\n" // absolute fastest paragraph search but skips last one
    let paragraphRegex = @"(?:.+(?:\n|\z))+(?:\n|\z)" // fastest correct paragraph search adds around 2ms
    let singleStepRegex = Permutations.permuteAltInParagraph [ regexForRuntime ]
    let conjunctionRegex = regexForSbre
    let opts_None = Text.RegularExpressions.RegexOptions.None
    let opts_NonBacktracking = Text.RegularExpressions.RegexOptions.NonBacktracking
    let opts_Compiled = Text.RegularExpressions.RegexOptions.Compiled

    member val ConjunctionRegex: Regex = Unchecked.defaultof<_> with get, set

    member val None_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_None) with get, set

    member val NonBack_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_NonBacktracking) with get, set

    member val Compiled_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_Compiled) with get, set

    member val None_TwostepRegex: System.Text.RegularExpressions.Regex = null with get, set
    member val NonBack_TwostepRegex: System.Text.RegularExpressions.Regex = null with get, set
    member val Compiled_TwostepRegex: System.Text.RegularExpressions.Regex = null with get, set

    member val NonBacktrack_SingleStep: System.Text.RegularExpressions.Regex = null with get, set
    member val Sbre_SingleStepRegex: Regex = Unchecked.defaultof<_> with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.None_TwostepRegex <-
            System.Text.RegularExpressions.Regex(
                regexForRuntime,
                options = opts_None,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

        this.NonBack_TwostepRegex <-
            System.Text.RegularExpressions.Regex(
                regexForRuntime,
                options = opts_None,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

        this.Compiled_TwostepRegex <-
            System.Text.RegularExpressions.Regex(
                regexForRuntime,
                options = opts_None,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

        this.NonBacktrack_SingleStep <-
            System.Text.RegularExpressions.Regex(
                singleStepRegex,
                options = opts_NonBacktracking,
                matchTimeout = TimeSpan.FromMilliseconds(10_000.)
            )

        this.ConjunctionRegex <- Regex(conjunctionRegex)


    [<Benchmark>]
    member this.None_TwoStep() =
        let results = ResizeArray()
        let mutable entireParagraphIsMatch = true
        let mutable e = this.None_Paragraph.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            entireParagraphIsMatch <- true

            let mutable result =
                this.None_TwostepRegex.Match(inputText, e.Current.Index, e.Current.Length)

            if result.Success then
                results.Add({ Index = result.Index; Length = result.Length })

        results

    [<Benchmark>]
    member this.NonBack_TwoStep() =
        let results = ResizeArray()
        let mutable entireParagraphIsMatch = true
        let mutable e = this.NonBack_Paragraph.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            entireParagraphIsMatch <- true

            let mutable result =
                this.NonBack_TwostepRegex.Match(inputText, e.Current.Index, e.Current.Length)

            if result.Success then
                results.Add({ Index = result.Index; Length = result.Length })

        results

    [<Benchmark>]
    member this.Compiled_TwoStep() =
        let results = ResizeArray()
        let mutable entireParagraphIsMatch = true
        let mutable e = this.Compiled_Paragraph.EnumerateMatches(inputText)

        // enumerate paragraphs during match
        while e.MoveNext() do
            entireParagraphIsMatch <- true

            let mutable result =
                this.Compiled_TwostepRegex.Match(inputText, e.Current.Index, e.Current.Length)

            if result.Success then
                results.Add({ Index = result.Index; Length = result.Length })

        results

    [<Benchmark>] // single regex with line loop and alternations
    member this.NonBack_OneStep() =
        let result = this.NonBacktrack_SingleStep.Matches(inputText)
        result.Count

    [<Benchmark>]
    member this.Sbre_Neg_Conj() =
        this.ConjunctionRegex.MatchPositions(inputText) |> Seq.length




[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type TestAllEngines(pattern: string, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    let opts_None =
        Text.RegularExpressions.RegexOptions.None
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_NonBacktracking =
        Text.RegularExpressions.RegexOptions.NonBacktracking
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_Compiled =
        Text.RegularExpressions.RegexOptions.Compiled
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture

    member val None_Regex: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(pattern, opts_None) with get, set

    member val NonBack_Regex: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(pattern, opts_NonBacktracking) with get, set

    member val Compiled_Regex: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(pattern, opts_Compiled, matchTimeout = TimeSpan.FromMilliseconds(2_000.)) with get, set

    member val Sbre_Regex: Regex = Regex(pattern) with get, set


    [<GlobalSetup>]
    member this.Setup() = ()


    [<Benchmark(Description = "NonBacktrack")>]
    member this.Symbolic() =
        this.NonBack_Regex.Count(inputText)
        // let result = this.NonBack_Regex.Matches(inputText)
        // result.Count

    //  181 KB

    [<Benchmark(Description = "Compiled")>]
    member this.Compiled() =
        this.Compiled_Regex.Count(inputText)
        // let result = this.Compiled_Regex.Matches(inputText)
        // result.Count

    // [<Benchmark(Description = "None")>]
    // member this.Default() =
    //     this.None_Regex.Count(inputText)


    [<Benchmark(Description = "Sbre")>]
    member this.Sbre() =
        this.Sbre_Regex.Count(inputText)



[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type TestAllEnginesAllPatternsMatchOnly(patterns: string list, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    let opts_None =
        Text.RegularExpressions.RegexOptions.None
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_NonBacktracking =
        Text.RegularExpressions.RegexOptions.NonBacktracking
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_Compiled =
        Text.RegularExpressions.RegexOptions.Compiled
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture


    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns

    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set


    member val None_Regex: System.Text.RegularExpressions.Regex = Unchecked.defaultof<_> with get, set

    member val NonBack_Regex: System.Text.RegularExpressions.Regex =
        Unchecked.defaultof<_> with get, set

    member val Compiled_Regex: System.Text.RegularExpressions.Regex =
        Unchecked.defaultof<_> with get, set


    member val Sbre_Regex: Regex = Unchecked.defaultof<_>  with get, set


    [<GlobalSetup>]
    member this.Setup() =
        this.None_Regex <- System.Text.RegularExpressions.Regex(this.Pattern, opts_None)
        // this.NonBack_Regex <- System.Text.RegularExpressions.Regex(this.Pattern, opts_NonBacktracking)
        this.Compiled_Regex <- System.Text.RegularExpressions.Regex(this.Pattern, opts_Compiled)
        this.Sbre_Regex <- Regex(this.Pattern)


    // [<Benchmark(Description = "NonBacktrack")>]
    // member this.Symbolic() =
    //     this.NonBack_Regex.Count(inputText)
    //
    // [<Benchmark(Description = "Compiled")>]
    // member this.Compiled() =
    //     this.Compiled_Regex.Count(inputText)

    // [<Benchmark(Description = "Sbre")>]
    // member this.Sbre() =
    //     this.Sbre_Regex.Count(inputText)

    //
    // [<Benchmark(Description = "None")>]
    // member this.None() =
    //     this.None_Regex.Count(inputText)

    // [<Benchmark(Description = "Sbre")>]
    // member this.Sbre() =
    //     this.Sbre_Regex.Count(inputText)

    //
    [<Benchmark()>]
    member this.LastIndexOfOrdinalIgnoreCase() =
        let span = inputText.AsSpan()
        let mutable looping = true
        let mutable count = 0
        let mutable currPos = inputText.Length - 1
        while looping do
            let slice = span.Slice(0,currPos)
            match slice.LastIndexOf("Sherlock Holmes", StringComparison.OrdinalIgnoreCase) with
            | -1 -> looping <- false
            | n ->
                count <- count + 1
                currPos <- n
        // if count <> 522 then
        //     failwith $"invalid count: {count}" |> ignore

    [<Benchmark()>]
    member this.IndexOfOrdinalIgnoreCase() =
        let span = inputText.AsSpan()
        let mutable looping = true
        let mutable count = 0
        let mutable currPos = 0
        let textLength = "Sherlock Holmes".Length
        while looping do
            let slice = span.Slice(currPos)
            match slice.IndexOf("Sherlock Holmes", StringComparison.OrdinalIgnoreCase) with
            | -1 -> looping <- false
            | n ->
                count <- count + 1
                currPos <- currPos + n + textLength
        // if count <> 522 then
        //     failwith $"invalid count: {count}" |> ignore

    [<Benchmark()>]
    member this.LastIndexOfOrdinal() =
        let span = inputText.AsSpan()
        let mutable looping = true
        let mutable count = 0
        let mutable currPos = inputText.Length - 1
        while looping do
            let slice = span.Slice(0,currPos)
            match slice.LastIndexOf("Sherlock Holmes", StringComparison.Ordinal) with
            | -1 -> looping <- false
            | n ->
                count <- count + 1
                currPos <- n
        // if count <> 513 then
        //     failwith $"{count}" |> ignore



    [<Benchmark()>]
    member this.IndexOfOrdinal() =
        let span = inputText.AsSpan()
        let mutable looping = true
        let mutable count = 0
        let mutable currPos = 0
        let textLength = "Sherlock Holmes".Length
        while looping do
            let slice = span.Slice(currPos)
            match slice.IndexOf("Sherlock Holmes", StringComparison.Ordinal) with
            | -1 -> looping <- false
            | n ->
                count <- count + 1
                currPos <- currPos + n + textLength
        // if count <> 513 then
        //     failwith $"{count}" |> ignore





[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type TestAllEnginesAllPatternsParagraphSeparate(words: (string list), input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    let opts_None =
        Text.RegularExpressions.RegexOptions.None
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_NonBacktracking =
        Text.RegularExpressions.RegexOptions.NonBacktracking
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_Compiled =
        Text.RegularExpressions.RegexOptions.Compiled
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture

    let paragraphRegex = @"(?:.+(?:\n|\z))+(?:\n|\z)" // fastest correct paragraph search adds around 2ms

    let searchParagraphs (paragraphEngine:System.Text.RegularExpressions.Regex) (innerEngines:System.Text.RegularExpressions.Regex[]) =
        let mutable counter = 0
        let inputSpan = inputText.AsSpan()
        let mutable e = paragraphEngine.EnumerateMatches(inputText)
        // enumerate paragraphs during match
        while e.MoveNext() do
            let mutable entireParagraphIsMatch = true
            let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
            let mutable i = 0
            // run multiple ismatch regexes on each paragraph
            while
                i < innerEngines.Length do
                let reg = innerEngines[i]
                if not (reg.IsMatch(paragraphSpan)) then
                    entireParagraphIsMatch <- false
                i <- i + 1
            if entireParagraphIsMatch then
                counter <- counter + 1
        counter

    member val ConjunctionRegex: Regex = Unchecked.defaultof<_> with get, set

    member val None_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_None) with get, set

    member val NonBack_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_NonBacktracking) with get, set

    member val Compiled_Paragraph: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(paragraphRegex, opts_Compiled) with get, set

    member val None_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set

    member val NonBacktrack_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set

    member val Compiled_MultipleIsMatchRegexes: System.Text.RegularExpressions.Regex[] =
        null with get, set


    [<Params(1,2,3,4, 5)>]
    member val NumOfWords: int = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.Compiled_MultipleIsMatchRegexes <- [|
            for word in words[0.. this.NumOfWords - 1] do
                yield
                    System.Text.RegularExpressions.Regex(
                        word,
                        options = opts_Compiled,
                        matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                    )
        |]

        this.NonBacktrack_MultipleIsMatchRegexes <- [|
            for word in words[0.. this.NumOfWords - 1] do
                yield
                    System.Text.RegularExpressions.Regex(
                        word,
                        options = opts_NonBacktracking,
                        matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                    )
        |]
        let conjunctionRegex = Permutations.permuteConjInParagraph words[0.. this.NumOfWords - 1]
        this.ConjunctionRegex <- Regex(conjunctionRegex)

    [<Benchmark(Description="NonBacktrack")>]
    member this.Symbolic() =
        searchParagraphs this.NonBack_Paragraph this.NonBacktrack_MultipleIsMatchRegexes

    [<Benchmark(Description="Compiled")>]
    member this.Compiled() =
        searchParagraphs this.Compiled_Paragraph this.Compiled_MultipleIsMatchRegexes

    [<Benchmark(Description = "Sbre")>]
    member this.Sbre() =
        this.ConjunctionRegex.Count(inputText)

[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type TestAllEnginesAllPatternsSeparate(patterns: (string*string) list, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    let opts_None =
        Text.RegularExpressions.RegexOptions.None
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_NonBacktracking =
        Text.RegularExpressions.RegexOptions.NonBacktracking
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_Compiled =
        Text.RegularExpressions.RegexOptions.Compiled
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture


    member val Counts: int list = (patterns |> List.indexed |> List.map (fun v -> fst v + 1)) with get, set

    // [<Params(1,2,3,4,5,6,7)>]
    [<ParamsSource("Counts")>]
    member val NumOfWords: int = 0 with get, set

    member val NonBack_Regex: System.Text.RegularExpressions.Regex =
        Unchecked.defaultof<_> with get, set

    member val Compiled_Regex: System.Text.RegularExpressions.Regex =
        Unchecked.defaultof<_> with get, set

    member val Sbre_Alt_Regex: Regex = Unchecked.defaultof<_>  with get, set
    member val Sbre_Regex: Regex = Unchecked.defaultof<_>  with get, set


    [<GlobalSetup>]
    member this.Setup() =
        let n = this.NumOfWords - 1
        this.NonBack_Regex <- System.Text.RegularExpressions.Regex(fst (patterns[n]), opts_NonBacktracking, TimeSpan.FromSeconds(10))
        this.Compiled_Regex <- System.Text.RegularExpressions.Regex(fst (patterns[n]), opts_Compiled, TimeSpan.FromSeconds(10))
        this.Sbre_Alt_Regex <- Regex(fst (patterns[n]))
        this.Sbre_Regex <- Regex(snd (patterns[n]))


    [<Benchmark(Description="NonBacktrack: .*R1.*R2.*|.*R2.*R1.*")>]
    member this.Symbolic() =
        this.NonBack_Regex.Count(inputText)

    [<Benchmark(Description="Compiled: .*R1.*R2.*|.*R2.*R1.*")>]
    member this.Compiled() =
        this.Compiled_Regex.Count(inputText)

    [<Benchmark(Description="Sbre: .*R1.*R2.*|.*R2.*R1.*")>]
    member this.SbreAlt() =
        let tsk =
            task{
                return this.Sbre_Alt_Regex.Count(inputText)
            }
        tsk.Wait(10000)



    [<Benchmark(Description = "Sbre: .*R1.*&.*R2.*")>]
    member this.Sbre() =
        this.Sbre_Regex.Count(inputText)


[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type TestAllEnginesAllPatternsWithCompileTime(patterns: (string) list, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    let opts_None =
        Text.RegularExpressions.RegexOptions.None
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_NonBacktracking =
        Text.RegularExpressions.RegexOptions.NonBacktracking
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_Compiled =
        Text.RegularExpressions.RegexOptions.Compiled
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture

    member val Counts: int list = (patterns |> List.indexed |> List.map (fun v -> fst v + 1)) with get, set

    // [<Params(1,2,3,4,5,6,7)>]
    // [<ParamsSource("Counts")>]
    // member val NumOfWords: int = 0 with get, set
    //
    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns
    member val CompiledEngine: Sbre.RegexMatcher<TSet> = Unchecked.defaultof<_> with get, set

    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let regex = Regex(this.Pattern)
        let matcher = regex.Matcher :?> RegexMatcher<TSet>
        this.CompiledEngine <- matcher
        ()

    //
    [<Benchmark(Description="None")>]
    member this.None() =
        System.Text.RegularExpressions.Regex(this.Pattern, opts_None, TimeSpan.FromSeconds(10)).Count(inputText)

    // [<Benchmark(Description="NonBacktrack")>]
    // member this.Symbolic() =
    //     System.Text.RegularExpressions.Regex(this.Pattern, opts_NonBacktracking, TimeSpan.FromSeconds(10)).Count(inputText)
    //
    // [<Benchmark(Description="Compiled")>]
    // member this.Compiled() =
    //     System.Text.RegularExpressions.Regex(this.Pattern, opts_Compiled, TimeSpan.FromSeconds(10)).Count(inputText)

    [<Benchmark(Description = "Sbre")>]
    member this.Sbre() =
        // Regex(this.Pattern).Count(inputText)
        // (Regex(this.Pattern).Matcher :?> RegexMatcher<uint64>).DfaCount(inputText)
        this.CompiledEngine.Count(inputText)
        // this.CompiledEngine.DfaCount(inputText)
        // use cts = new CancellationTokenSource()
        // cts.CancelAfter(millisecondsDelay = 10_000)
        // let tsk =
        //     System.Threading.Tasks.Task.Factory.StartNew((fun v ->
        //         Regex(this.Pattern).Count(inputText)
        //     ))
        // tsk.Wait(cts.Token)



[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type TestSbreAllPatternsWithCompileTime(patterns: (string) list, input: string) =
    let inputText = input
    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns
    member val CompiledEngine: Sbre.RegexMatcher<TSet> = Unchecked.defaultof<_> with get, set
    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() = ()

    [<Benchmark(Description = "Sbre")>]
    member this.Sbre() =
        Sbre.Regex(this.Pattern).Count(inputText)

[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type TestSbreAllPatternsMatchOnly(patterns: (string) list, input: string) =
    let inputText = input
    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns
    member val CompiledEngine: Sbre.RegexMatcher<TSet> = Unchecked.defaultof<_> with get, set
    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let regex = Regex(this.Pattern)
        let matcher = regex.Matcher :?> RegexMatcher<TSet>
        this.CompiledEngine <- matcher
        ()

    [<Benchmark(Description = "Sbre")>]
    member this.Sbre() =
        this.CompiledEngine.Count(inputText)

[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type TestSbreAllPatternsCountSpans(patterns: (string) list, input: string) =
    let inputText = input
    member this.Patterns: System.Collections.Generic.IEnumerable<string> = patterns
    member val CompiledEngine: Sbre.RegexMatcher<TSet> = Unchecked.defaultof<_> with get, set
    [<ParamsSource("Patterns")>]
    member val Pattern: string = "" with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let regex = Regex(this.Pattern)
        let matcher = regex.Matcher :?> RegexMatcher<TSet>
        this.CompiledEngine <- matcher
        ()

    [<Benchmark(Description = "Sbre")>]
    member this.Sbre() =
        use asd = this.CompiledEngine.llmatch_all(inputText)
        ()

[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
[<HideColumns([| "" |])>]
type TestAllEnginesAllPatternsSeparateWithCompileTime(patterns: (string*string*string) list, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input

    let fst (a,b,c) = a
    let snd (a,b,c) = b
    let trd (a,b,c) = c

    // slow
    // let opts_None =
    //     Text.RegularExpressions.RegexOptions.None
    //     ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_NonBacktracking =
        Text.RegularExpressions.RegexOptions.NonBacktracking
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture
    let opts_Compiled =
        Text.RegularExpressions.RegexOptions.Compiled
        ||| Text.RegularExpressions.RegexOptions.ExplicitCapture

    member val Counts: int list = (patterns |> List.indexed |> List.map (fun (a,b) -> a + 1)) with get, set

    [<ParamsSource("Counts")>]
    member val NumOfWords: int = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() = ()

    // [<Benchmark(Description="NonBacktrack:Alt")>]
    // member this.Symbolic() =
    //     System.Text.RegularExpressions.Regex(fst (patterns[this.NumOfWords - 1]), opts_NonBacktracking, TimeSpan.FromSeconds(10)).Count(inputText)
    //
    // [<Benchmark(Description="Compiled:Alt")>]
    // member this.Compiled() =
    //     System.Text.RegularExpressions.Regex(fst (patterns[this.NumOfWords - 1]), opts_Compiled, TimeSpan.FromSeconds(10)).Count(inputText)

    // [<Benchmark(Description="Compiled:Look")>]
    // member this.CompiledLookahead() =
    //     System.Text.RegularExpressions.Regex(trd (patterns[this.NumOfWords - 1]), opts_Compiled, TimeSpan.FromSeconds(10)).Count(inputText)


    [<Benchmark(Description="Sbre:Alt")>]
    member this.SbreAlt() =
        use cts = new CancellationTokenSource()
        cts.CancelAfter(millisecondsDelay = 10_000)
        let tsk =
            System.Threading.Tasks.Task.Factory.StartNew((fun v ->
                Regex(fst (patterns[this.NumOfWords - 1])).Count(inputText)), cts.Token
            )

        tsk.Wait(cts.Token)

    [<Benchmark(Description = "Sbre:Conj")>]
    member this.Sbre() =
        Regex(snd (patterns[this.NumOfWords - 1])).Count(inputText)







[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
type TestAllEnginesSeparate(defaultRegex: string, sbreRegex: string, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    let opts_None = Text.RegularExpressions.RegexOptions.None
    let opts_NonBacktracking = Text.RegularExpressions.RegexOptions.NonBacktracking
    let opts_Compiled = Text.RegularExpressions.RegexOptions.Compiled


    member val None_Regex: System.Text.RegularExpressions.Regex =
            try System.Text.RegularExpressions.Regex(defaultRegex, opts_None, TimeSpan.FromSeconds(90))
            with e -> Unchecked.defaultof<_>
        with get, set

    member val NonBack_Regex: System.Text.RegularExpressions.Regex =
    // member val NonBack_Regex: System.Text.RuntimeRegexCopy.Regex =
            // try System.Text.RegularExpressions.Regex(defaultRegex, opts_NonBacktracking, TimeSpan.FromSeconds(90))
            // with e -> Unchecked.defaultof<_>
        null
        // System.Text.RegularExpressions.Regex(defaultRegex, opts_NonBacktracking, TimeSpan.FromSeconds(90))
        with get, set


    member val Compiled_Regex: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(defaultRegex, opts_Compiled, TimeSpan.FromSeconds(30)) with get, set

    member val SbreAlt_Regex: Regex = Regex(defaultRegex) with get, set
    member val Sbre_Regex: Regex = Regex(sbreRegex) with get, set


    [<GlobalSetup>]
    member this.Setup() =
        this.Compiled_Regex <- System.Text.RegularExpressions.Regex(defaultRegex, opts_Compiled, TimeSpan.FromSeconds(90))
        this.None_Regex <- System.Text.RegularExpressions.Regex(defaultRegex, opts_None, TimeSpan.FromSeconds(90))
        this.NonBack_Regex <- System.Text.RegularExpressions.Regex(defaultRegex, opts_NonBacktracking, TimeSpan.FromSeconds(90))
        // this.NonBack_Regex <- System.Text.RuntimeRegexCopy.Regex(defaultRegex, System.Text.RuntimeRegexCopy.RegexOptions.NonBacktracking, TimeSpan.FromSeconds(90))


    [<Benchmark(Description = "NonBacktrack: .*R1.*R2.*|.*R2.*R1.*")>]
    member this.Symbolic() =
        this.NonBack_Regex.Count(inputText)
    //     System.Text.RuntimeRegexCopy.Regex(
    //         defaultRegex,
    //         System.Text.RuntimeRegexCopy.RegexOptions.NonBacktracking,
    //         TimeSpan.FromSeconds(90)).Count(inputText)
    // // //
    // //
    // // [<Benchmark(Description="None: .*R1.*R2.*|.*R2.*R1.*")>]
    // // member this.None() =
    // //     this.None_Regex.Count(inputText)
    // //
    // // [<Benchmark(Description="Compiled: .*R1.*R2.*|.*R2.*R1.*")>]
    // // member this.Compiled() =
    // //     this.Compiled_Regex.Count(inputText)

    [<Benchmark(Description="Sbre: .*R1.*R2.*|.*R2.*R1.*")>]
    member this.SbreAlt() =
        this.SbreAlt_Regex.Count(inputText)

    // [<Benchmark(Description = "Sbre: .*R1.*&.*R2.*")>]
    // member this.Sbre() =
    //     this.Sbre_Regex.Count(inputText)


[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
[<AbstractClass>]
type TestAllEnginesCount(defaultRegex: string, sbreRegex: string, input: string) =
    do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
    let inputText = input
    let opts_None = Text.RegularExpressions.RegexOptions.None
    let opts_NonBacktracking = Text.RegularExpressions.RegexOptions.NonBacktracking
    let opts_Compiled = Text.RegularExpressions.RegexOptions.Compiled

    let alt_pattern = defaultRegex
    let conj_pattern = sbreRegex

    member val None_Regex: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(alt_pattern, opts_None) with get, set

    member val NonBack_Regex: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(alt_pattern, opts_NonBacktracking) with get, set

    member val Compiled_Regex: System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(alt_pattern, opts_Compiled) with get, set

    member val SbreAlt_Regex: Regex = Regex(alt_pattern) with get, set
    member val Sbre_Regex: Regex = Regex(conj_pattern) with get, set


    [<GlobalSetup>]
    member this.Setup() = ()

    // [<Benchmark(Description="None: '.*R1.*R2.*|.*R2.*R1.*'")>]
    // member this.Default() =
    //     let result = this.None_Regex.Matches(inputText)
    //     result.Count

    [<Benchmark(Description = "NonBacktrack")>]
    member this.Symbolic() =
        this.NonBack_Regex.Count(inputText)


    [<Benchmark(Description="Compiled")>]
    member this.Compiled() =
        this.Compiled_Regex.Count(inputText)

    [<Benchmark(Description = "Sbre")>]
    member this.Sbre() =
        this.Sbre_Regex.Count(inputText)


// [<MemoryDiagnoser(false)>]
// [<ShortRunJob>]
// [<AbstractClass>]
// [<HideColumns([| "" |])>]
// type TestOnlyNonBacktracking(pattern: string, input: string) =
//     do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
//     let inputText = input

//     let opts_NonBacktracking =
//         Text.RuntimeRegexCopy.RegexOptions.NonBacktracking
//         ||| Text.RuntimeRegexCopy.RegexOptions.ExplicitCapture

//     member val NonBack_Regex: System.Text.RuntimeRegexCopy.Regex =
//         System.Text.RuntimeRegexCopy.Regex(pattern, opts_NonBacktracking) with get, set


//     [<GlobalSetup>]
//     member this.Setup() = ()


//     [<Benchmark(Description = "NonBacktrack")>]
//     member this.Symbolic() =
//         this.NonBack_Regex.Count(inputText)
