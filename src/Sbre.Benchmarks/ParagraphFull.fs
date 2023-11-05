module Sbre.Benchmarks.ParagraphFull

open System.Text.RegularExpressions
open Sbre.Benchmarks.Jobs

let fullInput =
    __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText

let shortInput20k = fullInput[..19999] // 10k chars limit
let shortInput10k = fullInput[..9999] // 10k chars limit



let words = [ "Huck"; "from"; "you" ]
// let words = [ "the"; "and"; "that" ]


type DebugRuntime() =
    inherit
        Jobs.RuntimeFullSearch(
            // 570ms
            // [@"\s(?=[a-zA-Z]*a)(?=[a-zA-Z]*b)(?=[a-zA-Z]*c)(?=[a-zA-Z]*d)[a-zA-Z]{0,12}ing"], // 1s
            // 1s
            // [  @"(?i)[a-z]{2,12}ing to the (?:d[a-z]+)\s" ],
            // 2s
            // [  @"(?=[a-z]*a)(?=[a-z]*b)(?=[a-z]*c)(?=[a-z]*d)[a-z]*"  ], // 1s
            // 350ms
            // [  @"(?i)Tom|Sawyer|Huckleberry|Finn" ], // 1s

            [  @"(?:(?i)[a-z]{0,12}ing to the (?:d[a-z]{0,12})\s)"; "Huck" ],
            fullInput,
            // RegexOptions.None
            RegexOptions.Multiline
        )


type NonBacktracking_3() =
    inherit
        Jobs.RuntimeFullSearch(
            words[0..2],
            fullInput,
            RegexOptions.NonBacktracking
        )


type None_3() =
    inherit
        Jobs.RuntimeFullSearch(
            words[0..2],
            fullInput,
            RegexOptions.None
        )

type Sbre_Combined_3() =
    inherit
        Jobs.SbreCombinedSearch(
            words[0..2],
            fullInput
        )

type Sbre_3_SingleRegex() =
    inherit
        Jobs.SbreCombinedSearch(
            words[0..2],
            fullInput
        )

// let patterns = [ @"\n\n~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*";  ]
let patterns = [
    // @"\n\n~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*"; // 51.4ms
    // @"\n\n~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*"; // 34ms


    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*"; // 58 ms
    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*"; // 58 ms
    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*"; // 30 ms
    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*"; // 16 ms

    // Permutations.permuteConjInParagraph ["Huck"] // 13.3
    // Permutations.permuteConjInParagraph [ @"Tom.{10,25}river|river.{10,25}Tom"; ] // 102
    // Permutations.permuteConjInParagraph [ @"(?i)c[a-zA-Z]{0,12}"; ] // 102
    // Permutations.permuteConjInParagraph [ @"\s([a-z]*a[a-z]*&[a-z]*b[a-z]*&[a-z]*c[a-z]*&[a-z]*d[a-z]*)" ] // 102
    // Permutations.permuteConjInParagraph ["Huck"; "Finn"; "from"; "tw[a-z]+"] // 48
    // Permutations.permuteConjInParagraph [  @"(?i)huck"; @"\sand\s"; @"\sof\s"; @"\sthat\s"; @"\sHuck\s" ] // 48
    // Permutations.permuteConjInParagraph [  @"(?i)huck"; @"\sand\s"; @"\sof\s"; @"\sthat\s"; @"\sHuck\s" ] // 48
    Permutations.permuteConjInParagraph [ @"\s([a-zA-Z]{0,12}ing&⊤*b⊤*)" ] // 48
    // Permutations.permuteConjInParagraph ["(?:Tom|Sawyer|Huckleberry|Finn)"; "Twain" ] // 48
]

type DebugSbre() =
    inherit Jobs.SbreDebugSearch(patterns, fullInput)


let common4 = ["the";"and";"of";"that"]

type Compiled_4() =
    inherit
        Jobs.RuntimeFullSearch(
            common4,
            fullInput,
            RegexOptions.None
        )






type DebugAll() =

    inherit
        // Jobs.AllRegexesInParagraph( ["(?:Mark|Twain|file)"; "(?:stories|speeches)"], shortInput10k )
        // Jobs.AllRegexesInParagraph( ["(?:Mark|Twain|file)"; "(?:stories|speeches)"], shortInput20k )
        // Jobs.AllRegexesInParagraph( [ "(?:Tom|Sawyer|before)"; "(?:Huckleberry|Finn|legs)" ], shortInput20k )
        // Jobs.AllRegexesInParagraph( [ "(?:Tom|Sawyer|before)"; "(?:Huckleberry|Finn|legs)" ], fullInput )
        // Jobs.AllRegexesInParagraph( [ "(?:Tom|Sawyer|before)";  @"old[\s\S]*thing" ], fullInput ) // 4x
        // Jobs.AllRegexesInParagraph( [ "(?:Tom|Sawyer|before)";  @"[a-z]ing[\s\S]*thing" ], fullInput ) // 4x
        // Jobs.AllRegexesInParagraph( [@"(?i)through[\s\S]*game[\s\S]*other"], fullInput ) // 4x
        Jobs.AllRegexesInParagraph( [@"Huck"], fullInput ) // 4x
        // Jobs.AllRegexesInParagraph( ["(?:Mark|Twain|file)"; "(?:stories|speeches)"], shortInput20k )
        // Jobs.AllRegexesInParagraph( ["(?:Mark|Twain|file)"; "org.*ing" ; "(?:stories|speeches)"], shortInput20k )
        // Jobs.AllRegexesInParagraph( ["[a-zA-Z]+ing"; "Huck"; "Finn" ], shortInput20k )


type All_1() =
    inherit
        // Jobs.AllRegexesInParagraph( ["Huck"], fullInput )
        // Jobs.AllRegexesInParagraph( [@"(?:(?i)Tom|Sawyer|Huckleberry|Finn)"], fullInput )
        Jobs.AllRegexesInParagraph( [ @"(?:Tom|Sawyer|Huckleberry|Finn)[\s\S]*[a-zA-Z]*ing[\s\S]*river"; ], fullInput ) // 2x
        // Jobs.AllRegexesInParagraph( [ @"(?:(?i)Tom|Sawyer|Huckleberry|Finn)[\s\S]*(and|of|from)[\s\S]*[a-zA-Z]*ing[\s\S]*river"; ], fullInput ) // ok

type All_2() =
    inherit
        // Jobs.AllRegexesInParagraph( [@"(?:Huck[a-zA-Z]+)"; @"(?:Saw[a-zA-Z]+)"], fullInput )
        // Jobs.AllRegexesInParagraph( [@"(?:(?i)Huck[a-zA-Z]+)"; @"(?:(?i)Saw[a-zA-Z]+)"], fullInput )
        // Jobs.AllRegexesInParagraph( [@"([A-Za-z]awyer|[A-Za-z]inn)\s"], fullInput )
        Jobs.AllRegexesInParagraph( [@".{2,4}(Tom|Sawyer|Huckleberry|Finn)"], fullInput )















