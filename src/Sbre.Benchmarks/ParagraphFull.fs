module Sbre.Benchmarks.ParagraphFull

open System.Text.RegularExpressions

let fullInput =
    __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText

let shortInput = fullInput[..9999] // 10k chars limit



let words = [ "Huck"; "from"; "you" ]
// let words = [ "the"; "and"; "that" ]

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

    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*"; 58 ms
    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*"; // 30 ms
    @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*"; // 16 ms
    // @"~(⊤*\n\n⊤*)\n"; // 20 ms
    // @"~(⊤*\n\n⊤*)"; // 20 ms (alloc 20mb)

    // @"~(⊤*\n\n⊤*)\n&⊤*chuc\w+⊤*&⊤*from⊤*"; // 49 ms
    // @"~(⊤*\n\n⊤*)\n&⊤*the⊤*&⊤*and⊤*&⊤*that⊤*"; // 49 ms

    // @"~(⊤*\n\n⊤*)\n&⊤*chuc\w+⊤*&⊤*from⊤*"; // 49 ms
]

type Sbre_Debug() =
    inherit Jobs.SbreDebugSearch(patterns, fullInput)


let common4 = ["the";"and";"of";"that"]

type Compiled_4() =
    inherit
        Jobs.RuntimeFullSearch(
            common4,
            fullInput,
            RegexOptions.None
        )




type All_1() =
    inherit
        Jobs.AllRegexesInParagraph( ["Huck"], fullInput )














