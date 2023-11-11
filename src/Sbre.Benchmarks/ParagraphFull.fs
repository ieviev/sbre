module Sbre.Benchmarks.ParagraphFull

open System.Text.RegularExpressions
open Sbre.Benchmarks.Jobs

let fullInput =
    __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText

let shortInput20k = fullInput[..19999]
let shortInput100k = fullInput[..99_999]
let shortInput10k = fullInput[..9999]



let words = [ "Huck"; "from"; "you" ]



type DebugRuntime() =
    inherit
        Jobs.RuntimeFullSearch(
            // 570ms
            // [@"\s(?=[a-zA-Z]*a)(?=[a-zA-Z]*b)(?=[a-zA-Z]*c)(?=[a-zA-Z]*d)[a-zA-Z]{0,12}ing"], // 1s
            // 1s
            // [  @"(?i)[a-z]{2,12}ing to the (?:d[a-z]+)\s" ],



            // 476ms
            // [  @"(?=[a-z]*a)(?=[a-z]*b)(?=[a-z]*c)(?=[a-z]*d)[a-z]*"  ], // 1s
            [  @"(?=[a-z]*a)(?=[a-z]*b)(?=[a-z]*c)(?=[a-z]*d)[a-z]{5,16}"  ], // 1s













            // 564.4 ms
            // [  @"(?=\w*a)(?=\w*b)(?=\w*c)(?=\w*d)\w*"  ], // 1s

            // 65
            // [  @"(a|an|the)\s(man|year|day|thing|place|life|house|home)"  ], // 1s
            // 50
            // [  @"(man|year|day|thing|place|life|house|home)\s+of\s+\w+"  ], // 1s
            //
            // [  @"(man|year|day|thing|place|life|house|home)\s+\w+\s+of"  ], // 1s
            // [  @"[a-q][^u-z]{13}x"  ], // 1s


            // (a|the)...
            // 350ms
            // [  @"(a|the)\s(?i)Tom|Sawyer|Huckleberry|Finn" ], // 35ms
            // ["((?i)Tom|Sawyer|Huckleberry|Finn)";  ],

            //
            // [  @"(?:(?i)[a-z]{0,12}ing to the (?:d[a-z]{0,12})\s)"; "Huck" ],


            // [  @"(?:(?i)(a|the)\s+\w{4,12}\s)" ],

            // [  @"(?:(?i)(a|the)\s+\w{12,25}\s[\s\S]*(a|the)\s+\w{12,25})" ],
            // [  @"(?:(?i)(a|the)\s+\w{6,25}\s[\s\S]*(a|the)\s+\w{10,25})" ],

            // [  @"(?:(?i)[a-z]{0,12}ing to the (?:d[a-z]{0,12})\s)"; "Huck" ],


            // [  @"(?:(?i)Tom|Sawyer|Huckleberry|Finn)[\s\S]*(and|of|from)\s[a-zA-Z]{0,12}ing\sthe"; ],
            fullInput,
            // RegexOptions.None
            RegexOptions.Compiled
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



    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*"; // 8 ms
    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*"; // 4 ms
    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*"; // 58 ms



    // Permutations.permuteConjInParagraph [ @"\s([a-z]*a[a-z]*&[a-z]*b[a-z]*&[a-z]*c[a-z]*&[a-z]*d[a-z]*)" ] // 102

    // Permutations.permuteConjInParagraph [
    //     @"\s(\w*a\w*&\w*b\w*&\w*c\w*&\w*d\w*)\s"
    // ]

    // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*"; // 2 ms
    @"~(⊤*\n\n⊤*)&⊤*Huck⊤*&⊤*Finn⊤*"; // 2 ms
    // @"~(⊤*\n\n⊤*)&⊤*Huck⊤*&⊤*Finn⊤*"; // 2 ms
    // Permutations.permuteAltInLine ["which";"could"] // 22ms
    // Permutations.permuteConjInLine ["which";"could"] // 18ms
    // Permutations.permuteConjInLine ["wh[a-z]+h";"co[a-z]+d"] // 107
    // Permutations.permuteConjInLine ["w[a-z]+h";"c[a-z]+d"] // 143
    // Permutations.permuteConjInLine [@"w[a-z]*h "; @"c[a-z]*d "; ] // 123
    // Permutations.permuteConjInLine [@"w[a-z]*h "; @"c[a-z]*d "; @"s[a-z]*d " ] // 266
    // Permutations.permuteConjInLine [@"w[a-z]*h "; @"c[a-z]*d "; @"s[a-z]*d "; @"w[a-z]*d " ] // 273
    // Permutations.permuteConjInLine ["the";"and";"was";"with"] // 18ms

    // Permutations.permuteConjInParagraph ["Huck"; ] // 13.3

    // ---------
    // @"~(⊤*\n\n⊤*)&⊤*ing&occ⊤*"
    // @"occ~(⊤*\n\n⊤*)ing"

    // @"\w* \d" // 22ms
    // @"lethargy.*air" // 22ms
    // @"~(⊤*\n\n⊤*)" // 12ms

    // "⊤*have⊤*&⊤*there⊤*&⊤*other⊤*&.*" // 100ms

    // @".*have.*there.*|.*there.*have.*"
    // ".*thing.*&.*great.*"
    // ".*thing.*&.*again.*"

    // @"⊤*have⊤*&⊤*there⊤*&⊤*other⊤*&.*" // 144b
    // @".*have.*&.*there.*&.*other.*" // 278b


]

type DebugSbre() =
    // inherit Jobs.SbreDebugSearch(patterns, shortInput100k)
    // inherit Jobs.SbreDebugSearch([@"~(⊤*\n\n⊤*)\n&⊤*Twain⊤*"], shortInput100k)
    inherit Jobs.SbreDebugSearch(patterns, fullInput)


type DebugSbre2() =
    // inherit Jobs.SbreDebugSearch(["⊤*have⊤*&⊤*there⊤*&.*"], fullInput)
    // inherit Jobs.SbreDebugSearch([".*have.*&.*there.*"], fullInput)
    inherit Jobs.SbreDebugSearch([".*city.*&.*town.*"], fullInput)


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















