module Sbre.Benchmarks.Paper

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.CompilerServices
open Sbre.Benchmarks.Jobs

let fullInput =
    __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText

let shortInput20k = fullInput[..19999] // 10k chars limit
let shortInput10k = fullInput[..9999] // 10k chars limit


type ParagraphShort1Word() =
    inherit
        Jobs.AllRegexesInParagraph(
            ["Huck"],
            shortInput20k
        )


type ParagraphLong1Word() =
    inherit
        Jobs.AllRegexesInParagraph(
            ["Huck"],
            // @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*",
            fullInput
        )



type ParagraphLong2Word() =
    inherit
        Jobs.AllRegexesInParagraph(
            ["Huck"; "Finn"],
            fullInput
        )

type ParagraphLong3Word() =
    inherit
        Jobs.AllRegexesInParagraph(
            ["Huck"; "Finn"; "Tom";],
            fullInput
        )


type ParagraphLong4Word() =
    inherit
        Jobs.AllRegexesInParagraph(
            ["Huck"; "Finn"; "Tom"; "Sawyer"],
            fullInput
        )




type ParagraphConjunction1() =
    inherit
        Jobs.AllRegexesInParagraphSeparate(
            // [  @"(?i)(?=[a-z]*a)(?=[a-z]*b)(?=[a-z]*c)(?=[a-z]*d)[a-z]*"  ],
            // Permutations.permuteConjInParagraph [ @"(?:(?i)\s([a-z]*a[a-z]*&[a-z]*b[a-z]*&[a-z]*c[a-z]*&[a-z]*d[a-z]*))" ],
            [@"King[\s\S]*Arthur[\s\S]*his"],
            @"King~(⊤*\n\n⊤*)his&⊤*Arthur⊤*",
            fullInput
        )

type ParagraphComplexRegex1() =
    inherit
        Jobs.AllRegexesInParagraph(
            [  @"(?:Tom.{10,25}river|river.{10,25}Tom)[\s\S]*(?:(?i)Tom|Sawyer|Huckleberry|Finn)" ],
            fullInput
        )


type ParagraphComplexRegex2() =
    inherit
        Jobs.AllRegexesInParagraph(
            [  @"(?:Tom.{10,25}river|river.{10,25}Tom)" ],
            fullInput
        )




type ParagraphInnerMatch1() =
    inherit
        Jobs.MatchInParagraphSeparate(
            @"occ[\s\S]*ing",
            @"occ~(⊤*\n\n⊤*)ing",
            fullInput
        )


type Basic1() =
    inherit
        Jobs.TestAllEngines(
            // @"(?i)Tom|Sawyer|Huckleberry|Finn",
            // @"(?i)Tom|Sawyer|Huckleberry|Finn",
            // "\w+ [0-9]",
            // "\w+ [0-9]",
            ".*have.*there.*|.*there.*have.*",
            fullInput
        )

type Basic2() =
    inherit
        Jobs.TestAllEngines(
            // "(?=.*b.*b)(?=.*i.*i)(?=.*e.*e)(?=.*F.*F)(?!.*x).*",
            // ".*&⊤*b⊤*b⊤*&⊤*i⊤*i⊤*&⊤*e⊤*e⊤*&⊤*F⊤*F⊤*&~(⊤*x⊤*)",
            // "(?=(.*[Ee]){2})(?=(.*[Ii]){2})(?=.*nn)(?=.*F.*F)(?!.*x).*",
            // ".*&⊤*nn⊤*&⊤*[Ii]⊤*[Ii]⊤*&⊤*[Ee]⊤*[Ee]⊤*&⊤*F⊤*F⊤*&~(⊤*x⊤*)",
            // 2w
            // "(?=.*have)(?=.*[a-z]*ing).*",
            // ".*have.*there.*other.*|.*there.*have.*other.*|.*there.*other.*have.*|.*have.*other.*there.*|.*other.*have.*there.*|.*other.*there.*have.*",
            // "⊤*have⊤*&⊤*there⊤*&⊤*other⊤*&.*",
            // ".*have.*there.*other.*|.*there.*have.*other.*|.*there.*other.*have.*|.*have.*other.*there.*|.*other.*have.*there.*|.*other.*there.*have.*"
            // ".*have.*&.*there.*",
            // "⊤*have⊤*&⊤*there⊤*&⊤*other⊤*&.*",

            // ".*have.*there.*other.*|.*there.*have.*other.*|.*there.*other.*have.*|.*have.*other.*there.*|.*other.*have.*there.*|.*other.*there.*have.*",
            "Twain",
            //
            fullInput

            // (?<=e)b.*asd
        )

let str = @"lethargy.*air"

type Basic3() =
    inherit
        Jobs.TestAllEngines(
            str,
            fullInput
        )


// Permutations.permuteConjInLine ["th.*at"; "an.*d"; "th.*e";"wa.*s"]
// Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"]

type DebugSbre3() =
    inherit Jobs.SbreDebugSearch([
        Permutations.permuteConjInParagraph ["Huck"]
        // Permutations.permuteConjInParagraph ["Huck"; ]
        // Permutations.permuteConjInParagraph ["Huck";"Finn"; ]
        // Permutations.permuteConjInParagraph ["Huck";"Finn"; "Tom"; ]
        // Permutations.permuteConjInParagraph ["Huck";"Finn"; "Tom"; "Sawyer" ]

        // @"(?:(?i)~(⊤*\n\n⊤*)&⊤*Huck⊤*)"
        // @"(?:(?i)~(⊤*\n\n⊤*)&⊤*Huck⊤*)"

        // @".*Huck.*&~(⊤*the⊤*)"
        // @".*Huck.*&~(⊤*the⊤*)&.*\n"
        // @".*Huck.*&~(⊤*the⊤*)"
        // @".*Huck.*&~(⊤*the⊤*)"

        // Permutations.permuteConjInLine ["which";]
        // Permutations.permuteConjInLine ["which";"could"]
        // Permutations.permuteConjInLine ["which";"could"; "where"]
        // Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"]

        // @"~(⊤*\n\n⊤*)&⊤*Huck⊤*&⊤*could⊤*&⊤*there⊤*"
    ], fullInput)


type WordsLine1() =
    inherit
        Jobs.TestAllEnginesWords(
            ["which";],
            fullInput
        )

type WordsLine2() =
    inherit
        Jobs.TestAllEnginesWords(
            ["which";"could"],
            fullInput
        )

type WordsLine3() =
    inherit
        Jobs.TestAllEnginesWords(
            ["which";"could"; "there"],
            fullInput
        )

type WordsLine4() =
    inherit
        Jobs.TestAllEnginesWords(
            ["which";"could"; "there"; "thing"],
            fullInput
        )

type WordsLine5() =
    inherit
        Jobs.TestAllEnginesWords(
            // ["which";"could"; "that"; "have"; "were"],
            ["were";"have"; "which";"could"; "that"],
            fullInput
        )

type LWord1() =
    inherit
        Jobs.TestAllEnginesWords(
            // ["w[a-z]+h";"c[a-z]+d"],
            // [@"w[a-z]*h"; @"c[a-z]*d"; ],
            [@"w[a-z]*h"; @"c[a-z]*d"; ],
            fullInput
        )




// let regexes =  ["which";"could"; "where"]
// let regexes =  ["which";"could"; "where"]
// let regexes =  ["wh.*ich";"co.*uld"; "wh.*ere"]
let regexes =  ["wh.*ich";"co.*uld"; "wh.*ere"]
// let regexes = ["t.*hat"; "a.*nd"; "t.*he"]

type RegexLine1() =
    inherit
        Jobs.TestAllEnginesSeparate(
            // Permutations.permuteAltInLine ["that"; "and"; "the";"was"],
            // Permutations.permuteAltInLine ["th.*at"; "an.*d"; "th.*e";"wa.*s"],
            Permutations.permuteAltInLine regexes,
            // Permutations.permuteAltInLine ["that"; "and"; "the";"was"; "for"; "his"],
            // Permutations.permuteConjInLine ["th.*at"; "an.*d"; "th.*e";"wa.*s"],
            Permutations.permuteConjInLine2 regexes,
            fullInput
        )

// type RegexLine1() =
//     inherit
//         Jobs.TestAllEnginesSeparate(
//             // Permutations.permuteAltInLine ["that"; "and"; "the";"was"],
//             // Permutations.permuteAltInLine ["th.*at"; "an.*d"; "th.*e";"wa.*s"],
//             Permutations.permuteAltInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"],
//             // Permutations.permuteAltInLine ["that"; "and"; "the";"was"; "for"; "his"],
//             // Permutations.permuteConjInLine ["th.*at"; "an.*d"; "th.*e";"wa.*s"],
//             Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"],
//             fullInput
//         )


type RegexLine3() =
    inherit
        Jobs.TestAllEnginesWords(
            [@"whi[a-z]*h";@"cou[a-z]*d"; @"the[a-z]*e" ],
            fullInput
        )



type Lines1() =
    inherit
        Jobs.TestAllEnginesSeparate(
            // Permutations.permuteAltInLine ["that"; "and"; "the";"was"],
            // Permutations.permuteAltInLine ["th.*at"; "an.*d"; "th.*e";"wa.*s"],
            Permutations.permuteAltInLine regexes,
            // Permutations.permuteAltInLine ["that"; "and"; "the";"was"; "for"; "his"],
            // Permutations.permuteConjInLine ["th.*at"; "an.*d"; "th.*e";"wa.*s"],
            ".*Huck.*&~(.*F.*)",
            fullInput
        )






type Twain_1() =
    inherit
        Jobs.TestAllEngines(
            // "Twain", // OK
            // "(?i)Twain", // .net faster
            // "[a-z]shing", // .net faster
            @"Huck[a-zA-Z]+|Saw[a-zA-Z]+", // .net
            // "[a-q][^u-z]{13}x", // ????
            // "Tom|Sawyer|Huckleberry|Finn", // same
            fullInput
        )





































