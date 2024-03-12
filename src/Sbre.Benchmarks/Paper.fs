module Sbre.Benchmarks.Paper

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


type DebugSbre3() =
    inherit Jobs.SbreDebugSearch([

        Permutations.permuteConjInParagraph ["Huck";]
        Permutations.permuteConjInLine ["Huck";"Finn"; "Tom"; "Saw" ]
        Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"]
        Permutations.permuteConjInParagraph ["Huck";"Finn"; "Tom"; "Saw" ]
        ".{0,2}(Tom|Sawyer|Huckleberry|Finn)"
        "\s[a-zA-Z]{0,12}ing\s"
        "\s([A-Za-z]awyer|[A-Za-z]inn)\s"

        // "Twain"
        // Permutations.permuteConjInLine ["th.*at"; "an.*d"; "th.*e";"wa.*s"]
        // Permutations.permuteAltInLine [@"whi[a-z]*h";@"cou[a-z]*d"; @"the[a-z]*e" ]
    ], fullInput)


type DebugAll() =
    inherit Jobs.TestAllEngines(
        // Permutations.permuteAltInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"]
        Permutations.permuteAltInLine ["Huck";"Finn"; "Tom"; "Sawyer" ]

    , fullInput)


type WordsLine1() =
    inherit
        Jobs.TestAllEnginesSeparate(
            ".*Huck.*",
            ".*Huck.*",
            // Permutations.permuteAltInLine ["Huck" ],
            // Permutations.permuteConjInLine ["Huck" ],
            fullInput
        )

type WordsLine2() =
    inherit
        Jobs.TestAllEnginesSeparate(
            // ".*Huck.*Finn.*|.*Finn.*Huck.*",
            Permutations.permuteAltInLine  ["Huck"; "Finn"; ],
            Permutations.permuteConjInLine  ["Huck"; "Finn"; ],
            fullInput
        )

type WordsLine3() =
    inherit
        Jobs.TestAllEnginesSeparate(
            Permutations.permuteAltInLine ["Huck"; "Finn"; "Tom"; ],
            Permutations.permuteConjInLine ["Huck"; "Finn"; "Tom"; ],
            fullInput
        )

type WordsLine4() =
    inherit
        Jobs.TestAllEnginesSeparate(
            Permutations.permuteAltInLine ["Huck"; "Finn"; "Tom"; "Sawyer"],
            Permutations.permuteConjInLine ["Huck"; "Finn"; "Tom"; "Sawyer"],
            fullInput
        )

type WordsLine5() =
    inherit
        Jobs.TestAllEnginesSeparate(
            Permutations.permuteAltInLine ["Huck"; "Finn"; "Tom"; "Sawyer"; "Usually"],
            Permutations.permuteConjInLine ["Huck"; "Finn"; "Tom"; "Sawyer"; "Usually"],
            fullInput
        )

type WordsLine6() =
    inherit
        Jobs.TestAllEnginesSeparate(
            Permutations.permuteAltInLine ["Huck"; "Finn"; "Tom"; "Sawyer"; "Usually"; "Now" ],
            Permutations.permuteConjInLine ["Huck"; "Finn"; "Tom"; "Sawyer"; "Usually"; "Now" ],
            fullInput
        )


type WordsLine7() =
    inherit
        Jobs.TestAllEnginesSeparate(
            Permutations.permuteAltInLine ["Huck"; "Finn"; "Tom"; "Sawyer"; "Usually"; "Now"; "Yes" ],
            Permutations.permuteConjInLine ["Huck"; "Finn"; "Tom"; "Sawyer"; "Usually"; "Now"; "Yes" ],
            fullInput
        )


// type WordsLine5() =
//     inherit
//         Jobs.TestAllEnginesWords(
//             // ["which";"could"; "that"; "have"; "were"],
//             ["were";"have"; "which";"could"; "that"],
//             fullInput
//         )
//
// type LWord1() =
//     inherit
//         Jobs.TestAllEnginesWords(
//             // ["w[a-z]+h";"c[a-z]+d"],
//             // [@"w[a-z]*h"; @"c[a-z]*d"; ],
//             [@"w[a-z]*h"; @"c[a-z]*d"; ],
//             fullInput
//         )




// let regexes =  ["H.*k"; "F.*n"; "T.*m" ]

// 1.
// let regexes =  [@"(?i)[a-z]{2,12}ing to the (?:d[a-z]+)\s"; "H.*k"; "F.*n"; "T.*m" ]
// let regexes =  [@"(?:(?i)ing to the (?:d[a-z]+)\s)"; "H.*k"; "F.*n"; "T.*m" ]
// let regexes =  [@"d[a-z]{0,5}p "; "[gw][a-z]*ing to " ]
// let regexes = [ @" [gw][a-z]{0,8}ing to [a-z]{0,8}[a-z] "; "Huck"; "Finn"; "Tom"; "Sawyer" ]
let regexes = [ @" [gw][a-z]{0,8}ing to [a-z]{0,8}[a-z] "; "Huck"; "Finn"; "Tom"; "Sawyer" ]


// @"[a-qA-Q][^u-z]{0,13}l [a-qA-Q][^u-z]{0,13}l [a-qA-Q][^w-z]{0,13}t" // 1.06
// @"[a-qA-Q][^u-z]{0,13}g.*investigation"


// @"[a-qA-Q][^u-z]{0,13}l [a-qA-Q][^u-z]{0,13}l [a-qA-Q][^w-z]{0,13}t" // 1.06
// @"[a-qA-P][^w-z]{0,13}g.*investigation" // 2.5

let pattern =
    Permutations.permuteAltInLine [
            @"[a-qA-Q][^u-z]{0,13}l [a-qA-Q][^u-z]{0,13}l [a-qA-Q][^w-z]{0,13}t" // 1.06
            @"[a-qA-P][^w-z]{0,13}g.*investigation" // 2.5
        ]


type PatternLine1() =
    inherit
        Jobs.TestAllEnginesSeparate(
            Permutations.permuteAltInLine [
                @"[a-qA-Q][^u-z]{0,13}l [a-qA-Q][^u-z]{0,13}l [a-qA-Q][^w-z]{0,13}t" // 1.06
                @"[a-qA-P][^w-z]{0,13}g.*investigation" // 2.5
            ],
            Permutations.permuteConjInLine [
                @"[a-qA-Q][^u-z]{0,13}l [a-qA-Q][^u-z]{0,13}l [a-qA-Q][^w-z]{0,13}t" // 1.06
                @"[a-qA-P][^w-z]{0,13}g.*investigation" // 2.5
            ],
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

// type RegexLine3() =
//     inherit
//         Jobs.TestAllEngines(
//             Permutations.permuteAltInLine [@"whi[a-z]*h";@"cou[a-z]*d"; @"the[a-z]*e" ],
//             fullInput
//         )



type Lines1() =
    inherit
        Jobs.TestAllEnginesSeparate(
            @"(?:(?!T).)*Huck(?:(?!T).)*",
            ".*Huck.*&~(.*T.*)",
            fullInput
        )

type Lines2() =
    inherit
        Jobs.TestAllEnginesSeparate(
            @"(?:(?![aeio]).)*Huck(?:(?![aeio]).)*",
            ".*Huck.*&~(.*[aeio].*)",
            fullInput
        )


type TwainRegexes() =
    inherit
        Jobs.TestAllEnginesAllPatternsWithCompileTime(
            [
               "Twain"
               "(?i)Twain"
               "[a-z]shing"
               @"Huck[a-zA-Z]+|Saw[a-zA-Z]+"
               "[a-q][^u-z]{13}x"
               "Tom|Sawyer|Huckleberry|Finn"
               "(?i)Tom|Sawyer|Huckleberry|Finn"
               ".{0,2}(Tom|Sawyer|Huckleberry|Finn)"
               ".{2,4}(Tom|Sawyer|Huckleberry|Finn)"
               "Tom.{10,25}river|river.{10,25}Tom"
               "[a-zA-Z]+ing"
               "\s[a-zA-Z]{0,12}ing\s"
               "\s([A-Za-z]awyer|[A-Za-z]inn)\s"
               """["'][^"']{0,30}[?!\.]["']"""
            ],
            fullInput
        )

let sherlock = "/mnt/g/repos/rebar/benchmarks/haystacks/sherlock.txt" |> System.IO.File.ReadAllText

type TwainRegexesMatchOnly() =
    inherit
        Jobs.TestAllEnginesAllPatternsMatchOnly(
            [
               // """\b\w+nn\b"""
               "Twain"
               // "(?i)Twain"
               // "[a-z]shing"
               // @"Huck[a-zA-Z]+|Saw[a-zA-Z]+" // ++
               // "[a-q][^u-z]{13}x"
               // "Tom|Sawyer|Huckleberry|Finn" // ++
               // "(?i)Tom|Sawyer|Huckleberry|Finn" // ++compiled
               // ".{0,2}(Tom|Sawyer|Huckleberry|Finn)"
               // ".{2,4}(Tom|Sawyer|Huckleberry|Finn)"
               // "Tom.{10,25}river|river.{10,25}Tom" // ++
               // "[a-zA-Z]+ing"
               // "\s([A-Za-z]awyer|[A-Za-z]inn)\s" // ++
               // """["'][^"']{0,30}[?!\.]["']""" // ++
               // "\s[a-zA-Z]{0,12}ing\s"
            ],
            sherlock
        )



type Rebar1() =
    inherit
        Jobs.TestAllEnginesAllPatternsMatchOnly(
            [
               // """Sherlock Holmes"""
               """(?i)Sherlock Holmes"""
            ],
            fullInput
        )
type Rebar1Ru() =
    inherit
        // Jobs.TestSbreAllPatternsCountSpans(
        // Jobs.TestSbreAllPatternsMatchOnly(
        Jobs.TestAllEnginesAllPatternsMatchOnly(
            [
               // """Шерлок Холмс"""
               """(?i)Шерлок Холмс"""
            ],
            "/mnt/g/repos/rebar/benchmarks/haystacks/opensubtitles/ru-sampled.txt" |> System.IO.File.ReadAllText
        )

type Rebar2() =
    inherit
        // Jobs.TestSbreAllPatternsCountSpans(
        // Jobs.TestSbreAllPatternsMatchOnly(
        Jobs.TestAllEnginesAllPatternsMatchOnly(
            [
               // """Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"""
               """Sherlock H|John W|Irene A|Inspector L|Professor M"""
            ],
            "/mnt/g/repos/rebar/benchmarks/haystacks/opensubtitles/en-sampled.txt" |> System.IO.File.ReadAllText
        )

type Rebar6() =
    inherit
        Jobs.TestSbreAllPatternsCountSpans(
        // Jobs.TestSbreAllPatternsMatchOnly(
        // Jobs.TestAllEnginesAllPatternsMatchOnly(
            [
               """.*.*=.*"""
            ],
            "/mnt/g/repos/rebar/benchmarks/haystacks/cloud-flare-redos.txt" |> System.IO.File.ReadAllText
        )

type Rebar4() =
    inherit
        // Jobs.TestSbreAllPatternsCountSpans(
        // Jobs.TestSbreAllPatternsMatchOnly(
        Jobs.TestAllEnginesAllPatternsMatchOnly(
            [
               """Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"""
            ],
            "/mnt/g/repos/rebar/benchmarks/haystacks/opensubtitles/en-sampled.txt" |> System.IO.File.ReadAllText
        )

type Rebar5() =
    inherit
        Jobs.TestAllEnginesAllPatternsMatchOnly(
            [
               """\b[0-9A-Za-z_]+\b"""
            ],
            "/mnt/g/repos/rebar/benchmarks/haystacks/opensubtitles/en-sampled.txt" |> System.IO.File.ReadAllText
        )


type Rebar10() =
    inherit
        Jobs.TestAllEnginesAllPatternsMatchOnly(
            [
               """[A-Za-z]{10}\s+[\s\S]{0,100}Result[\s\S]{0,100}\s+[A-Za-z]{10}"""
            ],
            // "/mnt/g/repos/rebar/benchmarks/haystacks/rust-src-tools-3b0d4813.txt" |> System.IO.File.ReadAllText
            let all = "/mnt/g/repos/rebar/benchmarks/haystacks/rust-src-tools-3b0d4813.txt" |> System.IO.File.ReadAllText
            // all[..500000]
            all
        )


type Rebar14() =
    inherit
        Jobs.TestAllEnginesAllPatternsMatchOnly(
            [
               """.*[^A-Z]|[A-Z]"""
            ],
            String.replicate 200 "A"
        )



type CounterCompileTimeRegexes() =
    inherit
        Jobs.TestSbreAllPatternsWithCompileTime(
            [
               "[a-q][^u-z]{13}x"
               ".{0,2}(Tom|Sawyer|Huckleberry|Finn)"
               ".{2,4}(Tom|Sawyer|Huckleberry|Finn)"
               "Tom.{10,25}river|river.{10,25}Tom"
               "\s[a-zA-Z]{0,12}ing\s"
               """["'][^"']{0,30}[?!\.]["']"""
            ],
            fullInput
        )

type CounterMatchTimeRegexes() =
    inherit
        Jobs.TestSbreAllPatternsMatchOnly(
            [
               "[a-q][^u-z]{13}x"
               ".{0,2}(Tom|Sawyer|Huckleberry|Finn)"
               ".{2,4}(Tom|Sawyer|Huckleberry|Finn)"
               "Tom.{10,25}river|river.{10,25}Tom"
               "\s[a-zA-Z]{0,12}ing\s"
               """["'][^"']{0,30}[?!\.]["']"""
            ],
            fullInput
        )

type SampleRegexes() =
    inherit
        Jobs.TestSbreAllPatternsMatchOnly(
        // Jobs.TestSbreAllPatternsCountSpans(
            [
               // "Twain"
               // "(?i)Twain"
               // "[a-z]shing"
               // @"Huck[a-zA-Z]+|Saw[a-zA-Z]+"
               // "[a-q][^u-z]{13}x"
               // "Tom|Sawyer|Huckleberry|Finn"
               // "(?i)Tom|Sawyer|Huckleberry|Finn"
               // ".{0,2}(Tom|Sawyer|Huckleberry|Finn)"
               // ".{2,4}(Tom|Sawyer|Huckleberry|Finn)"
               // "Tom.{10,25}river|river.{10,25}Tom"
               // "[a-zA-Z]+ing"
               "\s[a-zA-Z]{0,12}ing\s"
               // "\s([A-Za-z]awyer|[A-Za-z]inn)\s"
               // """["'][^"']{0,30}[?!\.]["']"""
               // """\b\w+nn\b"""
               // -----------------
               // """(?<=\W)\w+nn(?=\W)"""
               // """(?<=\a|\W)\w+nn(?=\W|\z)"""
               // @"[a-z][a-z]shing"
               // @"[a-z][a-z]sh(?=ing)"
                // Permutations.permuteConjInParagraph ["Huck";]
                // Permutations.permuteConjInLine ["Huck";"Finn"; "Tom"; "Saw" ]
                // Permutations.permuteConjInLine ["t.*hat"; "a.*nd"; "t.*he";"w.*as"]
                // Permutations.permuteConjInParagraph ["Huck";"Finn"; "Tom"; "Saw" ]
            ],
            fullInput
        )



let paragraphWords = ["Huck"; "Finn"; "Tom"; "Sawyer"; "Usually"; ]

type ParagraphRegexes() =
    inherit
        Jobs.TestAllEnginesAllPatternsParagraphSeparate(
            ["Huck"; "Finn"; "Tom"; "Sawyer"; "Usually"],
            fullInput
        )





let lineWords =
    [
        "Huck"; "Finn"; "Tom"; "Sawyer"
        "Usually"; "Now"; "Yes"; "They"
        "There"; "You"; "Why"; "Mrs"
        "She"; "Then"; "When"; "What"
        "New"; "THE"; "One" ; "His"
    ]


let inLine n =
    "",
    // Permutations.permuteAltInLine lineWords[0..n],
    Permutations.permuteConjInLine lineWords[0..n],
    Permutations.permuteLookaheadInLine lineWords[0..n]

type LineRegexes() =
    inherit
        Jobs.TestAllEnginesAllPatternsSeparateWithCompileTime(
            [
               inLine 0
               // inLine 1
               // inLine 2
               // inLine 3
               // inLine 4
               // inLine 5
               // inLine 6
               // inLine 19
            ],
            fullInput
        )








// type TestNonbacktrackingByte() =
//     inherit
//         Jobs.TestOnlyNonBacktracking(
//             ".*Huck.*",
//             fullInput
//         )




















