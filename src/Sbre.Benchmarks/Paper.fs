module Sbre.Benchmarks.Paper

open System.Text.RegularExpressions
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
            [  @"(?i)(?=[a-z]*a)(?=[a-z]*b)(?=[a-z]*c)(?=[a-z]*d)[a-z]*"  ],
            Permutations.permuteConjInParagraph [ @"(?:(?i)\s([a-z]*a[a-z]*&[a-z]*b[a-z]*&[a-z]*c[a-z]*&[a-z]*d[a-z]*))" ],
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
        Jobs.TestAllBasic(
            @"(?i)Tom|Sawyer|Huckleberry|Finn",
            @"(?i)Tom|Sawyer|Huckleberry|Finn",
            fullInput
        )

type Basic2() =
    inherit
        Jobs.TestAllBasic(
            @"Tom.{10,25}river|river.{10,25}Tom",
            @"Tom.{10,25}river|river.{10,25}Tom",
            fullInput
        )

type Basic3() =
    inherit
        Jobs.TestAllBasic(
            @"Tom.{10,25}river|river.{10,25}Tom",
            @"Tom.{10,25}river|river.{10,25}Tom",
            fullInput
        )