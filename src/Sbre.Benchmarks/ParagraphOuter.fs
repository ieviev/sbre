module Sbre.Benchmarks.ParagraphOuter


let paragraphRegexes = [
    // line-loop (excludes last paragraph)
    @"(?:.+\n)+\n"
    // lazy loop with trailing whitespace (excludes last paragraph)
    @"(?:[\s\S])+?\n\n+"

    // lazy loop between anchors // SLOW
    // @"(?<=\n\n|\A)(?:[\s\S])+?(?=\n\n|\z)"
    // line loop proper
    @"(?:.+(?:\n|\z))+(?:\n|\z)"
    // lazy loop with trailing whitespace
    @"(?:[\s\S])+?(?:\n\n+|\z)"
    // neg-lookahead with leading whitespace // not supported
    @"(?:[\s\S](?!\n\n))+."
    // neg-lookahead with trailing whitespace // not supported
    @"(?:[\s\S](?!\n\n))*[\s\S]{2}\n*"
]

let inputText =
    __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText

type None1() = inherit Jobs.OnlyC_None(paragraphRegexes,inputText)
type NonBack1() = inherit Jobs.OnlyC_NonBacktracking(paragraphRegexes,inputText)
// no use benchmarking these on sbre because they're unoptimized
type Sbre1() = inherit Jobs.OnlySbre([ @"~(⊤*\n\n⊤*)" ], inputText)

// | C_None |       (?:.+(?:\n|\z))+(?:\n|\z) |  21.55 ms |  18.79 ms |  1.030 ms |  10.36 MB |
// | C_None |                     (?:.+\n)+\n |  17.33 ms |  10.84 ms |  0.594 ms |  10.36 MB |















