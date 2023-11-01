module Sbre.Benchmarks.ParagraphInner





let inputText =
    __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText

let shortInput = inputText[..9999] // 10k chars limit


let innerRegexes = [
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



let sampleParagraph =
    """
Well, there wasn't any way now to settle which was the greatest traveler;
some said it was Nat, some said it was Tom. Everybody allowed that Nat
had seen the most longitude, but they had to give in that whatever Tom
was short in longitude he had made up in latitude and climate. It was
about a stand-off; so both of them had to whoop up their dangerous
adventures, and try to get ahead THAT way. That bullet-wound in Tom's leg
was a tough thing for Nat Parsons to buck against, but he bucked the best
he could; and at a disadvantage, too, for Tom didn't set still as he'd
orter done, to be fair, but always got up and sauntered around and worked
his limp while Nat was painting up the adventure that HE had in
Washington; for Tom never let go that limp when his leg got well, but
practiced it nights at home, and kept it good as new right along.
"""

let sampleParagraph2 =
    """
We were pleasantly situated in a small two-storied inn, in an empty large
compound which was surrounded by a mud wall as high as a man's head.  The
inn was kept by nine Hindoo brothers, its owners.  They lived, with their
families, in a one-storied building within the compound, but off to one
side, and there was always a long pile of their little comely brown
children loosely stacked in its veranda, and a detachment of the parents
wedged among them, smoking the hookah or the howdah, or whatever they
call it.  By the veranda stood a palm, and a monkey lived in it, and led
a lonesome life, and always looked sad and weary, and the crows bothered
him a good deal.
"""


let words = [ "Tom"; "longitude"; "tough"; "right"; "adventures" ]


type None_2Words() =
    inherit
        Jobs.RuntimeInnerParagraph(
            words[0..1],
            sampleParagraph,
            System.Text.RegularExpressions.RegexOptions.None
        )

type None_All() =
    inherit
        Jobs.RuntimeInnerParagraph(
            words,
            sampleParagraph,
            System.Text.RegularExpressions.RegexOptions.None
        )

type NonBacktracking_3() =
    inherit
        Jobs.RuntimeInnerParagraph(
            words[..2],
            sampleParagraph,
            System.Text.RegularExpressions.RegexOptions.NonBacktracking
        )

type NonBacktracking_All() =
    inherit
        Jobs.RuntimeInnerParagraph(
            words,
            sampleParagraph,
            System.Text.RegularExpressions.RegexOptions.NonBacktracking
        )


// type NonBack1() = inherit Jobs.OnlyC_NonBacktracking(paragraphRegexes,inputText)
// // no use benchmarking these
type Sbre_2() = inherit Jobs.SbreInnerParagraph(words[..1], sampleParagraph)
type Sbre_All() = inherit Jobs.SbreInnerParagraph(words, sampleParagraph)
//
//


