[<Xunit.Collection("Sequential")>]
module Sbre.Test._06_MatchTests


#if DEBUG

open Sbre
open Sbre.Algorithm
open Xunit
open Common



[<Fact>]
let ``anchors test 1``() =
    let matcher = Regex(@"^\d$")
    let ism = matcher.IsMatch("1")
    Assert.True(ism)


[<Fact>]
let ``anchors test 2``() =
    let matcher = Regex("^\\d$")
    let ism = matcher.IsMatch("324")
    Assert.False(ism)

[<Fact>]
let ``anchors test 3``() =
    let matcher = Regex("^\\d$")
    let ism = matcher.IsMatch("a")
    Assert.False(ism)


[<Fact>]
let ``anchors test 4 : nullability of anchors should not be cached``() =
    let matcher = Regex("^\\d*$")
    let ism = matcher.IsMatch("123")
    Assert.True(ism)



[<Fact>]
let ``multi-nodes ordering test 1``() =
    let matcher = Regex(@"q[\d\D]*q")
    let ism = matcher.IsMatch("q my comment q")
    Assert.True(ism)

[<Fact>]
let ``multi-nodes ordering test 2``() =
    let matcher = Regex(@"/\*[\d\D]*?\*/")
    let ism = matcher.IsMatch("/* my comment */")
    Assert.True(ism)




[<Fact>]
let ``lots of captures test``() =
    let matcher = Regex("""((\d{2})|(\d))\/((\d{2})|(\d))\/((\d{4})|(\d{2}))""")

    let ism = matcher.IsMatch("4/05/89")
    Assert.True(ism)



[<Fact>]
// fails if ? is turned to epsilon
let ``phone number test 1``() =
    let pattern = """^(\(?\+?[0-9]*\)?)?[0-9_\- \(\)]*$"""
    let input = "(+44)(0)20-12341234"
    let matcher = Regex(pattern)
    let ism = matcher.IsMatch(input)
    Assert.True(ism)


[<Fact>]
let ``phone number test 2``() =
    let pattern =
        """^([0-9]( |-)?)?(\(?[0-9]{3}\)?|[0-9]{3})( |-)?([0-9]{3}( |-)?[0-9]{4}|[a-zA-Z0-9]{7})$"""

    let input = "1-(123)-123-1234"
    let matcher = Regex(pattern)
    let ism = matcher.IsMatch(input)
    Assert.True(ism)


[<Fact>]
// fails if ? is turned to epsilon
let ``top level or remove in correct order``() =
    let pattern = """\d{1,3}.?\d{0,3}\s[a-zA-Z]{2,30}\s[a-zA-Z]{2,15}"""
    let input = "65 Beechworth/ Rd"
    let matcher = Regex(pattern)
    let ism = matcher.IsMatch(input)
    Assert.False(ism)




// [<Fact>]
// let ``lookarounds test 1``() =
//     assertFirstMatch """ Sep""" """ Sep""" (0,5)
//

[<Fact>]
let ``lookarounds test 2``() =
    let matcher = Regex("""1(?! Sep)""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.False(ism)


[<Fact>]
let ``lookarounds test 3``() =
    let matcher = Regex("""^(1(?! (Sep))).*$""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.False(ism)

[<Fact>]
let ``lookarounds test 4``() =
    let matcher = Regex("""^(1(?= (Sep))).*$""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.True(ism)


[<Fact>]
let ``lookarounds test 5``() =
    assertNoMatch """.*(?<=aaa)""" "aa"


[<Fact>]
let ``lookarounds test 6``() =
    assertFirstMatchText """.*(?=aaa)""" "baaa" "b"


[<Fact>]
let ``lookarounds test 7``() =
    assertFirstMatchText """(?<=aaa).*""" "aaabbb" "bbb"


[<Fact>]
let ``lookarounds test 8``() =
    assertFirstMatchText """(?<=aaa\{).*(?=\})""" "aaa{bbb {ccc}}" "bbb {ccc}"


[<Fact>]
let ``lookarounds test 9``() =
    assertFirstMatchText
        """^(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])$"""
        "0.0.0.0"
        "0.0.0.0"


[<Fact>]
let ``caching lookarounds test 2 ``() =
    assertNoMatch
        """(^\d{3,5}\,\d{2}$)|(^\d{3,5}$)"""
        "1300333444"




[<Fact>]
let ``boundaries after``() =
    assertFirstMatchText
        """a\b"""
        "a "
        "a"



[<Fact>]
let ``boundaries test 1``() =
    assertFirstMatchText
        """\b1\b"""
        "1"
        "1"

[<Fact>]
let ``boundaries test 1-2``() =
    let matcher = Regex("""\b1\b""")
    let ism = matcher.IsMatch(" 1")
    Assert.True(ism)

[<Fact>]
let ``boundaries test 1-3``() =
    let matcher = Regex("""\b1\b""")
    let ism = matcher.IsMatch("1 ")
    Assert.True(ism)


[<Fact>]
let ``boundaries test 2``() =
    let matcher = Regex("""\b1\b""")
    let ism = matcher.IsMatch(" 1 ")
    Assert.True(ism)


[<Fact>]
let ``boundaries test 3``() =
    let matcher = Regex("""1\b """)
    let ism = matcher.IsMatch("1 ")
    Assert.True(ism)

[<Fact>]
let ``boundaries test 4``() =
    let matcher = Regex("""1\b-""")
    let ism = matcher.IsMatch("1-")
    Assert.True(ism)


[<Fact>]
let ``boundaries test 5``() =
    let matcher = Regex("""\b-""")
    let ism = matcher.IsMatch("1-2")
    Assert.True(ism)

[<Fact>]
let ``boundaries test 6``() =
    let matcher = Regex(@"1\b-")
    let ism = matcher.IsMatch("1-2")
    Assert.True(ism)

[<Fact>]
let ``boundaries test 7``() =
    let matcher = Regex("""1\b-2""")
    let ism = matcher.IsMatch("1-2")
    Assert.True(ism)





[<Fact>]
let ``lots of alternations test``() =
    let matcher =
        Regex(
            """[du]{2}|[gu]{2}|[tu]{2}|[ds]{2}|[gs]{2}|[da]{2}|[ga]{2}|[ta]{2}|[dq]{2}|[gq]{2}|[tq]{2}|[DU]{2}|[GU]{2}|[TU]{2}|[DS]{2}|[GS]{2}|[DA]{2}|[GA]{2}|[TA]{2}|[DQ]{2}|[GQ]{2}|[TQ]{2}"""
        )

    let ism = matcher.IsMatch("DU")
    Assert.True(ism)

[<Fact>]
let ``lots of alternations test 2``() =
    // let matcher = Matcher("""^(6[0-4]\d\d\d|65[0-4]\d\d|655[0-2]\d|6553[0-5])$""")
    let matcher = Regex("""^(6[0-4]\d\d\d|65[0-4]\d\d|655[0-2]\d|6553[0-5])$""")
    let ism = matcher.IsMatch("65535")
    Assert.True(ism)



[<Fact>]
let ``lots of alternations test 3``() =
    // let matcher = Matcher("""^(6[0-4]\d\d\d|65[0-4]\d\d|655[0-2]\d|6553[0-5])$""")
    let matcher =
        Regex(
            """^((\d{2}(([02468][048])|([13579][26]))[\-\/\s]?((((0?[13578])|(1[02]))[\-\/\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\-\/\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\-\/\s]?((0?[1-9])|([1-2][0-9])))))|(\d{2}(([02468][1235679])|([13579][01345789]))[\-\/\s]?((((0?[13578])|(1[02]))[\-\/\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\-\/\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\-\/\s]?((0?[1-9])|(1[0-9])|(2[0-8]))))))(\s(((0?[1-9])|(1[0-2]))\:([0-5][0-9])((\s)|(\:([0-5][0-9])\s))([AM|PM|am|pm]{2,2})))?$"""
        )

    let ism = matcher.IsMatch("2004-2-29")
    Assert.True(ism)



//^((0?[13578]\.)|(0?[13456789]\.))$


[<Fact>]
let ``exit range test 1``() =
    assertFirstMatchText @".*b|a" " aaab " " aaab"


[<Fact>]
let ``exit range test 2``() =
    assertFirstMatchText @"a+" " aaa " "aaa"


[<Fact>]
let ``inverted startset test 1``() =
    assertFirstMatchText @"..(?<=A.*)" "Aa" "Aa"

[<Fact>]
let ``inverted startset test 2``() =
    assertFirstMatchText
        @"(?=.*A)(?=.*a)(?=.*1)..."
        "Aa1"
        "Aa1"



[<Fact>]
let ``reverse pattern 1``() =
    let startLocation = Pat.Location.create "1aA" 0


    // let res = RegexNode.matchEnd (m.Cache,startLocation , ValueNone, m.ReversePattern)

    // let m_rev = Matcher(@".{3,3}(?<=1.*)(?<=a.*)(?<=A.*)")
    // let p1 = m_rev.RawPattern
    //
    // let m = Matcher(@"(?=.*A)(?=.*a)(?=.*1).{3,3}")
    // let p2 = m.ReversePattern

    // let res_rev = RegexNode.matchEnd (m_rev.Cache,startLocation , ValueNone, m_rev.RawPattern)
    let a = 1
    // Assert.Equal(p2,p1)
    ()

// [<Fact>]
// let ``reverse pattern 2``() =
//     let mutable startLocation = Pat.Location.create "1aA" 0
//     let m = Regex(@"(?=.*A)(?=.*a)(?=.*1).{3,3}")
//     let m_rev = Regex(@".{3,3}(?<=1.*)(?<=a.*)(?<=A.*)")
//     let res = m_rev.FindMatchEnd("1aA")
//     ()




[<Fact>]
let ``negation range test 1``() =
    assertFirstMatchText
        @"~(⊤*\d\d⊤*)"
        "Aa11aBaAA"
        "Aa1"

[<Fact>]
let ``negation range test 2``() =
    assertFirstMatchText
        @"~(.*\d\d.*)"
        "Aa11aBaAA"
        "Aa1"

[<Fact>]
let ``negation startset inference test``() =
    assertFirstMatchText
        @"a.*&~(.*b.*)b"
        "---a------bbb"
        "a------b"


[<Fact>]
let ``end with truestar test``() =
    assertFirstMatchText
        "class=\"⊤*"
        @"class=""dasdasdsdasd"""
        @"class=""dasdasdsdasd"""


[<Fact>]
let ``line loop test``() =
    let input = "\naaa\n\nbbb\n\nccc\n\n"
    let matcher = Regex(@"(?:.+\n)+\n")
    // let result = matcher.MatchPositions(input) |> Seq.toArray
    let result = matcher.MatchPositions(input) |> Seq.toArray
    // Assert.Equal(Some "a------b", result)
    Assert.Equal(3, result.Length)








let webAppSample =
    "Lorem Ipsum is simply dummy tej55zhA25wXu8bvQxFxt of the printing and typesetting industry.
Lorem Ipsum iHK3khIUTQYxHx9r has been the Aa11aBaAA standard dfgI51d7ZPhOwGwI2vpcdfgr since the 1500s,
when an unknown versions of Lorem Ipsum.
"



[<Fact>]
let ``web app debug``() =
    let matcher = Regex(@"~(.*\d\d.*)&[a-zA-Z\d]{8,}")
    let result = matcher.MatchPositions("y tej55zhA25wXu8bvQxFxt o") |> Seq.toArray
    Assert.Equal(1, result.Length)



[<Fact>]
let ``web app test 1``() =
    let result = getAllLLmatches (@".*[a-z].*&[a-zA-Z\d]{8,}") ("y tej55zhA25wXu8bvQxFxt o")
    Assert.Equal(1, result.Count)



[<Fact>]
let ``web app test 2``() =
    let input = webAppSample
    let matcher = Regex(@".*[a-z].*&.*[A-Z].*&.*\d.*&[a-zA-Z\d]{8,}&~(.*\d\d.*)")
    let result = matcher.MatchPositions("y tej55zhA25wXu8bvQxFxt o") |> Seq.toArray
    Assert.Equal(1, result.Length)


[<Fact>]
let ``web app test 3``() =
    let input = webAppSample

    let matcher =
        Regex(@"\((⊤*A⊤*B⊤*C⊤*|⊤*A⊤*C⊤*B⊤*|⊤*B⊤*A⊤*C⊤*|⊤*B⊤*C⊤*A⊤*|⊤*C⊤*A⊤*B⊤*|⊤*C⊤*B⊤*A⊤*)\)")

    let result = matcher.MatchPositions("(A----B----C)") |> Seq.toArray
    Assert.Equal(1, result.Length)

let webAppSample2 =
    @"@article{de2000thyroid,
  title={Thyroid cancer in French Polynesia between 1985 and 1995: influence of atmospheric nuclear bomb tests performed at Mururoa and Fangataufa between 1966 and 1974},
  author={De Vathaire, Florent and Le Vu, B{\'e}atrice and Challeton-de Vathaire, C{\'e}cile},
  journal={Cancer Causes \& Control},
  volume={11},
  number={1},
  pages={59--63},
  year={2000},
  publisher={Springer}
}"


[<Fact>]
let ``web app test 4.1``() =
    assertFirstMatchText ((@".*(?=.*E)&~(.*and.*)")) @"___and__E" "___an"



[<Fact>]
let ``web app test 5``() =
    let result = getAllLLmatches (@"(?<=or=\{.*).*(?=.*\},)&~(⊤*and⊤*)&(\b.*\b)") webAppSample2
    let matchTexts =
        result
        |> Seq.map _.GetText(webAppSample2)
        |> Seq.toArray

    // TODO: sure?
    Assert.Equal<string>(
        [|
            "De Vathaire, Florent "
            ""
            " Le Vu, B{\\'e}atrice "
            ""
            " Challeton-de Vathaire, C{\\'e}cile"
            ""
        |],
        matchTexts
    )

let sample3  = """
lethargy, and and the air tainted with
chloroform!  I saw two men in the room, and one was saying to the other,
in a hoarse whisper, 'I told her I would, if she made a noise, and as
for the child--'
half
consciousness out of a long, exhausting lethargy; when the first faint
balmy airs go wandering about, whispering the secret of the coming
change; when the abused brown grass, newly relieved of snow, seems
"""

let sample3s  = """
lethargy, and and the air tainted with
c
"""


[<Fact>]
let ``startset 2 test small``() =
    let matcher = Regex(@"lethargy.*air")
    let result =
        matcher.Matches(sample3s)
        |> Seq.map (fun v -> v.Value)
        |> Seq.toArray

    Assert.Equal<string>( [| "lethargy, and and the air" |], result )

[<Fact>]
let ``startset 2 test``() =
    let matcher = Regex(@"lethargy.*air")
    let result =
        matcher.Matches(sample3)
        |> Seq.map (fun v -> v.Value)
        |> Seq.toArray

    Assert.Equal<string>(
        [|
            // if true then _solver.Full else
            "lethargy, and and the air"

        |],
        result
    )


[<Fact>]
let ``startset 3 test``() =
    let matcher = Regex(@"⊤*have⊤*&⊤*there⊤*&.*")
    let result =
        matcher.Matches("to have sdfgfs there fdsgf \n asddf have\n dfgsdf there have \n fsdgf")
        |> Seq.map (fun v -> v.Value)
        |> Seq.toArray

    ()

[<Fact>]
let ``out of range test``() =
    let matcher = Regex(@"<!--[\s\S]*--[ \t\n\r]*>")
    let result =
        matcher.Matches("<!-- anything -- >")
        |> Seq.map (fun v -> v.Value)
        |> Seq.toArray

    ()


[<Fact>]
let ``ensure negation prevents match``() =
    assertNoMatch
        ".*which.*&.*could.*&.*that.*&~(.*the.*)"
        "could only partly conceal the ravages which that long siege of storms had"

[<Fact>]
let ``single char in negation``() =
    assertFirstMatchText
        ".*Huck.*&~(.*F.*)"
        "The Adventures of Huckleberry Finn', published in 1885."
        "The Adventures of Huckleberry "



let abc =
    """
The fists of all the generals came down this time, and again the
King's eye sparkled with pleasure. The Chancellor sprang to his

"Save your charity for another occasion, my lord," said Joan, as
calmly as before. "Whenever anything is done to injure the
interests and degrade the honor of France, all but the dead know
how to name the two conspirators-in-chief--"
"""


[<Fact>]
let ``index out of bounds test``() =
    let llmatches = getAllLLmatches @"~(⊤*\n\n⊤*)" abc
    Assert.Equal( 3, llmatches.Count )



[<Fact>]
let ``negated end``() =
    assertFirstMatchText
        "F.*&~(.*Finn)"
        "Finn', published in 1885."
        "Finn', published in 1885."


[<Fact>]
let ``just loop``() =
    let result = getAllLLmatches "a*" "bbbbaaabbbbb"
    //  11 matches -> aaa has 2 matches
    Assert.Equal( 11, result.Count )


[<Fact>]
let ``simple 1``() =
    let result = getAllLLmatches "..g" "dfdff dfggg gfgdfg gddfdf"
    Assert.Equal( 3, result.Count )


[<Fact>]
let ``set star loop test 1``() =
    let result = getAllLLmatches "a*" "bbbb"
    Assert.Equal( 5, result.Count )


[<Fact>]
let ``dfa match 2``() =
    assertFirstMatch ".*a{3}" "aa aaa" (0,6)


//
// [<Fact>]
// let ``dfa match 3``() =
//     let regex = Regex(@"~(⊤*\d\d⊤*)")
//     let matcher = regex.TSetMatcher
//     assertAllStates regex "aa11aaa" [
//         [ @"⟨⊤*~(⟨⊤*⟨\d{2,2}⊤*⟩⟩)⟩" ]
//         [ @"⟨⊤*~(⟨⊤*⟨\d{2,2}⊤*⟩⟩)⟩" ]
//         [ @"⟨⊤*~(⟨⊤*⟨\d{2,2}⊤*⟩⟩)⟩"; @"~(⟨⊤*⟨\d{2,2}⊤*⟩⟩)" ]
//         [ @"⟨⊤*~(⟨⊤*⟨\d{2,2}⊤*⟩⟩)⟩" ]
//         [ @"⟨⊤*~(⟨⊤*⟨\d{2,2}⊤*⟩⟩)⟩" ]
//     ]


// [<Fact>]
// let ``dfa match 4``() = assertDfaMatchEnds "..a" "_a__" []
//
// [<Fact>]
// let ``dfa match 5``() = assertDfaMatchEnds "..a" "__a__" [3]
//
// [<Fact>]
// let ``dfa match 6``() = assertDfaMatchEnds "..a" "___a__" [4]
//
// [<Fact>]
// let ``dfa match 7``() = assertDfaMatchEnds "..a" "___a__" [4]

// [<Fact>]
// let ``llmatch 1``() =
//     let regex = Regex("abc")
//     let matcher = regex.TSetMatcher
//
//     let result =
//         matcher.LLMatchPositions("__abc__")
//         |> Seq.toArray
//     Assert.Equal([4; 7; 12], result |> Seq.map (fun v -> v.Index) )



#endif