[<Xunit.Collection("Sequential")>]
module Sbre.Test._06_MatchTests


#if DEBUG

open System.IO
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
let ``anchors test 1.1``() =
    let matcher = Regex("^\\d$")
    let ism = matcher.IsMatch("32")
    Assert.False(ism)

[<Fact>]
let ``anchors test 1.2``() =
    let matcher = Regex("^\\d$")
    let ism = matcher.IsMatch("324")
    Assert.False(ism)

[<Fact>]
let ``anchors test 1.3``() =
    let matcher = Regex("^\\d$")
    let ism = matcher.IsMatch("3245")
    Assert.False(ism)

[<Fact>]
let ``anchors test 1.4``() =
    let matcher = Regex("^\\d$")
    let ism = matcher.IsMatch("32455")
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
let ``anchors test 5 : nullability of anchors should not be cached``() =
    let matcher = Regex("^.{4,8}$")
    let ism = matcher.IsMatch("asd")
    Assert.False(ism)



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
let ``regexlib sample 1``() =
    let matcher = Regex(@"(\s|\n|^)(\w+://[^\s\n]+)")
    let ism = matcher.IsMatch("""<a href="http://acme.com">http://www.acme.com</a>""")
    Assert.False(ism)


[<Fact>]
let ``regexlib sample 2 - prefix test``() =
    let matcher = Regex(@"@( |)G( |)R( |)[a,A,@,(\/\\))]")
    let ism = matcher.IsMatch("""v1@G R /\""")
    Assert.True(ism)


[<Fact>]
let ``regexlib sample 3``() =
    // let matcher = Regex(@"(^0[1-9]\d{1}\s\d{4}\s?\d{4}$)|(^0[1-9]\d{2}\s\d{3}\s?\d{4}$)|(^0[1-9]\d{2}\s\d{4}\s?\d{3}$)|(^0[1-9]\d{3}\s\d{3}\s?\d{2}$)|(^0[1-9]\d{3}\s\d{3}\s?\d{3}$)|(^0[1-9]\d{4}\s\d{3}\s?\d{2}$)|(^0[1-9]\d{4}\s\d{2}\s?\d{3}$)|(^0[1-9]\d{4}\s\d{2}\s?\d{2}$)")
    let matcher = Regex(@"(^1300\d{6}$)|(^1800|1900|1902\d{6}$)|(^0[2|3|7|8]{1}[0-9]{8}$)|(^13\d{4}$)|(^04\d{2,3}\d{6}$)")
    // let matcher = Regex(@"^(((((0?[1-9])|(1\d)|(2[0-8]))\.((0?[1-9])|(1[0-2])))|((31\.((0[13578])|(1[02])))|((29|30)\.((0?[1,3-9])|(1[0-2])))))\.((20[0-9][0-9]))|(29\.0?2\.20(([02468][048])|([13579][26]))))$")
    // let ism = matcher.Match("01323 293 374")
    let ism = matcher.Match("0732105432")
    assertEqual ism.Value "0732105432"
    // let ism = matcher.Match("29.02.2004")
    // assertEqual ism.Value "01323 293 374"


[<Fact>]
let ``optimizations sanity check 1``() =
    let pat = @"a( |)b( |)c( |)d"
    let matcher = Regex(pat)
    let ism = matcher.IsMatch("""a b c d""")
    Assert.True(ism)



let sampleText1 = """asd asd down asdasd asd asd """

[<Fact>]
let ``empty loop test 1``() =
    let pat = @"~(⊤*(d⊤*){2})&.*down.*"
    assertNullablePositions pat sampleText1 [ 8; 7 ]



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


[<Fact>]
let ``lookarounds test 6``() =
    assertFirstMatchText """.*(?=aaa)""" "baaa" "b"



[<Fact>]
let ``lookarounds test 9``() =
    assertFirstMatchText
        """^(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])$"""
        "0.0.0.0"
        "0.0.0.0"


[<Fact>]
let ``caching lookarounds test 2 ``() =
    assertNoMatch """(^\d{3,5}\,\d{2}$)|(^\d{3,5}$)""" "1300333444"





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
let ``exit range test 2``() = assertFirstMatchText @"a+" " aaa " "aaa"







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
    assertFirstMatchText @"~(⊤*\d\d⊤*)" "Aa11aBaAA" "Aa1"

[<Fact>]
let ``negation range test 2``() =
    assertFirstMatchText @"~(.*\d\d.*)" "Aa11aBaAA" "Aa1"

[<Fact>]
let ``negation startset inference test 1``() =
    assertFirstMatchText @"a.*&~(.*b.*)b" "---a------bbb" "a------b"

[<Fact>]
let ``negation startset inference test 2``() =
    assertMatchEnd @"a.*&~(.*b.*)b" "---a------bbb" 3 11
// "a------b"


[<Fact>]
let ``end with truestar test``() =
    assertFirstMatchText "class=\"⊤*" @"class=""dasdasdsdasd""" @"class=""dasdasdsdasd"""


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


// [<Fact>]
// let ``web app test 5``() =
//     let result = getAllLLmatches (@"(?<=or=\{.*).*(?=.*\},)&~(⊤*and⊤*)&(\b.*\b)") webAppSample2
//     let matchTexts =
//         result
//         |> Seq.map _.GetText(webAppSample2)
//         |> Seq.toArray
//
//     Assert.Equal<string>(
//         [|
//             "De Vathaire, Florent "
//             ""
//             " Le Vu, B{\\'e}atrice "
//             ""
//             " Challeton-de Vathaire, C{\\'e}cile"
//             ""
//         |],
//         matchTexts
//     )

let sample3 =
    """
lethargy, and and the air tainted with
chloroform!  I saw two men in the room, and one was saying to the other,
in a hoarse whisper, 'I told her I would, if she made a noise, and as
for the child--'
half
consciousness out of a long, exhausting lethargy; when the first faint
balmy airs go wandering about, whispering the secret of the coming
change; when the abused brown grass, newly relieved of snow, seems
"""

let sample3s =
    """
lethargy, and and the air tainted with
c
"""


[<Fact>]
let ``startset 2 test small``() =
    let matcher = Regex(@"lethargy.*air")
    let result = matcher.Matches(sample3s) |> Seq.map (fun v -> v.Value) |> Seq.toArray

    Assert.Equal<string>([| "lethargy, and and the air" |], result)

[<Fact>]
let ``startset 2 test``() =
    let matcher = Regex(@"lethargy.*air")
    let result = matcher.Matches(sample3) |> Seq.map (fun v -> v.Value) |> Seq.toArray

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
        matcher.Matches("<!-- anything -- >") |> Seq.map (fun v -> v.Value) |> Seq.toArray

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
    Assert.Equal(3, llmatches.Count)



[<Fact>]
let ``negated end``() =
    assertFirstMatchText "F.*&~(.*Finn)" "Finn', published in 1885." "Finn', published in 1885."


[<Fact>]
let ``just loop``() =
    let result = getAllLLmatches "a*" "bbbbaaabbbbb"
    //  11 matches -> aaa has 2 matches
    Assert.Equal(11, result.Count)


[<Fact>]
let ``simple 1``() =
    let result = getAllLLmatches "..g" "dfdff dfggg gfgdfg gddfdf"
    Assert.Equal(3, result.Count)


[<Fact>]
let ``set star loop test 1``() =
    let result = getAllLLmatches "a*" "bbbb"
    Assert.Equal(5, result.Count)


[<Fact>]
let ``dfa match 2``() = assertFirstMatch ".*a{3}" "aa aaa" (0, 6)





let webappsample6 =
    """
The fists of all the generals came down this time, and again the
King's eye sparkled with pleasure. The Chancellor sprang to his
feet and appealed to his Majesty:

"Sire, I claim your protection."

But the King waved him to his seat again, saying:
"""

[<Fact>]
let ``web app test 6``() =
    // todo: check performance of this
    let result =
        getAllLLmatches """~(⊤*(\n⊤*){2})&⊤*g⊤*&~(⊤*")&[A-Za-z]{5}⊤*""" webappsample6

    Assert.Equal([ (5, 124); (212, 37) ], result |> Seq.map (fun v -> v.Index, v.Length))

let webappsample7 =
    """
remarked to Joan:

"Out of charity I will consider that you did not know who devised
this measure which you condemn in so candid language."

"Save your charity for another occasion, my
"""


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

let input =
    """[assembly: InternalsVisibleTo("Microsoft.Automata.Z3, PublicKey=" +

[assembly: InternalsVisibleTo("Experimentation, PublicKey=" +"""

[<Fact>]
let ``match order test``() =
    assertAllLLmatches "Pu" input [ 54, 2; 117, 2 ]




let input_dns =
    """
Jan 12 06:26:19: ACCEPT service http from 119.63.193.196 to firewall(pub-nic), prefix: "none" (in: eth0 119.63.193.196(5c:0a:5b:63:4a:82):4399 -> 140.105.63.164(50:06:04:92:53:44):80 TCP flags: ****S* len:60 ttl:32)
Jan 12 06:26:20: ACCEPT service dns from 140.105.48.16 to firewall(pub-nic-dns), prefix: "none" (in: eth0 140.105.48.16(00:21:dd:bc:95:44):4263 -> 140.105.63.158(00:14:31:83:c6:8d):53 UDP len:76 ttl:62)
Jan 12 06:27:09: DROP service 68->67(udp) from 216.34.211.83 to 216.34.253.94, prefix: "spoof iana-0/8" (in: eth0 213.92.153.78(00:1f:d6:19:0a:80):68 -> 69.43.177.110(00:30:fe:fd:d6:51):67 UDP len:576 ttl:64)"""


let reg1 =
    @"(?<=6|8\(|4|8|0\().*&~(.*\)\:.*)&\w.*&.*\w&.*(?=.*\)\:)&.*(?=\)\:|\)\:)"

[<Fact>]
let ``lookback 1``() =
    assertAllLLmatches reg1 input_dns [
        (48, 89)
        (140, 39)
        (260, 94)
        (357, 39)
        (451, 114)
        (568, 36)
    ]

let reg2 =
    @"(?<=6|8\(.*).*&(?<=6|8\(|4|8|0\().*&~(.*\)\:.*)&\w.*&.*\w&.*(?=.*\)\:)&.*(?=\)\:|\)\:)"


[<Fact>]
let ``lookback 2``() =
    assertAllLLmatches reg2 input_dns [

        // todo which is correct?
        (48, 89);(156, 23);(359, 37);(451, 114);(577, 27)
        // (48, 89);(156, 23);(359, 37);(451, 114);(568, 36)
    ]

// 49

// [<Fact>]
// let ``lookback 3``() = assertMatchEnd reg2 input_dns 49 0

// -2
[<Fact>]
let ``lookback 3``() = assertMatchEnd reg1 input_dns 48 137
// [<Fact>] let ``lookback 3``() = assertMatchEnd reg1 input_dns 48 0



[<Fact>]
let ``learning sample 1``() =
    assertAllLLmatches """<h.{1,60}>.*<\/h(5|3|2|1|4)>""" """<li><a href="/wiki/Lattes_Editori" title="Lattes Editori">Lattes Editori</a></li>""" [

    ]

[<Fact>]
let ``learning sample 2``() =
    assertAllLLmatches """<h.{1,60}>.*<\/h(5|3|2|1|4)>""" (File.ReadAllText(__SOURCE_DIRECTORY__ + "/data/sample-html.html")) [
        (0, 41);(1983, 41);(2175, 41);(13072, 41);(18595, 41)
    ]
    // Assert.Equal([ (5, 124); (212, 37) ], result |> Seq.map (fun v -> v.Index, v.Length))


[<Fact>]
let ``invalid match 1``() =
    assertAllLLmatches
        """\s[a-zA-Z]{0,12}ing\s"""
        """
--Something about the Azores Islands--Blucher's D
fsdf
sdf
"""

        [

    ]





#endif
