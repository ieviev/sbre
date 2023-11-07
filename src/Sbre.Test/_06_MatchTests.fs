[<Xunit.Collection("Sequential")>]
module Sbre.Test._06_MatchTests

open Sbre
open Sbre.Algorithm
open Xunit



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




[<Fact>]
let ``lookarounds test 1``() =
    let matcher = Regex(""" Sep""")
    let mutable loc = Pat.Location.create "1 Sep" 1
    let ism = matcher.MatchFromLocation(&loc)

    Assert.True(ism.IsSome)

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
    let matcher = Regex(""".*(?<=aaa)""")
    let result = matcher.MatchText("aa")
    Assert.Equal(None, result)


[<Fact>]
let ``lookarounds test 6``() =
    let matcher = Regex(""".*(?=aaa)""")
    let result = matcher.MatchText("baaa")
    Assert.Equal(Some "b", result)


[<Fact>]
let ``lookarounds test 7``() =
    let matcher = Regex("""(?<=aaa).*""")
    let result = matcher.MatchText("aaabbb")
    Assert.Equal(Some "bbb", result)



[<Fact>]
let ``lookarounds test 8``() =
    let matcher = Regex("""(?<=aaa\{).*(?=\})""")
    let result = matcher.MatchText("aaa{bbb {ccc}}")
    Assert.Equal(Some "bbb {ccc}", result)


[<Fact>]
let ``lookarounds test 9``() =
    let matcher =
        Regex(
            """^(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])\.(\d{1,2}|1\d\d|2[0-4]\d|25[0-5])$"""
        )

    let result = matcher.MatchText("0.0.0.0")
    Assert.Equal(Some "0.0.0.0", result)



[<Fact>]
let ``caching lookarounds test``() =
    let matcher =
        Regex(
            """(^1300\d{6}$)|(^1800|1900|1902\d{6}$)|(^0[2|3|7|8]{1}[0-9]{8}$)|(^13\d{4}$)|(^04\d{2,3}\d{6}$)"""
        )

    let result = matcher.MatchText("1300333444")
    Assert.Equal(Some "1300333444", result)

[<Fact>]
let ``caching lookarounds test 2 ``() =
    let matcher = Regex("""(^\d{3,5}\,\d{2}$)|(^\d{3,5}$)""")
    let result = matcher.MatchText("1300333444")
    Assert.Equal(None, result)




[<Fact>]
let ``boundaries after``() =
    //testPartDerivativeFromLocation (@"(?!a)", "a", 1, "")
    let matcher = Regex("""a\b""")
    let result = matcher.MatchText("a ")
    Assert.Equal(Some "a", result)



[<Fact>]
let ``boundaries test 1``() =
    let matcher = Regex("""\b1\b""")
    let ism = matcher.IsMatch("1")
    Assert.True(ism)

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
    let matcher = Regex(@".*b|a")
    let ism = matcher.MatchText(" aaab ")
    Assert.Equal(Some " aaab", ism)



[<Fact>]
let ``exit range test 2``() =
    let matcher = Regex(@"a+")
    let ism = matcher.MatchText(" aaa ")
    Assert.Equal(Some "aaa", ism)


[<Fact>]
let ``inverted startset test 1``() =
    let matcher = Regex(@"..(?<=A.*)")
    let ism = matcher.MatchText("Aa")
    Assert.Equal(Some "Aa", ism)

[<Fact>]
let ``inverted startset test 2``() =
    let matcher = Regex(@"(?=.*A)(?=.*a)(?=.*1)...")
    let ism = matcher.MatchText("Aa1")
    Assert.Equal(Some "Aa1", ism)
    ()

[<Fact>]
let ``inverted startset test 3``() =
    let matcher = Regex(@"^((?=\S*?[A-Z])(?=\S*?[a-z])(?=\S*?[0-9]).{6,})\S$")
    let ism = matcher.MatchText("Aa11aBaAA")
    ()



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

[<Fact>]
let ``reverse pattern 2``() =
    let mutable startLocation = Pat.Location.create "1aA" 0

    let m = Regex(@"(?=.*A)(?=.*a)(?=.*1).{3,3}")
    let m_rev = Regex(@".{3,3}(?<=1.*)(?<=a.*)(?<=A.*)")

    let res = RegexNode.matchEnd (m.Cache, &startLocation, ValueNone, m.ReversePattern)
    stdout.WriteLine "AAAAAAAAAAAAA"
    // let res_rev = RegexNode.matchEnd (m_rev.Cache,startLocation , ValueNone, m_rev.RawPattern)
    // Assert.Equal(res_rev,res)
    ()




[<Fact>]
let ``negation range test 1``() =
    // let matcher = Matcher(@"~(.*\d\d⊤*)")
    let matcher = Regex(@"~(⊤*\d\d⊤*)")
    let result = matcher.MatchText("Aa11aBaAA")
    Assert.Equal(Some "Aa1", result)
    ()


[<Fact>]
let ``negation range test 2``() =
    let matcher = Regex(@"~(.*\d\d.*)")
    let result = matcher.MatchText("Aa11aBaAA")
    Assert.Equal(Some "Aa1", result)
    ()


[<Fact>]
let ``negation startset inference test``() =
    let matcher = Regex(@"a.*&~(.*b.*)b")
    // let matcher = Matcher(@"~(Lorem⊤*)")
    let result = matcher.MatchText("---a------bbb")
    Assert.Equal(Some "a------b", result)




[<Fact>]
let ``end with truestar test``() =
    let matcher = Regex("class=\"⊤*")
    // let matcher = Matcher(@"~(Lorem⊤*)")
    let input = @"class=""dasdasdsdasd"""
    let result = matcher.Match(input)
    // Assert.Equal(Some "a------b", result)
    Assert.Equal(input, result.Value)





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
let ``web app test 1``() =
    let input = webAppSample
    // let matcher = Matcher(@".*[a-z].*&.*[A-Z].*&.*\d.*&[a-zA-Z\d]{8,}")
    let matcher = Regex(@".*[a-z].*&[a-zA-Z\d]{8,}")
    let result = matcher.MatchPositions("y tej55zhA25wXu8bvQxFxt o") |> Seq.toArray
    Assert.Equal(1, result.Length)



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
    let matcher = Regex(@".*(?=.*E)&~(.*and.*)")
    let result = matcher.Match(@"___and__E")
    // todo: should return empty match too?
    Assert.Equal("___an", result.Value)



[<Fact>]
let ``web app test 5``() =
    let matcher = Regex(@"(?<=or=\{.*).*(?=.*\},)&~(⊤*and⊤*)&(\b.*\b)")
    let result = matcher.Matches(webAppSample2) |> Seq.map (fun v -> v.Value)

    Assert.Equal<string>(
        [|
            "De Vathaire, Florent "
            " Le Vu, B{\\'e}atrice "
            " Challeton-de Vathaire, C{\\'e}cile"
        |],
        result
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