[<Xunit.Collection("Sequential")>]
module Sbre.Test.CaptureTests

open Sbre
open Sbre.Types
open Xunit

#if DEBUG


[<Literal>]
let RegexLibSamplesPath = __SOURCE_DIRECTORY__ + "/data/regexlibpatterns.json"

type RegexLibProvider = FSharp.Data.JsonProvider<RegexLibSamplesPath,InferTypesFromValues=false>

let regexlibSamples = RegexLibProvider.GetSamples()


let paragraphs_short = """
The fists of all the generals came down this time, and again the
King's eye sparkled with pleasure. The Chancellor sprang to his
feet and appealed to his Majesty:

"Sire, I claim your protection."

But the King waved him to his seat again, saying:

"Peace. She had a right to be consulted before that thing was
undertaken, since it concerned war as well as politics. It is but just
that she be heard upon it now."

The Chancellor sat down trembling with indignation, and
remarked to Joan:

"Out of charity I will consider that you did not know who devised
this measure which you condemn in so candid language."

"Save your charity for another occasion, my lord," said Joan, as
calmly as before. "Whenever anything is done to injure the
interests and degrade the honor of France, all but the dead know
how to name the two conspirators-in-chief--"
"""




let text =
    """type Context1 =
    get, set
    get, set

type Context2 =
    get, set
    get, set
    get, set
"""



[<Fact>]
let ``replace 1`` () =
    let r = Sbre.Regex("get")
    let expectedOutput = """type Context1 =
    hello, set
    hello, set

type Context2 =
    hello, set
    hello, set
    hello, set
"""
    let result = r.Replace(text, "hello")
    Assert.Equal(expectedOutput, result)


[<Fact>]
let ``replace 2`` () =
    let r = Sbre.Regex("a")
    let input = """a a a a a"""
    let expectedOutput = """ab ab ab ab ab"""
    let result = r.Replace(input, "$0b")
    Assert.Equal(expectedOutput, result)

[<Fact>]
let ``replace 3`` () =
    let pat = String.concat "&" [
        @"(?<=\T*(\n\n\T*){3})\T*"
        @"\b\w+\b"
        @".*e.*"
    ]
    let r = Sbre.Regex(pat)
    let result = r.Replace(paragraphs_short, "hello")
    ()


let escapeNegConj (str:string) = str.Replace("&",@"\&").Replace("~",@"\~")
let testCapture0InRange fromRange toRange =
    let mutable counter = fromRange
    for entry in regexlibSamples[fromRange..toRange] do
        counter <- counter + 1
        let pattern = entry.Pattern
        // escape ~ and & in pattern
        let pattern = escapeNegConj pattern

        // let matcher = Regex(pattern)
        let runtime = System.Text.RegularExpressions.Regex(pattern)

        // testing only matches
        for isMatch in entry.Matches do

            try
                let result =
                    Common.getFirstLLmatch pattern (isMatch) |> (fun (s,e) -> isMatch[s..s+e] )
                let result2 = runtime.Match(isMatch).Value
                Assert.True((result = result2), $"should be the same: {entry.Title}\n{pattern}\n{isMatch}\nmyregex:\n{result}\nruntime:\n{result2}")
            with
                | :? UnsupportedPatternException as v -> ()
                | e -> failwith e.Message


[<Fact>]
let ``captures 001-010`` () = testCapture0InRange 0 10

[<Fact>]
let ``captures 011-020`` () = testCapture0InRange 11 20

// [<Fact>] // test later
// let ``captures 021-030`` () = testCapture0InRange 21 30

[<Fact>]
let ``captures 031-040`` () = testCapture0InRange 31 40

// skip
// [<Fact>]
// let ``captures 041-050`` () = testCapture0InRange 41 50

// skip 51-60

// different semantics
// [<Fact>]
// let ``captures 061-070`` () = testCapture0InRange 61 70

// skip 71-80

// [<Fact>]
// let ``captures 081-090`` () = testCapture0InRange 81 90

// [<Fact>] // test later
// let ``captures 091-100`` () = testCapture0InRange 91 100

// skip 101-161

// [<Fact>]  // test later
// [<Fact>]
// let ``captures 141-150`` () = testCapture0InRange 141 150

// [<Fact>]  // test later
// let ``captures 151-160`` () = testCapture0InRange 151 160

// [<Fact>]
// let ``captures 161-170`` () = testCapture0InRange 161 170
//
// [<Fact>]
// let ``captures 171-180`` () = testCapture0InRange 171 180


// skip 181: backreference

[<Fact>]
let ``captures 191-200`` () = testCapture0InRange 191 200


// [<Fact>]
// let ``captures 201-230`` () = testCapture0InRange 201 230

// different semantics
// [<Fact>]
// let ``captures 231-240`` () = testCapture0InRange 231 240



// unsupported regexes


let testSameAsRuntime = _07_ComparisonTests.testSameAsRuntime
let assertNodeWithoutPrefix = _02_NodeTests.assertNodeWithoutPrefix
open Common

[<Fact>]
let ``unsupported 01``() =
    let pattern = """^((31(?!\ (Feb(ruary)?|Apr(il)?|June?|(Sep(?=\b|t)t?|Nov)(ember)?)))|((30|29)(?!\ Feb(ruary)?))|(29(?=\ Feb(ruary)?\ (((1[6-9]|[2-9][0-9])(0[48]|[2468][048]|[13579][26])|((16|[2468][048]|[3579][26])00)))))|(0?[1-9])|1[0-9]|2[0-8])\ (Jan(uary)?|Feb(ruary)?|Ma(r(ch)?|y)|Apr(il)?|Ju((ly?)|(ne?))|Aug(ust)?|Oct(ober)?|(Sep(?=\b|t)t?|Nov|Dec)(ember)?)\ ((1[6-9]|[2-9][0-9])[0-9]{2})$"""
    let input = "31 September 2003"
    testSameAsRuntime pattern input

[<Fact>]
let ``unsupported 02``() =
    let pattern = """^(1(?= ((Sept?)(em)?)) Sept? 1)$"""
    let input = "1 Sept 1"
    testSameAsRuntime pattern input


[<Fact>]
let ``unsupported 03``() =
    let pattern = """^(1(?! ((Sep(?=\b|t)t?|Nov)(ember)?))).*$"""
    let input = "31 September 2003"
    testSameAsRuntime pattern input


[<Fact>]
let ``unsupported 04``() =
    let matcher = Regex("""^(a(?!b)).*$""")
    let ism = matcher.IsMatch("ab")
    Assert.False(ism)
    // printAllDerivatives """^(a(?!b)).*$""" "ab" []


[<Fact>]
let ``unsupported 05``() =
    assertFirstMatchText @".*(?<=a)" "aaa" "aaa"


[<Fact>]
let ``unsupported 06``() =
    let matcher = Regex("""^(1(?! (Sep))).*$""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.False(ism)

[<Fact>]
let ``unsupported 07``() =
    let matcher = Regex("""^(1(?= (Sep))).*$""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.True(ism)

[<Fact>]
let ``unsupported 08``() = assertRawDerivative """⊤*\ba""" "a " [
    @"(⊤*((?<=φ)|\A)a)?"
    @"(⊤*((?<=φ)|\A)a|ε)"
    @"(⊤*(\A|(?<=φ))a|ε)"
    @"(ε|⊤*((?<=φ)|\A)a)"
    @"(ε|⊤*(\A|(?<=φ))a)"
    @"(⊤*(\A|(?<=φ))a)?"
    // --
    @"(⊤*(?<=(\A|φ))a)?"
    @"(⊤*(?<=(φ|\A))a)?"
    // --
    @"(⊤*(\A|φ)a)?"
    @"(⊤*(φ|\A)a)?"
]

[<Fact>]
let ``unsupported 09``() =
    assertNodeWithoutPrefix """(\s+)?((\bmittags?|((in der )?nachts?)))""" [
        // "φ*((in der )?nacht|mittag)s?"
        // @"φ*(mittag|(in der )?nacht)s?"
        @"φ*((in der )?nacht|mit{2,2}ag)s?"
        @"φ*(mit{2,2}ag|(in der )?nacht)s?"
    ]

[<Fact>]
let ``unsupported 10``() =
    let pattern = """\d(?=.*a)\d\d"""
    let input = "123___a"
    assertIsMatch pattern input

[<Fact>]
let ``unsupported 11``() =
    let pattern = """\d(?=.*a)\d.\d"""
    let input = "12a3___"
    assertFirstMatchText pattern input "12a3"

[<Fact>]
let ``unsupported 12``() =
    let pattern = """\d(?=.*a)\d.\d"""
    let input = "12_3a___"
    assertFirstMatchText pattern input "12_3"


[<Fact>]
let ``unsupported 13``() =
    let pattern = """~(^0*$)&~(^0*\.0*$)&^\d{1,5}(\.\d{1,3})?$"""
    let input = "12345.123"
    assertFirstMatchText pattern input "12345.123"



[<Fact>]
let ``unsupported 14``() = assertConverted """\b\d{5}(?:-\d{4})?\b""" [
    @"((?<=φ)|\A)φ{5,5}(-φ{4,4})?((?=φ)|\z)"
    @"(\A|(?<=φ))φ{5,5}(-φ{4,4})?(\z|(?=φ))"
    @"(\A|(?<=φ))φ{5,5}(-φ{4,4})?((?=φ)|\z)"

    @"(?<=(φ|\A))φ{5,5}(-φ{4,4})?(?=(φ|\z))"
    @"(?<=(\A|φ))φ{5,5}(-φ{4,4})?(?=(φ|\z))"
    @"(?<=(φ|\A))φ{5,5}(-φ{4,4})?(?=(\z|φ))"
    @"(?<=(\A|φ))φ{5,5}(-φ{4,4})?(?=(\z|φ))"
]




//
[<Fact>]
let ``rex sample 01.1``() =
    assertIsMatch
        "android-ndk-r(?<ver>\d+)(?<tag>\w*)-\w*"
        @"㏨android-ndk-r᪈-"


[<Fact>]
let ``rex sample 01.2``() =
    assertMatchEnd
        "1+\w*-\w*"
        @"1-"
        0 2


[<Fact>]
let ``rex sample 02.1``() =
    assertIsMatch
        @"(?<keep>[^aeiou])ies$"
        @"솱ies"

[<Fact>]
let ``rex sample 02.2``() =
    assertIsMatch
        @"[^aeiou]ies$"
        @"솱ies"

[<Fact>]
let ``rex sample 02.3``() =
    assertMatchEnd
        @"[^aeiou]ies$"
        @"솱ies"
        0 4

[<Fact>]
let ``rex sample 03.1``() =
    assertSameAsRuntime
        @"
[ 	]"
        @"║
 㩜昏"

[<Fact>]
let ``rex sample 03.2``() =
    assertIsMatch
        @"
[ 	]"
        @"║
 㩜昏"



[<Fact>]
let ``rex sample 04.1``() =
    assertIsMatch
        @"((a\b|na)\s+qqqq)"
        @"a qqqq"

[<Fact>]
let ``rex sample 04.2``() =
    assertIsMatch
        @"(a\b\s+qqqq)"
        @"a qqqq"


[<Fact>]
let ``rex sample 05.1``() =
    assertIsMatch
        @"((am|gegen|in der)\s+)?((nachmittags?|abends?|mitternachts?|\bmittags?|((in der )?nachts?)))"
        @"am mittags"

[<Fact>]
let ``rex sample 05.2``() =
    assertIsMatch
        @"((am|gegen|in der)\s+)?((\bmittags?|((in der )?nachts?)))"
        @"am mittags"

[<Fact>]
let ``rex sample 05.3``() =
    assertIsMatch
        @"(\s+)?((\bmittags?|((in der )?nachts?)))"
        @" mittags"


[<Fact>]
let ``rex sample 05.4``() =
    assertMatchEnd
        @"(\s+)?((\bmittags?|((in der )?nachts?)))"
        @" mittags"
        1 8


[<Fact>]
let ``rex sample 06.1``() =
    assertIsMatch
        @"\A(?:(?:http|https):\/\/)?([-a-zA-Z0-9.]{2,256}\.[a-z]{2,4})\b(?:\/[-a-zA-Z0-9@:%_\+.\~#?\&//=]*)?"
        @"http://hGy8s.wzzu�䪦൯"



[<Fact>]
let ``rex sample 06.2``() =
    assertIsMatch
        @"([-a-zA-Z0-9.]{2,}\.[a-z]{2,4})\b(?:\/[-a-zA-Z0-9@:%_\+.\~#?\&//=]*)?"
        @"http://hGy8s.wzzu�䪦൯"


[<Fact>]
let ``rex sample 07.1``() =
    assertIsMatch
        @"\(?\b([0-9]{3})\)?[\s]?[-.●]?[\s]?([0-9]{3})[\s]?[-.●]?[\s]?([0-9]{4})\b"
        @"�988)　602 5891꓏엩"

[<Fact>]
let ``rex sample 07.2``() =
    assertIsMatch
        @"\(?\b([0-9]{3})\)?[\s]?[-.●]?[\s]?([0-9]{3})[\s]?"
        @"�988)　602 5891꓏엩"

[<Fact>]
let ``rex sample 07.3``() =
    assertIsMatch
        @"\(?\b([0-9]{3})\)?[\s]?[-.●]?[\s]?"
        @"�988)　602 5891꓏엩"

[<Fact>]
let ``rex sample 08.1``() =
    assertIsMatch
        @"((ht|f)tp(s?)\:\/\/|www\.)[-a-zA-Z0-9@:%._\+~#=]{1,256}\.(?<Tld>[a-zA-Z0-9()]{1,6})\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)"
        @"ftps://Z.)꼑"

[<Fact>]
let ``rex sample 08.2``() =
    assertIsMatch
        @"[-a-zA-Z0-9@:%._\+~#=]{1,256}\.(?<Tld>[a-zA-Z0-9()]{1,6})\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)"
        @"Z.)꼑"

[<Fact>]
let ``rex sample 09.1``() =
    assertIsMatch
        @"\b(da(l(l[oae'])?|i|gli)?).+(a(l(l[oae'])?|i|gli)?)\b.+"
        @"dal㰞赉all'䬷⹺쇉;"

[<Fact>]
let ``rex sample 10.2``() =
    assertNoMatch
        @"\b(from).+(to)\b.+"
        @"from⍪堑to䶶孒"


[<Fact>]
let ``rex sample 11.1``() =
    assertIsMatch
        """(\\*)("|$)"""
        @"嶾⣒"

[<Fact>]
let ``rex sample 12.1``() =
    assertIsMatch
        """\s*content=["'][^;]+;\s*charset\s*=\s*([^'"]+)"""
        """Ѱ鉨켡暙턺 　content="囒;
 charset=�ᆲᓖ"""





#endif













