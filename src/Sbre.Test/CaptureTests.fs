[<Xunit.Collection("Sequential")>]
module Sbre.Test.CaptureTests

open Sbre
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
            let result =
                Common.getFirstLLmatch pattern (isMatch) |> (fun (s,e) -> isMatch[s..s+e] )
            let result2 = runtime.Match(isMatch).Value
            Assert.True((result = result2), $"should be the same: {entry.Title}\n{pattern}\n{isMatch}\nmyregex:\n{result}\nruntime:\n{result2}")


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



#endif



