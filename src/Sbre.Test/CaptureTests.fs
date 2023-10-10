[<Xunit.Collection("Sequential")>]
module Sbre.Test.CaptureTests

open System
open System.Collections
open System.Collections.Generic
open System.Globalization
open System.Text.RegularExpressions
open System.Text.RuntimeRegexCopy
open Sbre
open Sbre.Test
open FSharp.Data
open Xunit

[<Literal>]
let RegexLibSamplesPath = __SOURCE_DIRECTORY__ + "/data/regexlibpatterns.json"

type RegexLibProvider = FSharp.Data.JsonProvider<RegexLibSamplesPath,InferTypesFromValues=false>

let regexlibSamples = RegexLibProvider.GetSamples()



let escapeNegConj (str:string) = str.Replace("&",@"\&").Replace("~",@"\~")
let testCapture0InRange fromRange toRange =
    let mutable counter = fromRange
    for entry in regexlibSamples[fromRange..toRange] do
        counter <- counter + 1
        let pattern = entry.Pattern
        // escape ~ and & in pattern
        let pattern = escapeNegConj pattern

        let matcher = Matcher(pattern)
        let runtime = System.Text.RegularExpressions.Regex(pattern)

        // testing only matches
        for isMatch in entry.Matches do
            try

                let result =
                    matcher.MatchText(isMatch) |> Option.defaultValue ""
                let result2 = runtime.Match(isMatch).Value
                Assert.True((result = result2), $"should be the same: {entry.Title}\n{pattern}\n{isMatch}\nmyregex:\n{result}\nruntime:\n{result2}")
            with e ->
                Assert.True(false, $"exception in {counter}:{entry.Title}\n{pattern}\n{isMatch}\n{e.Message}")


[<Fact>]
let ``captures 001-010`` () = testCapture0InRange 0 10

[<Fact>]
let ``captures 011-020`` () = testCapture0InRange 11 20

// [<Fact>] // test later
// let ``captures 021-030`` () = testCapture0InRange 21 30

[<Fact>]
let ``captures 031-040`` () = testCapture0InRange 31 40

[<Fact>]
let ``captures 041-050`` () = testCapture0InRange 41 50

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







