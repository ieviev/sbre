[<Xunit.Collection("Sequential")>]
module Sbre.Test.RegexLibTests

open Sbre
open Xunit

[<Literal>]
let RegexLibSamplesPath = __SOURCE_DIRECTORY__ + "/data/regexlibpatterns.json"

type RegexLibProvider = FSharp.Data.JsonProvider<RegexLibSamplesPath,InferTypesFromValues=false>

let regexlibSamples = RegexLibProvider.GetSamples()

let escapeNegConj (str:string) = str.Replace("&",@"\&").Replace("~",@"\~")
let testSamplesRange fromRange toRange =
    for entry in regexlibSamples[fromRange..toRange] do
        let pattern = entry.Pattern
        // escape ~ and & in pattern
        let pattern = escapeNegConj pattern

        let matcher = Regex(pattern)
        let runtime = System.Text.RegularExpressions.Regex(pattern)


        for isMatch in entry.Matches do
            try
                let result = matcher.IsMatch(isMatch)
                let result2 = runtime.IsMatch(isMatch)
                Assert.True((result = result2), $"should be the same: {entry.Title}\n{pattern}\n{isMatch}\nmyregex:{result} = runtime:{result2}")
            with e ->
                Assert.True(false, $"exception in {entry.Title}\n{pattern}\n{isMatch}\n{e.Message}")
        for nonMatch in entry.NonMatches do
            try
                let result = matcher.IsMatch(nonMatch)
                let result2 = runtime.IsMatch(nonMatch)
                Assert.True((result = result2), $"should be the same: {entry.Title}\n{pattern}\n{nonMatch}\nmyregex:{result} = runtime:{result2}")
            with e ->
                Assert.True(false, $"exception in {entry.Title}\n{pattern}\n{nonMatch}\n{e.Message}")



[<Fact>]
let ``regexlib 001-010`` () = testSamplesRange 0 10

[<Fact>]
let ``regexlib 011-020`` () = testSamplesRange 11 20

[<Fact>]
let ``regexlib 021-030`` () = testSamplesRange 21 30

// PositiveLookaround and NegativeLookaround
[<Fact>]
let ``regexlib 031-040`` () = testSamplesRange 31 40

// [<Fact>] // nested inner lookarounds
// let ``regexlib 041-050`` () = testSamplesRange 41 50

// undefined reference
// [<Fact>]
// let ``regexlib 051-060`` () =
//
//     for entry in regexlibSamples[51..60] do
//         let pattern = entry.Pattern
//         let matcher = Matcher(pattern)
//         let runtime = System.Text.RegularExpressions.Regex(pattern)
//
//         for isMatch in entry.Matches do
//             let result = matcher.IsMatch(isMatch)
//             let result2 = runtime.IsMatch(isMatch)
//             Assert.True((result = result2), $"should be the same: {entry.Title}\n{pattern}\n{isMatch}\nmyregex:{result} = runtime:{result2}")
//
//         for nonMatch in entry.NonMatches do
//             let result = matcher.IsMatch(nonMatch)
//             let result2 = runtime.IsMatch(nonMatch)
//             Assert.True((result = result2), $"should be the same: {entry.Title}\n{pattern}\n{nonMatch}\nmyregex:{result} = runtime:{result2}")



[<Fact>]
let ``regexlib 061-070`` () = testSamplesRange 61 70



// undefined reference
// [<Fact>]
// let ``regexlib 071-080`` () = testSamplesRange 71 80

// [<Fact>] // nested inner lookarounds
// let ``regexlib 081-090`` () = testSamplesRange 81 90

[<Fact>]
let ``regexlib 091-100`` () = testSamplesRange 91 100

// not impl node
// [<Fact>]
// let ``regexlib 101-110`` () = testSamplesRange 101 110

// not impl node
// [<Fact>]
// let ``regexlib 111-120`` () = testSamplesRange 111 120

// undefined reference
// [<Fact>]
// let ``regexlib 121-130`` () = testSamplesRange 121 130

// not impl node
// [<Fact>]
// let ``regexlib 131-140`` () = testSamplesRange 131 140

[<Fact>]
let ``regexlib 141-150`` () = testSamplesRange 141 150

[<Fact>]
let ``regexlib 151-160`` () = testSamplesRange 151 160

// [<Fact>] // nested inner lookarounds
// let ``regexlib 161-170`` () = testSamplesRange 161 170

[<Fact>]
let ``regexlib 171-180`` () = testSamplesRange 171 180

// not impl node
// [<Fact>]
// let ``regexlib 181-190`` () = testSamplesRange 181 190

[<Fact>]
let ``regexlib 191-200`` () = testSamplesRange 191 200

//
// [<Fact>]
// let ``regexlib 200`` () = testSamplesRange 201 999