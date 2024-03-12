[<Xunit.Collection("Sequential")>]
module Sbre.Test._10_RexTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Types
open Xunit

#if DEBUG
let [<Literal>] SampleFile = __SOURCE_DIRECTORY__ + "/data/rex-realworld-1.json"
type Provider = FSharp.Data.JsonProvider<SampleFile,SampleIsList=true, InferenceMode=FSharp.Data.InferenceMode.NoInference>
let ctx = Provider.GetSamples()

let regexlibSamples = Provider.GetSamples()

let escapeNegConj (str:string) = str.Replace("&",@"\&").Replace("~",@"\~")
let testSamplesRange (samples:Provider.Root seq) =
    for entry in samples do
        let pattern = entry.Pattern
        // escape ~ and & in pattern
        let pattern = escapeNegConj pattern
        let matcher =
            try Some (Regex(pattern))
            with e -> None
        let runtime = System.Text.RegularExpressions.Regex(pattern)
        match matcher with
        | None ->
            () // unsupported
        | Some matcher ->
            for isMatch in entry.Samples do
                try
                    let result = matcher.IsMatch(isMatch)
                    let result2 = runtime.IsMatch(isMatch)
                    Assert.True((result = result2), $"should be the same: {pattern}\n{isMatch}\nmyregex:{result} = runtime:{result2}")
                with e ->
                    Assert.True(false, $"exception in \n{pattern}\n{isMatch}\n{e.Message}")

// [<Fact>]
// let ``rex 01`` () =
//     __SOURCE_DIRECTORY__ + "/data/rex-realworld-1.json"
//     |> System.IO.File.ReadAllText
//     |> Provider.ParseList
//     |> testSamplesRange



#endif