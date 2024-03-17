[<Xunit.Collection("Sequential")>]
module Sbre.Test._10_RexTests

open System.Globalization
open System.Text.RuntimeRegexCopy
open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Types
open Xunit

#if DEBUG
let [<Literal>] SampleFile = __SOURCE_DIRECTORY__ + "/data/rex-realworld-1.json"
type Provider = FSharp.Data.JsonProvider<SampleFile,SampleIsList=true, InferenceMode=FSharp.Data.InferenceMode.NoInference>
let ctx = Provider.GetSamples()

let regexlibSamples = Provider.GetSamples()

let escapeNegConj (str:string) =
    System.Text.RegularExpressions.Regex.Replace(str,@"(?<!\\)[&~]|(?<=\\\\)[&~]", "\$0")
    // str.Replace("&",@"\&").Replace("~",@"\~")
let testSamplesRange (samples:Provider.Root seq) =
    System.Globalization.CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
    let failedSamples = ResizeArray()
    for entry in samples do
        let pattern = entry.Pattern
        // escape ~ and & in pattern
        let escapedPattern = escapeNegConj pattern
        let matcher =
            try Some (Regex(escapedPattern))
            with e -> None

        let runtime =
            try Some (System.Text.RegularExpressions.Regex(
                pattern, System.Text.RegularExpressions.RegexOptions.CultureInvariant))
            with e -> None


        match matcher, runtime with
        | None,_ | _, None ->
            () // unsupported
        | Some matcher, Some runtime ->
            for isMatch in entry.Samples do
                try
                    let result = matcher.IsMatch(isMatch)
                    let result2 = runtime.IsMatch(isMatch)
                    if result <> result2 then
                        failedSamples.Add($"pat:{pattern}; input:{isMatch}; sbre {result}, dotnet {result2}")
                    // Assert.True((result = result2), $"should be the same: {pattern}\n{isMatch}\nmyregex:{result} = runtime:{result2}")
                with e ->
                    Assert.True(false, $"exception in \n{pattern}\n{isMatch}\n{e.Message}")
        if failedSamples.Count > 0 then
            failwith
                (failedSamples |> String.concat "\n")

//
[<Fact>]
let ``rex 01`` () =
    __SOURCE_DIRECTORY__ + "/data/rex-realworld-1.json"
    |> System.IO.File.ReadAllText
    |> Provider.ParseList
    |> testSamplesRange

// //
// [<Fact>]
// let ``rex 02`` () =
//     __SOURCE_DIRECTORY__ + "/data/rex-realworld-2.json"
//     |> System.IO.File.ReadAllText
//     |> Provider.ParseList
//     |> testSamplesRange
//

#endif