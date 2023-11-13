#I "../src/Sbre/bin/Release/net7.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
#r "nuget: FSharp.Data"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Text.RuntimeRegexCopy
open System.Globalization

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ 

let longSample = __SOURCE_DIRECTORY__ + "/input-text.txt" |> System.IO.File.ReadAllText


// let matchConjunction(conjs: string list) =
//     Matcher(String.Join('&', conjs)).Match(longSample)

// let m1 = matchConjunction [ "THE.*LIFE" ] // "THE TURNING-POINT OF MY LIFE"
// let m2 = matchConjunction [ "THE.*LIFE"; @".*FIVE.*" ] // "THE FIVE BOONS OF LIFE"
// let m3 = matchConjunction [ "THE.*"; @"~(⊤*\n)" ] // THE
// let m4 = matchConjunction [ "THE.*"; @"~(\n⊤*)" ] // "THE ENTIRE GUTENBERG TWAIN FILES"


let trimChars = "\r\n .,-\"!?():;'".ToCharArray()

let words = 
    longSample.Split([|'\r';'\n'|])
    |> Seq.collect (fun v -> v.Split(' '))
    |> Seq.collect (fun v -> v.Split("--"))
    // |> Seq.map (fun v -> v.Trim(trimChars).ToLowerInvariant())
    |> Seq.map (fun v -> v.Trim(trimChars))
    


let wordDict = 
    words
    |> Seq.groupBy id
    |> Seq.map (fun (a,b) -> a, Seq.toArray b)
    |> dict


let byFrequency = 
    wordDict
    |> Seq.sortByDescending (fun v -> v.Value.Length)
    |> Seq.toArray


let counts = 
    byFrequency 
    |> Seq.where (fun v -> v.Key <> "")
    // |> Seq.take 10 
    |> Seq.map (fun (entry) -> $"{entry.Key}\t{entry.Value.Length}" )
    |> Seq.append [ "word\tcount"]
    |> File.writeLinesTo "analysis-frequency.csv"


fsi.PrintWidth <- 5000
