#I "../src/Sbre/bin/Release/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
#r "nuget: FSharp.Data"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Text.RuntimeRegexCopy
open System.Globalization
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ 

let longSample = __SOURCE_DIRECTORY__ + "/input-text.txt" |> System.IO.File.ReadAllText
let shortSample = longSample[..20000]


// using a lazy loop
let paragraphs1 =
    let pat = @"(?:[\s\S])*?\n\n+"
    let reg = System.Text.RegularExpressions.Regex(pat)
    let matches = reg.Matches(longSample)
    matches
    
paragraphs1.Count // 47164


let countMatches (pattern:string) =
    let reg = System.Text.RegularExpressions.Regex(pattern)
    let matches = reg.Matches(longSample)
    matches.Count, matches |> Seq.last
    // |> Seq.last



let paragraphRegexes = 
    [ 
        // line-loop (excludes last paragraph)
        @"(?:.+\n)+\n"
        // lazy loop with trailing whitespace (excludes last paragraph)
        @"(?:[\s\S])+?\n\n+"
 
        // line loop proper 
        @"(?:.+(?:\n|\z))+(?:\n|\z)"
        // lazy loop with trailing whitespace
        @"(?:[\s\S])+?(?:\n\n+|\z)"
        // neg-lookahead with leading whitespace
        @"(?:[\s\S](?!\n\n))+."
        // neg-lookahead with trailing whitespace
        @"(?:[\s\S](?!\n\n))*[\s\S]{2}\n*"
    ]

// let m = countMatches paragraphRegexes[0] // 47165
// let m = countMatches paragraphRegexes[1]
// let m = countMatches paragraphRegexes[2]
// let m = countMatches paragraphRegexes[3]
// let m = countMatches paragraphRegexes[4]
// let m = countMatches paragraphRegexes[5]

let sample = "\naaa\n\nbbb\n\nccc\n\n"

let sh = 
    // 15 matches
    Regex(paragraphRegexes[0]).Matches(sample).Count
    // 49 matches
// let mat = Matcher(@"~(⊤*\n\n⊤*)")
// let mat = Matcher(paragraphRegexes[0]).MatchPositions(sample) |> Seq.toArray
// let mat = Matcher(@"~(⊤*\n\n+[^\n]⊤*)")

// let r1 = mat.MatchPositions(shortSample) |> Seq.toArray

// let r2 = 
//     Matcher(@"~(⊤*\n\n⊤*)").MatchPositions(shortSample) |> Seq.where (fun v -> v.startIndex <> v.endIndex) |> Seq.length

// // neg-lookahead
// // leading ws
// // (?:[\s\S](?!\n\n))+.
// // trailing ws
// // (?:[\s\S](?!\n\n))*[\s\S]{2}\n*
// let paragraphs2 =
//     let pat = @"(?:[\s\S](?!\n\n))*[\s\S]{2}\n*"
//     let reg = System.Text.RegularExpressions.Regex(@"(?:[\s\S])*?\n\n+")
//     let matches = reg.Matches(longSample)
//     matches
    

// // ([\s\S](?!\n\n))*\n\n
// let paragraphs3 =
//     let pat = @"([\s\S])*?(?!\n\n)"
//     let reg = System.Text.RegularExpressions.Regex(@"(?:[\s\S])*?\n\n+")
//     let matches = reg.Matches(shortSample)
//     matches

// paragraphs1.Count // 14


let linesplit1 = 
    longSample.Split("\n\n") 
    |> Seq.where (fun v -> v <> "")
    |> Seq.length





// (?:[\s\S])*?\n\n+

// let a1 =
//     // System.Text.RegularExpressions.Regex(@"\n\n([\s\S](?!\n\n))*")
//     System.Text.RegularExpressions.Regex(@"\n\n(.+\n)+?*")
//     // System.Text.RegularExpressions.Regex(@"\n\n[^\n]*\n")
//         .Match(input)
    
// let enumerator =
//     let regex = System.Text.RegularExpressions.Regex(@"\n\n([\s\S])*?\n\n")
//     let mutable e = regex.EnumerateMatches(input)
//     while e.MoveNext() do
        
//         ()
    

