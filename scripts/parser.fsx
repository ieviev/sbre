#I "../src/Sbre/bin/Debug/net7.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
#r "nuget: FSharp.Data"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Text.RuntimeRegexCopy
open System.Globalization


let input = 
    "/home/ian/f/ieviev/sbre/scripts/input-text.txt"
    |> File.readAllText
    |> (fun v -> v[..15000])

let optimizations (pattern:string) =
    let regexTree =
        ExtendedRegexParser.Parse(
            pattern,
            RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking,
            CultureInfo.InvariantCulture
        )
    RegexFindOptimizations(regexTree.Root, RegexOptions.NonBacktracking)

// let opt = optimizations "OTSING" // indexof
// let opt = optimizations "OTSING|new eBooks" // indexof

let a1 =
    // System.Text.RegularExpressions.Regex(@"\n\n([\s\S])*?\n\n")
    // System.Text.RegularExpressions.Regex(@"\n\n([\s\S](?!\n\n))*")
    System.Text.RegularExpressions.Regex(@"\n\n(.+\n)+?*")
    // System.Text.RegularExpressions.Regex(@"\n\n[^\n]*\n")
        .Match(input)
    
let enumerator =
    let regex = System.Text.RegularExpressions.Regex(@"\n\n([\s\S])*?\n\n")
    let mutable e = regex.EnumerateMatches(input)
    while e.MoveNext() do
        
        ()
    


