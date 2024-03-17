// #I "../src/Sbre.Test/bin/Debug/net8.0"
#I "../src/Sbre.Test/bin/Release/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
open System
open System.Threading
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types
let longSample = __SOURCE_DIRECTORY__ + "/input-text.txt" |> System.IO.File.ReadAllText
fsi.PrintWidth <- 500    
let measureMillis fn = 
    let watch = System.Diagnostics.Stopwatch.StartNew()
    let result = fn()
    watch.Stop()
    watch.Elapsed,result


let comparePerf pattern = 
    let sbre = Sbre.Regex pattern
    let none = System.Text.RegularExpressions.Regex pattern
    let compiled = System.Text.RegularExpressions.Regex(pattern, System.Text.RegularExpressions.RegexOptions.Compiled)
    let r1 = measureMillis (fun v -> sbre.Count(longSample) )
    let r2 = measureMillis (fun v -> none.Count(longSample) )
    let r3 = measureMillis (fun v -> compiled.Count(longSample) )
    join "\n" [
        ""
        $"{nameof sbre,8}: {fst r1,-20} - {snd r1}"
        $"{nameof none, 8}: {fst r2, -20} - {snd r2}"
        $"{nameof compiled, 8}: {fst r3, -20} - {snd r3}"
    ]

let a = comparePerf "Twain"

let allMatches pattern = 
    Sbre.Regex(pattern).Matches(longSample)
    |> Seq.toArray
    

let t1 = allMatches @"\b\w+\b" // 2877671
let t2 = allMatches @"\w+" // 2877671
let t3 = allMatches @"~(\T*\W\T*)" // 6555774 (empty matches)
let t4 = allMatches @"~(\T*\W\T*)&\T+" // 2877671
let t5 = allMatches @"~(|\T*\W\T*)" // 2877671

t5.Length

