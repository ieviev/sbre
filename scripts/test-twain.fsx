#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
open System
open System.Threading
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types

let longSample = __SOURCE_DIRECTORY__ + "/input-text.txt" |> System.IO.File.ReadAllText
let shortSample = longSample[..50000]

// let nodeInfo = RegexNodeInfo<TSet>()

let regex = Sbre.Regex """["'][^"']{0,30}[?!\.]["']"""
let matcher = regex.TSetMatcher

let match1 = 
    regex.Matches(longSample)
    |> Seq.toArray
    

let mult = 
    match1
    |> Seq.where (fun v -> v.Value.Contains("'"))
    |> Seq.toArray
