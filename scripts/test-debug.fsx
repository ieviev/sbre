#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
open System
open System.Threading
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types

// let nodeInfo = RegexNodeInfo<TSet>()

let regex = Sbre.Regex "(ab)+"
let matcher = regex.TSetMatcher

let match1 = 
    regex.Matches("__abab__ab__")
    |> Seq.toArray
    

    