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

let pattern = @"(?i)^(application/json|[^;/ 	]+/[^;/ 	]+[+]json)[ 	]*(;.*)?$"
let input = @"APPLİcaTIOn/JsOn"


let ismatch1 = System.Text.RegularExpressions.Regex.IsMatch(input, pattern)

let ismatch2 = Sbre.Regex(pattern).IsMatch(input)



    

    