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

let sample1 = 
    "/home/ian/f/ieviev/sbre/src/Sbre.Test/data/sample-html-1.html"
    |> File.readAllText

let matchText(conjs: string list) =
    Matcher(String.concat "&" conjs).Match(sample1)

// this comment somehow breaks
let t1 = 
    matchText [ "class=\"⊤*" ] // Length = 1013086
let t2 = 
    matchText [ "class=\"~(⊤*\"⊤*)" ] // Length = 548

let t3 = 
    [ "<script>~(⊤*>⊤*)" ]
    |> matchText  // Length = 548



let h1 = Matcher(String.concat "&" ["class=\"⊤*"]).Match(sample1)

let hw = 
    Matcher("class=\"⊤*").Match(sample1)

// r.Value

