#I "../src/Sbre.Test/bin/Release/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types

// let mkder (symbol: char) node =
//     let unused = Pat.Location.getNonInitial ()
//     let tset = m.Cache.CharToMinterm(symbol)
//     m.CreateDerivative(&unused, tset, node)

fsi.AddPrinter<RegexNode<uint64>>(fun v -> v.ToString())

let os = Sbre.SbreOptions(CanonicalizeStates = true, CompressPattern = true)

let compress(pattern: string) =
    let r =
        Sbre.Regex(pattern, Sbre.SbreOptions(CanonicalizeStates = true, CompressPattern = true))

    let m = r.TSetMatcher
    m.PrettyPrintNodeLong(m.RawPattern)

let c1 = compress @"[\dA-Z].+&.{5,65}&.+[\d]&\w.+&.+\w"
let c2 = compress """(?<=[\d"]>.*).*&.+&.*(?=.*<\/h[\d])"""
let c3 = compress """(?<=or=\{|nd ).*&(?<=or=\{.*).*&~(.* a.*)&.+ .+&.*(?=[ \}])"""
let c4 = compress """(?<=\. ).*&~(.*[,:] [ \w]{2}.*)&.+[A-Z].+&.+\.&.*(?=[,:])"""
