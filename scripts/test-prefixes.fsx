#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types
open Sbre.Pat
open Sbre.Optimizations

let regex1 = Sbre.Regex(".*have.*&.*there.*")
let cache1 = regex1.TSetMatcher.Cache
let prefix1 = regex1.InitialReversePrefix

let prefixSets1 = 
    match prefix1 with
    | InitialOptimizations.PotentialStartPrefix(prefix) -> Array.toList (prefix.ToArray())
    | _ -> failwith "debug"
    
let prefixPretty1 = Optimizations.printPrefixSets cache1 (prefixSets1)

// "e;[rv];[ae];h"

let regex2 = Sbre.Regex("Huck[a-zA-Z]+|Saw[a-zA-Z]+")
let cache2 = regex2.TSetMatcher.Cache
let prefix2 = regex2.InitialReversePrefix
let prefixSets2 = 
    match prefix2 with
    | InitialOptimizations.PotentialStartPrefix(prefix) -> Array.toList (prefix.ToArray())
    | _ -> failwith "debug"
let prefixPretty2 = Optimizations.printPrefixSets cache2 (prefixSets2)    
// "[A-Za-z];[kw];[ac];[Su]"

let p1Chars = cache2.MintermChars(prefixSets2[0]).ToArray() // 52 chars in [A-Za-z]
let p2Chars = cache2.MintermChars(prefixSets2[1]).ToArray() // just 2 chars in [kw]

// priority of searches
// 1. [Su] // uppercase characters are rare
// 2. [kw] // 
// 3. [ac] // vowels are usually more frequent
// 4. [A-Za-z] (compare either as TSet or System.Buffers.SearchValues)
