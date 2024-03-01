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

let r = Regex ("a(|b)|[abc][abc]?")

r.TSetMatcher.Cache.Minterms()

// [abc]{0,1}

let regex1 = Sbre.Regex(".*have.*&.*there.*")
let cache1 = regex1.TSetMatcher.Cache
let prefix1 = regex1.InitialReversePrefix

let prefixSets1 =
    match prefix1 with
    | InitialOptimizations.PotentialStartPrefix(prefix) -> 
        Array.toList (prefix.ToArray())
    | _ -> failwith "debug"

let prefixPretty1 = Optimizations.printPrefixSets cache1 (prefixSets1)

// ".*have.*&.*there.*")
// "e;[rv];[ae];h"

let regex2 = Sbre.Regex("Huck[a-zA-Z]+|Saw[a-zA-Z]+")
let cache2 = regex2.TSetMatcher.Cache
let prefix2 = regex2.InitialReversePrefix

let prefixSets2 =
    match prefix2 with
    | InitialOptimizations.PotentialStartPrefix(prefix) -> 
        Array.toList (prefix.ToArray())
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


let regex3 = Sbre.Regex("(?i)Twain")
let cache3 = regex3.TSetMatcher.Cache
let prefix3 = regex3.InitialReversePrefix

let prefixSets3 =
    match prefix3 with
    | InitialOptimizations.SetsPrefix(prefix, _) -> Array.toList (prefix.ToArray())
    | _ -> failwith "debug"

let prefixPretty3 = Optimizations.printPrefixSets cache3 (prefixSets3)
// "[Nn];[Ii];[Aa];[Ww];[Tt]"

// let p3Chars = cache3.MintermChars(prefixSets3[1]).ToArray() // just 2 chars in [kw]

let regex4 = Sbre.Regex("(abcd|bqwe|crty)")
let cache4 = regex4.TSetMatcher.Cache
let prefix4 = regex4.InitialReversePrefix

let prefixSets4 =
    match prefix4 with
    | InitialOptimizations.PotentialStartPrefix(prefix) -> Array.toList (prefix.ToArray())
    | _ -> failwith "debug"

let prefixPretty4 = Optimizations.printPrefixSets cache4 (prefixSets4)
// "[dey];[ctw];[bqr];[a-c]"

let testchars = cache4.MintermChars(prefixSets4[3]).ToArray()
// [|'a'; 'b'; 'c'|]
let testcharcodes = testchars |> Array.map int
// [|97; 98; 99|] - sequential char codes
// search for sequential char codes can be optimized with ranges
let test = "____b_".AsSpan().IndexOfAnyInRange('a', 'c')


// more difficult examples

let regex5 = Sbre.Regex("a{0,10}.")
let cache5 = regex5.TSetMatcher.Cache
let prefix5 = regex5.InitialReversePrefix

let prefixSets5 =
    match prefix5 with
    | InitialOptimizations.PotentialStartPrefix(prefix) -> Array.toList (prefix.ToArray())
    | _ -> failwith "debug"

let prefixPretty5 = Optimizations.printPrefixSets cache5 (prefixSets5)
// "[dey];[ctw];[bqr];[a-c]"

let chars = cache5.MintermChars(prefixSets5[0]).ToArray()
// [|'\010'|]


cache5.Minterms() |> Array.map cache5.PrettyPrintMinterm
// [|"[^\na]"; "\n"; "a"|]

// TSet negation condition
let charIsNegated =
    // (cache5.Minterms()[0]) is where all negated characters are
    Solver.elemOfSet (cache5.Minterms()[0]) prefixSets5[0]

let test5 = "\n\n\n____b_".AsSpan().IndexOfAnyExcept(chars)


