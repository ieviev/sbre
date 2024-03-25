#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types

let r = Sbre.Regex("Twain")
let m = r.TSetMatcher

let mkder (symbol: char) node =
    let tset = m.Cache.CharToMinterm(symbol)
    m.CreateNonInitialDerivative(tset,node)

fsi.AddPrinter<RegexNode<uint64>>(fun v -> v.ToString())

let initial = m.RawPattern // "⊤*(bb|abba)"
string initial // "(bb|abba)"
let _der1 = mkder 'T' initial // "bba" (remember |abba)
let _der2 = mkder 'w' _der1 //  "ba"  (remember |abba)
let _der3 = mkder 'a' _der2 // "a" // "a|bba"
let _der4 = mkder 'i' _der3 // "ε" // "|a|bba"



let wasds =
    use pos = Sbre
    ()

// |aabba -> ⊥
// a|abba -> abba

// "abb|a| abb|a| abb|a| abb|a|"

// bb bb bb bb

// |Twain


// Huckleberry|Finn
// H -> 11
// F -> 4

//


// nonbacktracking engine
// O(n)- IsMatch
// O(n^2) - All Matches
// - nonbacktracking algorithm - 1 pass over the entire string returns all match starts
// - earliest match semantics - (abba|bb) returns not a|bb|b but |abba|,

// backtracking engine
// O(n^2) - IsMatch
// O(2^n) - All Matches
