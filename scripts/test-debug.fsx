#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
open System
open System.Threading
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types

let r = Sbre.Regex("bb|abba")
let m = r.TSetMatcher

string r.TSetMatcher.ReverseTrueStarredPattern

let mkder (symbol:char) node =  
    let unused = Pat.Location.getNonInitial()
    let tset = m.Cache.CharToMinterm(symbol)
    m.CreateDerivative(&unused,tset,node)


// "⊤*(abba|bb)"

let initial = m.ReverseTrueStarredPattern // "⊤*(bb|abba)"
string initial
let der1 = mkder 'a' initial // "(bba|⊤*(bb|abba))"
let der2 = mkder 'b' der1 // "(b|ba|⊤*(bb|abba))"
let der3 = mkder 'b' der2 // ""([ab]|⊤*(bb|abba)|ε)"" // "a|bba"
let der4 = mkder 'a' der3 // """((bba|⊤*(bb|abba)))?""" // "|a|bba"

// (.*|b)
// (.*)

"abb|a| abb|a| abb|a| abb|a|"

// (b|ba*)
// ba*

// nonbacktracking engine
// O(n)- IsMatch
// O(n^2) - All Matches
// - nonbacktracking algorithm - 1 pass over the entire string returns all match starts
// - earliest match semantics - (abba|bb) returns not a|bb|b but |abba|, 

// backtracking engine
// O(n^2) - IsMatch
// O(2^n) - All Matches

let initial2 = m.RawPattern // "⊤*(bb|abba)"
string initial2  // "(bb|abba)"
let _der1 = mkder 'a' initial2 // "bba" (remember |abba)
let _der2 = mkder 'b' _der1 //  "ba"  (remember |abba)
let _der3 = mkder 'b' _der2 // "a" // "a|bba"
let _der4 = mkder 'a' _der3 // "ε" // "|a|bba"

string _der4

// |aabba -> ⊥
// a|abba -> abba

// "abb|a| abb|a| abb|a| abb|a|"

// bb bb bb bb

// |Twain


// Huckleberry|Finn
// H -> 11
// F -> 4

//  

// step 1: match start

// |g|f|s|d|g|d|f|g
// |Huck
// gdsfgdsfgdf

// step 2: match end
// gfsdgdfg
// Huck
// gdsfgdsfgdf|
