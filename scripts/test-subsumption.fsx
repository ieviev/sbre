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

let pat = "a(|b)|[abc]b?"

let reg = Regex(pat)
let mat = reg.TSetMatcher
let c = mat.Cache

// 



let mkder : (TSet * RegexNode<TSet> -> RegexNode<TSet>) = 
    (fun (mt,node) -> 
        let loc = Location.getNonInitial()
        mat.CreateDerivative(&loc,mt,node)
    )
    

let R = mat.RawPattern    

let printDers (c:RegexCache<_>) (ders:(TSet * RegexNode<TSet>) array) = 
    ders 
    |> Seq.map (fun (mt,n) -> 
        let m = c.PrettyPrintMinterm(mt)
        let d = c.PrettyPrintNode(n)
        $"{m,-8} ==> {d}"
    )
    |> String.concat "\n" 
    |> (fun v -> "\n" + v)


let getDers n = getImmediateDerivatives mkder mat.Cache n |> Seq.toArray
let ders1 = getDers R
printDers c ders1



string R








