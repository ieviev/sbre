// module Program = let [<EntryPoint>] main _ = 0

module Program

open Sbre
open Sbre.Test

let matcher = Regex(@"~(⊤*\d\d⊤*)")
let result = matcher.MatchText("Aa11aBaAA")
//
//
// let r = Sbre.Regex("(a|ab)+").Matcher :?> RegexMatcher<uint64>
// // let r = Sbre.Regex("the")
// let result =
//     // r.llmatch("fsdfgdg bababa sdfgdfg")
//     r.Match("fsdfgdg ababab sdfgdfg")
//     // r.Match("fs th df th gdg ab the abab sdfgdfg")
//
// let r1 = Sbre.Regex("abc").Matcher :?> RegexMatcher<uint64>
// let asdf =
//     // r1.Match("|abc abc abc") // <- going backwards
//     // r1.Match("abc abc abc|") // <- left to right (mirrored)
//     r1.Match("abc| abc abc") // <- exit condition (⊥|T*Huck)

// (a|ab) <- a is satisfied : exit
// ababab

// T*Huck  <- implicit pattern
// Huck
// uck|T*Huck
// ck|T*Huck
// k|T*Huck
// ε|T*Huck
// ⊥|T*Huck

let asd = 1
// let r = _09_DfaTests.``dfa end 01``()
// let r = _09_DfaTests.``dfa all ends equal 02``()
// let r = _09_DfaTests.``dfa all ends equal 02``()
