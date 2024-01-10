// module Program = let [<EntryPoint>] main _ = 0

module Program

open Sbre
open Sbre.Test

// let regex = Regex(@"(abc)+")
// let a = Regex(@"abc(abc)*")
// let ab = Regex(@"abcabc")
// let regex = Regex(@"(?<=_.*)(abc)+(?=.*_)")
// let pat = Regex(@"ab")
//
// let matcher = regex.TSetMatcher
// // let formalLlmatch = matcher.llmatch("__abc_abc_")
// let formalLlmatch = matcher.llmatch("__abcabc_abc_")
//


let domatch =
    let matcher = Regex(@"..(?<=A.*)")
    let ism = matcher.MatchText("Aa")
    assert(Some "Aa" = ism)


// let domatch2 =
//     let matcher = Regex("""1300\d{6}$""").TSetMatcher
//     let cache = matcher.Cache
//     let mutable _toplevelOr = matcher.InitialPattern
//     let mutable loc = Pat.Location.create "1300333444" 0
//     let result = matcher.DfaEndPosition(cache, &loc, &_toplevelOr)
//     result

let w = 1
