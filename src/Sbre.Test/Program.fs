// module Program = let [<EntryPoint>] main _ = 0

module Program

open Sbre
open Sbre.Test

let regex = Regex(@"abcd")
let matcher = regex.TSetMatcher
let formalLlmatch =
    matcher.llmatch("__abcd___")

let result = matcher.MatchText("Aa11aBaAA")
