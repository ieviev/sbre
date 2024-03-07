[<Xunit.Collection("Sequential")>]
module Sbre.Test._04_DerivativeTests

open Common

#if DEBUG

open System
open Sbre
open Sbre.Algorithm
open Sbre.CountingSet
open Sbre.Info
open Sbre.Pat
open Sbre.Types
open Xunit



let testFullDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern).TSetMatcher
    let cache = matcher.Cache
    let node = matcher.TrueStarredPattern
    let location = (Location.create input 0)

    let result =
        matcher.CreateDerivative (  &location, cache.MintermForLocation(location), node)
        |> matcher.PrettyPrintNode

    Assert.Equal(expectedDerivative, result)


let testFullDerivativeMultiple(pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let result = Common.der1 matcher input false
    Assert.Contains(result, expectedDerivatives)


let test2ndDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern).TSetMatcher
    let cache = matcher.Cache
    let node = matcher.TrueStarredPattern
    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let der1 = matcher.CreateDerivative  (  &location, cache.MintermForLocation(location), node)

    let der2 = matcher.CreateDerivative  (  &location1, cache.MintermForLocation(location1), der1)

    let result = der2 |> (fun v -> matcher.PrettyPrintNode v)


    Assert.Equal(expectedDerivative, result)


let test2ndDerivatives(pattern: string, input: string, expectedDerivatives: string list) =

    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let result =
        let matcher = Regex(pattern).TSetMatcher
        let cache = matcher.Cache
        let node = matcher.TrueStarredPattern
        let der1 = matcher.CreateDerivative  (  &location, cache.MintermForLocation(location), node)
        let der2 = matcher.CreateDerivative (  &location1, cache.MintermForLocation(location1), der1)
        matcher.PrettyPrintNode der2


    Assert.Contains(result, expectedDerivatives)






let testRawDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let result = der1 matcher input true
    Assert.Equal(expectedDerivative, result)

let testRevDerivative(pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let result = der1Rev matcher input
    Assert.Contains(matcher.TSetMatcher.PrettyPrintNode(result),expectedDerivatives)



let testPartDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let location = (Location.create input 0)
    let result = Common.der1rawlocs matcher location


    Assert.Equal(expectedDerivative, result)


let testPartDerivatives(pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let prettyResult = Common.der1 matcher input true
    Assert.Contains(prettyResult , expectedDerivatives)




let testPartDerivativeFromLocation
    (
        pattern: string,
        input: string,
        position: int,
        expectedDerivative: string
    )
    =
    let matcher = Regex(pattern)
    let location = (Location.create input position)
    let prettyResult = Common.der1rawlocs matcher location
    Assert.Equal(expectedDerivative, prettyResult)


let testPartDerivativeFromLocationMultiple
    (
        pattern: string,
        input: string,
        position: int,
        expectedDerivatives: string list
    )
    =
    let matcher = Regex(pattern)
    let location = (Location.create input position)
    let prettyResult = Common.der1rawlocs matcher location

    Assert.Contains(prettyResult, expectedDerivatives)

let testPartDerivativesLoc
    (
        pattern: string,
        loc: Location,
        expectedDerivatives: string list
    )
    =
    let matcher = Regex(pattern)
    let result = Common.der1rawlocs matcher loc

    Assert.Contains(result, expectedDerivatives)



[<Fact>]
let ``raw derivative of ab``() = testRawDerivative ("ab", "ab", "b")

[<Fact>]
let ``derivative of ab``() =
    testFullDerivativeMultiple ("ab", "ab", [
        "(b|⊤*ab)"; @"(⊤*ab|b)"; @"(ε|⊤*a)b"; @"(⊤*a|ε)b"; "(⊤*a)?b"
    ])


[<Fact>]
let ``derivative of true``() = testPartDerivative ("⊤", "324", "ε")


[<Fact>]
let ``derivative of true ismatch``() = testPartDerivative ("⊤", "324", "ε")


[<Fact>]
let ``derivative of lookback 1``() =
    // TODO: subsume this to .*
    testPartDerivativeFromLocationMultiple (@"(?<=-.*).*", "-aaaa-", 5, [
        @"(.*|(?<=.*).*)"; @"((?<=.*).*|.*)"
        @"(?<=.*).*"
        @".*"
    ])


[<Fact>]
let ``2 derivative of Twain``() =
    test2ndDerivatives ("Twain", "Twain", [

        @"(⊤*Tw)?ain"
        "(ain|⊤*Twain)"
        @"(⊤*Twain|ain)"
        @"(⊤*Tw|ε)ain"
        @"(ε|⊤*Tw)ain"
    ])

[<Fact>]
let ``derivative lookaround 1``() =
    testPartDerivatives (@"^\d$", "1", [
        @"$"
        // @"((?=\n)|(?!⊤))"; @"((?!⊤)|(?=\n))"
    ])



[<Fact>]
let ``derivative lookaround 2``() = testPartDerivative (@"\b11", "11", "1")

//([]1|1)

[<Fact>]
let ``derivative boundary 1``() =
    testPartDerivativeFromLocation (@"(?<=\s)22", "1 2", 1, "22")

[<Fact>]
let ``derivative boundary 4``() =
    testPartDerivativeFromLocation (@"(?<=\d)a", "1a", 0, "a")





// [<Fact>]
// let ``derivative or tail``() =
//     testPartDerivative (@"(310|0[1-9]2|452)", "002", "[1-9]2")


[<Fact>]
let ``derivative of plus``() =
    testPartDerivatives (@"^\d+$", "123", [
        @"φ*$"
        // @"φ*((?!⊤)|(?=\n))"; @"φ*((?=\n)|(?!⊤))"
    ])


[<Fact>]
let ``derivative concat lookaround``() =
    testPartDerivatives (@"^\d+$", "123", [
        @"φ*$"
        // @"φ*((?=\n)|(?!⊤))"; @"φ*((?!⊤)|(?=\n))"
    ])




// [<Fact>]
// let ``derivative lookback 1``() =
//     // TODO: subsumption
//     let loc = Location.create "-aaaa-" 0
//     testPartDerivativesLoc (@"(.*(?=.*-)&\S.*\S)", loc,  [
//         @"(.*φ&((?=(ε|.*-))|.*(?=.*-)))"
//         @"((.*(?=.*-)|(?=(ε|.*-)))&.*φ)"
//         // @"(.*φ&.*(?=.*-))";"(.*(?=.*-)&.*φ)"
//     ])






[<Fact>]
let ``deriv negation 1 ``() =
    testPartDerivatives (@"~(.*11.*)", "1", [
        @"~((1.*|.*11.*))"
        @"~((.*11.*|1.*))"
        @"~((.*1)?1.*)"

        @"~((.*1|ε)1.*)"
        @"~((ε|.*1)1.*)"
    ])

// [<Fact>]
// let ``deriv negation 2 ``() =
//     test2ndDerivatives (@"~(.*11.*)", "11", [
//         @"(⊤*~(.*11.*)|~(.*)|~((.*11.*|1.*)))"
//         @"(~(.*)|⊤*~(.*11.*)|~((1.*|.*11.*)))"
//         @"(~(.*)|⊤*~(.*11.*)|~((.*11.*|1.*)))"
//         @"(~(.*)|~((.*11.*|1.*))|⊤*~(.*11.*))"
//         @"(~(.*)|~((1.*|.*11.*))|⊤*~(.*11.*))"
//         @"(⊤*~(.*11.*)|~(.*)|~((1.*|.*11.*)))"
//         @"(~((1.*|.*11.*))|~(.*)|⊤*~(.*11.*))"
//         @"(⊤*~(.*11.*)|~((1.*|.*11.*))|~(.*))"
//         @"(~((.*11.*|1.*))|~(.*)|⊤*~(.*11.*))"
//         @"(~((.*11.*|1.*))|⊤*~(.*11.*)|~(.*))"
//         @"(⊤*~(.*11.*)|~((.*11.*|1.*))|~(.*))"
//         @"(~((1.*|.*11.*))|⊤*~(.*11.*)|~(.*))"
//         @"((⊤*~(.*11.*)|~((.*11.*|1.*)))|~(.*))"
//
//         @"((~((ε|.*1)1.*)|⊤*~(.*11.*))|~((.*|(ε|.*1)1.*)))"
//         @"(~(((.*1|ε)1.*|.*))|(⊤*~(.*11.*)|~((.*1|ε)1.*)))"
//         @"(~((.*|(.*1|ε)1.*))|(~((.*1|ε)1.*)|⊤*~(.*11.*)))"
//     ])






// [<Fact>]
// let ``subsumption or loop limited 1``() =
//     test2ndDerivatives (
//         "[a-z]{0,10}y",
//         "ccccc",
//         [ @"([a-z]{0,9}y|⊤*[a-z]{0,10}y)" ]
//         // [ "⊤*[a-z]{0,10}y" ]
//     )




[<Fact>] // implies set eats a node it shouldnt
let ``derivative eats node from set``() =
    testPartDerivativeFromLocationMultiple (
        @"^((0?[13578]a)|(0?[13456789]a))$",
        "4a",
        0,
        [
            @"a$"
            // @"a((?=\n)|(?!⊤))"; @"a((?!⊤)|(?=\n))"
        ]
    )


// [<Fact>]
// let ``subsumption true star epsilon`` () = testPartDerivative (@"(aa&⊤*)", "aa", @"a")


// [<Fact>]
// let ``neg lookaround 1``() = testPartDerivative (@"(?<!a)b", "ab", "⊥")
//
// [<Fact>]
// let ``neg lookaround 2``() = testPartDerivative (@"(?<!a)b", "bb", "ε")
//
// [<Fact>]
// let ``neg lookaround 3``() = testPartDerivative (@"(?!b)b", "bb", "⊥")
//
// [<Fact>]
// let ``neg lookaround 4``() = testPartDerivative (@"(?!a)b", "bb", "ε")
//
// [<Fact>]
// let ``neg lookaround 5``() = testPartDerivative (@"(?!b)", "b", "(?!ε)")
//
// [<Fact>] // means that pos 0 is nullable!!
// let ``neg lookaround 6``() = testPartDerivative (@"(?!a)", "b", "(?!⊥)")
//
// [<Fact>] // can only be sure of this in pos 0
// let ``neg lookaround 7``() = testPartDerivative (@"(?<!a)", "b", "(?<!⊥)")
//
// [<Fact>] // can not be sure of this
// let ``neg lookaround 8``() = testPartDerivative (@"(?<!a)b", "b", "ε")
//
// [<Fact>] // can not be sure of this
// let ``neg lookaround 9``() = test2ndDerivatives (@"(?<!a)bb", "abb", [
//     // @"(b|⊤*(?<!a)bb)" is not valid!
//     @"⊤*(?<!a)bb)"
// ])

// [<Fact>]
// let ``neg lookaround to pos 1``() = testPartDerivative (@"(?=~(|b))", "b", "(?=⊤+)")
//
// [<Fact>]
// let ``neg lookaround to pos 2``() = testPartDerivative (@"~(|b|bb)", "b", "~((b|ε))")

// let neg_neg_lookahead = @"~(\z|⊤\z|bb)"
//
// // L (?!bb)  = { ε\Z, ⊤\Z, ⊤[^b] }
//
// [<Fact>]
// let ``neg ismatch 1``() = assertMatchEnd neg_neg_lookahead "" 0 -2
// [<Fact>]
// let ``neg ismatch 2``() = assertMatchEnd neg_neg_lookahead "b" 0 -2
// [<Fact>]
// let ``neg ismatch 3``() = assertMatchEnd neg_neg_lookahead "bb" 0 -2
// [<Fact>]
// let ``neg ismatch 4``() = assertMatchEnd neg_neg_lookahead "ba" 0 2
//
// [<Fact>]
// let ``pos lookaround 1``() = testPartDerivative (@"(?<!a)b", "ab", "⊥")


[<Fact>]
let ``derivative neg lookaround 1``() =
    assertRawDerivative @"((?<=B.*).*&~(.*A.*))" "BA" [
        // "((?<=.*).*&~(.*A.*))"
        // @"(~(.*A.*)&(?<=.*).*)"
        // @"(~(.*A.*)&.*)"
        // @"(.*&~(.*A.*))"
        @"(?<=.*)(.*&~(.*A.*))"
        @"(?<=.*)(~(.*A.*)&.*)"
    ]





[<Fact>]
let ``simple 1``() =
    testRevDerivative ("..g","gggg",[
        @".{2,2}"
    ])



#endif



