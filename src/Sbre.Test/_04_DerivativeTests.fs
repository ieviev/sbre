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

let getDerivative(matcher: Regex, input: string) =
    let cache = matcher.TSetMatcher.Cache
    let node = matcher.TSetMatcher.RawPattern
    let location = (Location.create input 0)
    let state = RegexState(cache.NumOfMinterms())
    // let matchCache = RegexMatchCache(cache,node)
    createDerivative (cache, state, &location, cache.MintermForLocation(location), node)

let getDerivativeT<'t when 't : struct and 't :> IEquatable< 't >
        and 't: equality>(matcher: Regex, input: string) =
    let matcher = matcher.Matcher :?> RegexMatcher<TSet>
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let state = RegexState(cache.NumOfMinterms())
    let location = (Location.create input 0)
    createDerivative (cache, state, &location, cache.MintermForLocation(location), node)





let testFullDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern).TSetMatcher
    let cache = matcher.Cache
    let node = matcher.TrueStarredPattern
    let state = RegexState(cache.NumOfMinterms())
    let location = (Location.create input 0)

    let result =
        createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
        |> (fun v -> cache.PrettyPrintNode v)

    Assert.Equal(expectedDerivative, result)


let testFullDerivativeMultiple(pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let result = Common.der1 matcher input false
    Assert.Contains(result, expectedDerivatives)


let test2ndDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern).TSetMatcher
    let cache = matcher.Cache
    let node = matcher.TrueStarredPattern
    let state = RegexState(cache.NumOfMinterms())
    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)

    let der2 = createDerivative (cache, state, &location1, cache.MintermForLocation(location1), der1)

    let result = der2 |> (fun v -> cache.PrettyPrintNode v)


    Assert.Equal(expectedDerivative, result)


let test2ndDerivatives(pattern: string, input: string, expectedDerivatives: string list) =

    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let result =
        let matcher = Regex(pattern).TSetMatcher
        let cache = matcher.Cache
        let node = matcher.TrueStarredPattern
        let state = RegexState(cache.NumOfMinterms())

        let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
        let der2 = createDerivative (cache, state, &location1, cache.MintermForLocation(location1), der1)
        cache.PrettyPrintNode der2


    Assert.Contains(result, expectedDerivatives)






let testRawDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let result = der1 matcher input true
    Assert.Equal(expectedDerivative, result)




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
    testFullDerivativeMultiple ("ab", "ab", [ "(b|⊤*ab)"; @"(⊤*ab|b)" ])


[<Fact>]
let ``derivative of true``() = testPartDerivative ("⊤", "324", "ε")


[<Fact>]
let ``derivative of true ismatch``() = testPartDerivative ("⊤", "324", "ε")


[<Fact>]
let ``derivative of lookback 1``() =
    testPartDerivativeFromLocationMultiple (@"(?<=-.*).*", "-aaaa-", 5, [ ".*" ])

[<Fact>]
let ``derivative of lookback 2``() =
    testPartDerivativeFromLocationMultiple (@"(?<=-.*).*", "-aaaa-", 4, [ ".*" ])



[<Fact>]
let ``2 derivative of Twain``() =
    test2ndDerivatives ("Twain", "Twain", [ "(ain|⊤*Twain)"; @"(⊤*Twain|ain)" ])
// test2ndDerivative ("Twain", "Twain", "(⊤*Twain|ain)")

[<Fact>]
let ``derivative lookaround 1``() =
    testPartDerivatives (@"^\d$", "1", [ @"((?=\n)|(?!⊤))"; @"((?!⊤)|(?=\n))" ])

[<Fact>]
let ``derivative lookaround 1.2``() = testPartDerivative (@"(?<!\w)11", "11", "1")

[<Fact>]
let ``derivative lookaround 1.3``() = testPartDerivative (@"(?=1)11", "11", "1")



[<Fact>]
let ``derivative lookaround 2``() = testPartDerivative (@"\b11", "11", "1")

//([]1|1)

[<Fact>]
let ``derivative boundary 1``() =
    testPartDerivativeFromLocation (@"(?<=\s)22", "1 2", 2, "2")


[<Fact>]
let ``derivative boundary 2``() =
    testPartDerivativeFromLocation (@"\b22", "1 2", 2, "2")


// [<Fact>]
// let ``derivative boundary 3`` () = testPartDerivativeFromLocation (@"\b1\b", "1 ", 0, "((?<!φ)(?=φ)|(?<=φ)(?!φ))")


[<Fact>]
let ``derivative boundary 4``() =
    testPartDerivativeFromLocation (@"(?<=\d)a", "1a", 1, "ε")


[<Fact>]
let ``derivative boundary 5``() =
    testPartDerivativeFromLocation (@"(?=\w)a", "1a", 1, "ε")



// [<Fact>]
// let ``derivative or tail``() =
//     testPartDerivative (@"(310|0[1-9]2|452)", "002", "[1-9]2")


[<Fact>]
let ``derivative of plus``() =
    testPartDerivatives (@"^\d+$", "123", [ @"\d*((?=\n)|(?!⊤))"; @"\d*((?!⊤)|(?=\n))" ])


[<Fact>]
let ``derivative concat lookaround``() =
    testPartDerivatives (@"^\d+$", "123", [ @"\d*((?=\n)|(?!⊤))"; @"\d*((?!⊤)|(?=\n))" ])




[<Fact>]
let ``derivative lookback 1``() =
    let loc = Location.create "-aaaa-" 0
    testPartDerivativesLoc (@"(.*(?=.*-)&\S.*\S)", loc,  [@"(.*φ&.*(?=.*-))";"(.*(?=.*-)&.*φ)"])

[<Fact>]
let ``subsumption or loop ``() =
    testPartDerivative (@"(a*|.*)", "aaa", @".*")


[<Fact>]
let ``subsumption and loop ``() =
    testPartDerivative (@"(.*&.*s)", "aaa", @".*s")


[<Fact>]
let ``subsumption and larger ``() =
    testPartDerivatives (@"(.* and .*|and .*)&.*", "aaa", [@"(.* and .*|nd .*)";"(nd .*|.* and .*)"])

[<Fact>]
let ``deriv negation end ``() =
    testPartDerivatives (@"(.*&~((n|.*Finn)))", "nn", [
        "(~((ε|.*Finn))&.*)";"(~((.*Finn|ε))&.*)"; "(.*&~((ε|.*Finn)))"; "(.*&~((.*Finn|ε)))"
    ])


[<Fact>]
let ``subsumption or concat ``() =
    testPartDerivative (@".*t.*hat.*", "ttt", @".*hat.*")


[<Fact>]
let ``deriv negation 1 ``() =
    testPartDerivatives (@"~(.*11.*)", "1", [@"~((1.*|.*11.*))"; @"~((.*11.*|1.*))"])

[<Fact>]
let ``deriv negation 2 ``() =
    test2ndDerivatives (@"~(.*11.*)", "11", [
        @"(⊤*~(.*11.*)|~(.*)|~((.*11.*|1.*)))"
        @"(~(.*)|⊤*~(.*11.*)|~((1.*|.*11.*)))"
        @"(~(.*)|⊤*~(.*11.*)|~((.*11.*|1.*)))"
        @"(~(.*)|~((.*11.*|1.*))|⊤*~(.*11.*))"
        @"(~(.*)|~((1.*|.*11.*))|⊤*~(.*11.*))"
        @"(⊤*~(.*11.*)|~(.*)|~((1.*|.*11.*)))"
        @"(~((1.*|.*11.*))|~(.*)|⊤*~(.*11.*))"
    ])






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
        [ @"a((?=\n)|(?!⊤))"; @"a((?!⊤)|(?=\n))" ]
    )







// [<Fact>]
// let ``matchend test 3``() =
//     let matcher = Matcher(@"b.*|a")
//     let ism = matcher.FindMatchEnd("baaa ")
//     Assert.Equal(ValueSome 4, ism)



// [<Fact>]
// let ``subsumption true star epsilon`` () = testPartDerivative (@"(aa&⊤*)", "aa", @"a")



#endif





// [<Fact>]
// let ``counter debug``() =
//     let regex = Regex("a{1,3}b{1,3}a")
//     let result = assertCounterStates regex "aaa" [
//         CounterState.CanIncr
//         CounterState.CanIncr
//         CounterState.CanIncr
//     ]
//     assertAlternation [ "ε" ] result.Node
//


//
// [<Fact>]
// let ``counter 01``() =
//     let regex = Regex(".{2}c")
//     let result = assertCounterStates regex "__c" [
//         // --
//         [ CounterState.CanIncr, 0 ]
//         [ CounterState.CanExit, 1 ]
//         [  ]
//     ]
//     assertAlternation [ "ε" ] result.Node
//
//
//
// [<Fact>]
// let ``counter 02``() =
//     let regex = Regex(".{2}c$")
//     let result = assertCounterStates regex "__c" [
//         [ CounterState.CanIncr, 0 ]
//         [ CounterState.CanExit, 1 ]
//         [ CounterState.CanExit, 1 ]
//         [  ]
//     ]
//     assertAlternation [ "(?!⊤)"; @"(?=\n)" ] result.Node
// //
// [<Fact>]
// let ``counter 03``() =
//     let regex = Regex(".{2}(?=c)")
//     let result = assertCounterStates regex "__c" [
//         [CounterState.CanIncr, 0]
//         [CounterState.CanExit, 1]
//     ]
//     assertEqual result.IsNullable true
//
// [<Fact>]
// let ``counter 04``() =
//     let regex = Regex(".{2}(?<=c.*)")
//     let result = assertCounterStates regex "c_c" [
//         [CounterState.CanIncr, 0]
//         [CounterState.CanExit, 1]
//     ]
//     assertEqual result.IsNullable true
//
// //
// [<Fact>]
// let ``counter 05``() =
//     let regex = Regex("⊤*\d{2}⊤*")
//     let result = assertCounterStates regex "a11a" [
//         [CounterState.CanIncr, 0]
//         [CounterState.CanIncr, 0]
//         [CounterState.CanExit, 1]
//     ]
//     assertEqual result.IsNullable true

//
// [<Fact>]
// let ``counter 06``() =
//     let regex = Regex("~(⊤*\d{2}⊤*)")
//     let result = assertCounterStates regex "a11b" [
//         CounterState.CanIncr
//         CounterState.CanIncr
//         CounterState.CanIncr
//         CounterState.CanIncrExit
//     ]
//     assertEqual result.IsNullable false
//
// [<Fact>]
// let ``counter 08``() =
//     let regex = Regex("~(.*[a-z]{3}1)")
//     let result = assertCounterStates regex "__aaaaa1__" [
//         CounterState.CanIncr // 0
//         CounterState.CanIncr // 0
//         CounterState.CanIncr // 1
//         CounterState.CanIncr // 2
//         CounterState.CanIncr // 3
//         CounterState.CanIncrExit // 4 - 1
//         CounterState.CanIncrExit // 5 - 2
//     ]
//     assertEqual result.IsNullable true
//
//
// [<Fact>]
// let ``counter 09``() =
//     let regex = Regex(".*\d{2}c")
//     let result = assertCounterStates regex "__111c__" [
//         CounterState.CanIncr // 0
//         CounterState.CanIncr // 0
//         CounterState.CanIncr // 1
//         CounterState.CanIncr // 2
//         CounterState.CanIncrExit // 3
//         CounterState.CanIncrExit
//     ]
//     assertEqual result.IsNullable true

//
// [<Fact>]
// let ``counter 10``() =
//     let regex = Regex("\d\d")
//     let result = assertCounterStates regex "11" [
//         CounterState.CanIncr
//         CounterState.CanIncr
//     ]
//     assertEqual result.IsNullable true
//
//
//
//
// [<Fact>]
// let ``counter 11``() =
//     let regex = Regex("~(⊤*\d\d⊤*)")
//     let result = assertCounterStates regex "_11_" [
//         CounterState.CanIncr
//         CounterState.CanIncr
//     ]
//     assertEqual result.IsNullable true
//
// [<Fact>]
// let ``counter 12``() =
//     let regex = Regex("~(⊤*\d\d⊤*)")
//     let result = assertCounterStates regex "_11_" [
//         CounterState.CanIncr
//         CounterState.CanIncr
//         CounterState.CanIncr
//     ]
//     assertEqual result.IsNullable false
//
//
// [<Fact>]
// let ``counter 13``() =
//     let regex = Regex("~(⊤*\d\d⊤*)")
//     let result = assertCounterStates regex "1__11__" [
//         CounterState.CanIncr
//         CounterState.CanIncr
//     ]
//     assertEqual result.IsNullable true
//



