[<Xunit.Collection("Sequential")>]
module Sbre.Test._04_DerivativeTests

#if DEBUG

open System
open Sbre
open Sbre.Algorithm
open Sbre.Info
open Sbre.Pat
open Sbre.Types
open Xunit

let getDerivative(matcher: Regex, input: string) =
    let cache = matcher.TSetMatcher.Cache
    let node = matcher.TSetMatcher.RawPattern
    let location = (Location.create input 0)
    let state = RegexState()
    // let matchCache = RegexMatchCache(cache,node)
    createDerivative (cache, state, &location, cache.MintermForLocation(location), node)

let getDerivativeT<'t when 't : struct and 't :> IEquatable< 't >
        and 't: equality>(matcher: Regex, input: string) =
    let matcher = matcher.Matcher :?> RegexMatcher<TSet>
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let state = RegexState()
    let location = (Location.create input 0)
    createDerivative (cache, state, &location, cache.MintermForLocation(location), node)

let getNodeDerivative(matcher: Regex, state:RegexState, node: RegexNode<TSet>, input: string) =
    let matcher = matcher.Matcher :?> RegexMatcher<TSet>
    let cache = matcher.Cache
    // let node = matcher.RawPattern
    let location = (Location.create input 0)
    createDerivative (cache, state, &location, cache.MintermForLocation(location), node)



let testFullDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern).TSetMatcher
    let cache = matcher.Cache
    let node = matcher.InitialPattern
    let state = RegexState()
    let location = (Location.create input 0)

    let result =
        createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
        |> (fun v -> cache.PrettyPrintNode v)

    Assert.Equal(expectedDerivative, result)


let testFullDerivativeMultiple(pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let result = Common.getDerImpl matcher input

    Assert.Contains(result, expectedDerivatives)


let test2ndDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern).TSetMatcher
    let cache = matcher.Cache
    let node = matcher.InitialPattern
    let state = RegexState()
    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)

    let der2 = createDerivative (cache, state, &location1, cache.MintermForLocation(location1), der1)

    let result = der2 |> (fun v -> cache.PrettyPrintNode v)


    Assert.Equal(expectedDerivative, result)


let test2ndDerivatives(pattern: string, input: string, expectedDerivatives: string list) =

    let location = (Location.create input 0)
    let location1 = (Location.create input 1)
    let state = RegexState()
    let result =
        try
            let matcher = Regex(pattern).TSetMatcher
            let cache = matcher.Cache
            let node = matcher.InitialPattern

            let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
            let der2 = createDerivative (cache, state, &location1, cache.MintermForLocation(location1), der1)
            cache.PrettyPrintNode der2
        with
            e ->
                try
                    let matcher = Regex(pattern).ByteMatcher
                    let cache = matcher.Cache
                    let node = matcher.InitialPattern
                    let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
                    let der2 = createDerivative (cache, state, &location1, cache.MintermForLocation(location1), der1)
                    cache.PrettyPrintNode der2
                with e ->
                    let matcher = Regex(pattern).UInt16Matcher
                    let cache = matcher.Cache
                    let node = matcher.InitialPattern
                    let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
                    let der2 = createDerivative (cache, state, &location1, cache.MintermForLocation(location1), der1)
                    cache.PrettyPrintNode der2



    Assert.Contains(result, expectedDerivatives)






let testRawDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let result = Common.getDer1 matcher input
    Assert.Equal(expectedDerivative, result)




let testPartDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let location = (Location.create input 0)
    let result = Common.getDerLoc matcher location


    Assert.Equal(expectedDerivative, result)


let testPartDerivatives(pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let prettyResult = Common.getDer1 matcher input
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
    let prettyResult = Common.getDerLoc matcher location
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
    let prettyResult = Common.getDerLoc matcher location

    Assert.Contains(prettyResult, expectedDerivatives)

let testPartDerivativesLoc
    (
        pattern: string,
        loc: Location,
        expectedDerivatives: string list
    )
    =
    let matcher = Regex(pattern)
    let result = Common.getDerLoc matcher loc

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
let ``subsumption or loop limited 1``() =
    test2ndDerivatives (
        "[a-z]{0,10}y",
        "ccccc",
        // [ @"(⊤*[a-z]{0,10}y|[a-z]{0,9}y)"; @"([a-z]{0,9}y|⊤*[a-z]{0,10}y)" ]
        // CsA
        [ @"(⊤*[a-z]{0,10}y|[a-z]{0,10}y)";]
    )




[<Fact>] // implies set eats a node it shouldnt
let ``derivative eats node from set``() =
    testPartDerivativeFromLocationMultiple (
        @"^((0?[13578]a)|(0?[13456789]a))$",
        "4a",
        0,
        [ @"a((?=\n)|(?!⊤))"; @"a((?!⊤)|(?=\n))" ]
    )



[<Fact>]
let ``matchend test 1``() =
    // let matcher = Matcher(@".*(?=.*-)&\S.*\S")
    let matcher = Regex(@".*(?=-)")
    let result2 = matcher.FindMatchEnd(@"aa-")
    Assert.Equal(ValueSome 2, result2)

[<Fact>]
let ``matchend test 2``() =
    let matcher = Regex(@".*(?=.*-)&\S.*\S")
    let result2 = matcher.FindMatchEnd(@"-aaaa-")
    Assert.Equal(ValueSome 5, result2)


[<Fact>]
let ``matchend test 3.1``() =
    let matcher = Regex(@".*b")
    let ism = matcher.FindMatchEnd(" aaab ")
    Assert.Equal(ValueSome 5, ism)


[<Fact>]
let ``matchend test 3``() =
    let matcher = Regex(@".*b|a")
    let ism = matcher.FindMatchEnd(" aaab ")
    Assert.Equal(ValueSome 5, ism)

[<Fact>]
let ``matchend test 4``() =
    let matcher = Regex(@"a+")
    let ism = matcher.FindMatchEnd(" aaa ")
    Assert.Equal(ValueSome 4, ism)






// [<Fact>]
// let ``matchend test 3``() =
//     let matcher = Matcher(@"b.*|a")
//     let ism = matcher.FindMatchEnd("baaa ")
//     Assert.Equal(ValueSome 4, ism)



// [<Fact>]
// let ``subsumption true star epsilon`` () = testPartDerivative (@"(aa&⊤*)", "aa", @"a")



#endif


[<Fact>]
let ``deriv flags 1``() =
    let regex = Regex("a{1,3}b")
    let matcher = regex.TSetMatcher
    let state = RegexState()
    let d1 = getNodeDerivative(regex, state, matcher.InitialPattern,"a")
    let flags = d1.GetFlags()
    // (⊤*⟨a{1,3}b⟩|⟨a{1,3}b⟩)
    Assert.Equal(Flag.HasCounterFlag, flags)
    let counter = state.ActiveCounters.Values |> Seq.head
    Assert.Equal(counter.Offset, 1)
    // assertPatternIn


[<Fact>]
let ``deriv flags 2``() =
    let regex = Regex("a{1,3}b")
    let matcher = regex.TSetMatcher
    let state = RegexState()
    let d1 = getNodeDerivative(regex, state, matcher.InitialPattern,"a")
    let counter = state.ActiveCounters.Values |> Seq.head
    let d2 = getNodeDerivative(regex, state, d1, "a")
    Assert.Equal(counter.Offset, 2)
    let d3 = getNodeDerivative(regex, state, d2, "a")
    Assert.Equal(counter.Offset, 3)
    let d4 = getNodeDerivative(regex, state, d3, "b")
    Assert.Equal(counter.Offset, 0)
    Common.assertPatternIn ["(⊤*⟨a{1,3}b⟩|ε)"; "(ε|⊤*⟨a{1,3}b⟩)"] d4

[<Fact>]
let ``deriv flags 3``() =
    let regex = Regex(@"\d{2}$")
    let matcher = regex.TSetMatcher
    let state = RegexState()
    let d1 = getNodeDerivative(regex, state, matcher.InitialPattern,"1")
    let counter =
        state.ActiveCounters.Values |> Seq.head
    let d2 = getNodeDerivative(regex, state, d1, "2")
    Assert.Equal(counter.Offset, 2)
    let d3 = getNodeDerivative(regex, state, d2, "a")
    Assert.Equal(counter.Offset, 3)
    let d4 = getNodeDerivative(regex, state, d3, "b")
    Assert.Equal(counter.Offset, 0)
    Common.assertPatternIn ["(⊤*⟨a{1,3}b⟩|ε)"; "(ε|⊤*⟨a{1,3}b⟩)"] d4

