[<Xunit.Collection("Sequential")>]
module Sbre.Test._04_DerivativeTests

#if DEBUG

open Sbre
open Sbre.Algorithm
open Sbre.Pat
open Sbre.Types
open Xunit

let getDerivative(matcher: Regex, input: string) =
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input 0)
    // let matchCache = RegexMatchCache(cache,node)
    createDerivative (cache, location, cache.MintermForLocation(location), node)



let testFullDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.ImplicitPattern
    let location = (Location.create input 0)

    let result =
        createDerivative (cache, location, cache.MintermForLocation(location), node)
        |> cache.PrintNode

    Assert.Equal(expectedDerivative, result)


let testFullDerivativeMultiple(pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.ImplicitPattern
    let location = (Location.create input 0)

    let result =
        createDerivative (cache, location, cache.MintermForLocation(location), node)
        |> cache.PrintNode

    Assert.Contains(result, expectedDerivatives)


let test2ndDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.ImplicitPattern
    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let der1 = createDerivative (cache, location, cache.MintermForLocation(location), node)

    let der2 = createDerivative (cache, location1, cache.MintermForLocation(location1), der1)

    let result = der2 |> cache.PrintNode
    let a = 1

    Assert.Equal(expectedDerivative, result)


let test2ndDerivatives(pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.ImplicitPattern
    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let der1 = createDerivative (cache, location, cache.MintermForLocation(location), node)

    let der2 = createDerivative (cache, location1, cache.MintermForLocation(location1), der1)

    let result = der2 |> cache.PrintNode

    Assert.Contains(result, expectedDerivatives)






let testRawDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input 0)

    let result =
        createDerivative (cache, location, cache.MintermForLocation(location), node)
        |> cache.PrintNode

    Assert.Equal(expectedDerivative, result)




let testPartDerivative(pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input 0)
    let result = createDerivative (cache, location, cache.MintermForLocation(location), node)

    let print node = node |> cache.PrintNode

    Assert.Equal(expectedDerivative, print result)


let testPartDerivatives(pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input 0)
    let result = createDerivative (cache, location, cache.MintermForLocation(location), node)

    let print node = node |> cache.PrintNode

    Assert.Contains(print result, expectedDerivatives)




let testPartDerivativeFromLocation
    (
        pattern: string,
        input: string,
        position: int,
        expectedDerivative: string
    )
    =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input position)

    let result =
        match createDerivative (cache, location, cache.MintermForLocation(location), node) with
        | result -> cache.PrintNode result

    Assert.Equal(expectedDerivative, result)


let testPartDerivativeFromLocationMultiple
    (
        pattern: string,
        input: string,
        position: int,
        expectedDerivatives: string list
    )
    =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input position)

    let result =
        match createDerivative (cache, location, cache.MintermForLocation(location), node) with
        | result -> cache.PrintNode result

    Assert.Contains(result, expectedDerivatives)

let testPartDerivativesLoc
    (
        pattern: string,
        loc: Location,
        expectedDerivatives: string list
    )
    =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = loc

    let result =
        match createDerivative (cache, location, cache.MintermForLocation(location), node) with
        | result -> cache.PrintNode result

    Assert.Contains(result, expectedDerivatives)


[<Fact>]
let ``concat derivative nullability``() =
    let pattern = @"(?=.*A)(?=.*a)(?=.*1).{2,2}"
    let location = Pat.Location.create "1aA" 0
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.ReversePattern

    // let result1 =
    //     match createDerivative(cache, location, cache.MintermForLocation(location), node) with
    //     | Concat(head,(Concat(info=info2) as tail),info) ->
    //         let t = tail
    //         Assert.True(info2.CanBeNullable)
    //     | Concat(head,(Concat(info=info2) as tail),info) ->
    //     | x ->
    //         let a = 1
    //         failwith "invalid node"

    // Assert.Equal(expectedDerivative, result)
    ()


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



[<Fact>]
let ``derivative or tail``() =
    testPartDerivative (@"(310|0[1-9]2|452)", "002", "[1-9]2")


[<Fact>]
let ``derivative of plus``() =
    testPartDerivatives (@"^\d+$", "123", [ @"\d*((?=\n)|(?!⊤))"; @"\d*((?!⊤)|(?=\n))" ])


[<Fact>]
let ``derivative concat lookaround``() =
    testPartDerivatives (@"^\d+$", "123", [ @"\d*((?=\n)|(?!⊤))"; @"\d*((?!⊤)|(?=\n))" ])


// [<Fact>]
// let ``derivative conj lookaround`` () =
//     testPartDerivative (""".*rain.*&.*dogs.*""", "raining cats and dogs", @"\d*((?=\n)|(?!⊤))")
//
//


[<Fact>]
let ``derivative lookback 1``() =
    let loc =
        {
            Input = "-aaaa-"
            Position = 0
            Reversed = false
        }
    testPartDerivativesLoc (@"(.*(?=.*-)&\S.*\S)", loc,  [@"(.*φ&.*(?=.*-))";"(.*(?=.*-)&.*φ)"])

//
// [<Fact>]
// let ``derivative lookback 2``() =
//     let loc =
//         {
//             Input = "-aaaa-"
//             Position = 5
//             Reversed = true
//         }
//     testPartDerivativesLoc (@"((?<=-.*).*)", loc,  [@"(?<=A.*)"])
//
//
//
// [<Fact>]
// let ``derivative lookback 3``() =
//     let loc =
//         {
//             Input = "-aaaa-"
//             Position = 5
//             Reversed = true
//         }
//     testPartDerivativesLoc (@"((?<=-.*).*&\S.*\S)", loc,  [@"(?<=A.*)"])



[<Fact>]
let ``subsumption or loop ``() =
    testPartDerivative (@"(a*|.*)", "aaa", @".*")


[<Fact>]
let ``subsumption and loop ``() =
    testPartDerivative (@"(.*&.*s)", "aaa", @".*s")


// [<Fact>]
// let ``subsumption negation nullable ``() =
//     testPartDerivative (@"(.*(?=.*E)&~(d.*))", "dd", @"⊥")


[<Fact>]
let ``subsumption or loop limited 1``() =
    test2ndDerivatives (
        "[a-z]{0,10}y",
        "ccccc",
        [ @"(⊤*[a-z]{0,10}y|[a-z]{0,9}y)"; @"([a-z]{0,9}y|⊤*[a-z]{0,10}y)" ]
    )


// [<Fact>]
// let ``derivative or subsumption`` () = testPartDerivative (@"(aaa|[\s\S]*)", "aaa", @"⊤*")

// [<Fact>]
// let ``derivative and 1`` () = testPartDerivative (@"⊤*B&⊤*A⊤*", "BHBA", @"(⊤*A⊤*&(|⊤*B))")
//
//
// [<Fact>]
// let ``derivative and 2`` () =
//     testPartDerivative (
//         @"[\s\S]*French&[\s\S]*English[\s\S]*",
//         "French, English _______",
//         @"(⊤*English⊤*&(rench|⊤*French))")
//
//
// [<Fact>]
// let ``derivative and 3`` () =
//     testPartDerivativeFromLocation (
//         @"(.*rain.*|.*)&(.*dogs.*|ogs.*)",
//         "raining cats and dogs", 16,
//         @"(.*dogs.*&(.*|.*rain.*))")
//


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


[<Fact>]
let ``matchend test 5``() =
    let matcher = Regex(@".*(?=.*E)&~(.*and.*)")
    let ism = matcher.FindMatchEnd(@"and__E")
    Assert.Equal(ValueSome 2, ism)





// [<Fact>]
// let ``matchend test 3``() =
//     let matcher = Matcher(@"b.*|a")
//     let ism = matcher.FindMatchEnd("baaa ")
//     Assert.Equal(ValueSome 4, ism)



// [<Fact>]
// let ``subsumption true star epsilon`` () = testPartDerivative (@"(aa&⊤*)", "aa", @"a")



#endif
