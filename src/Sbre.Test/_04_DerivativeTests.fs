[<Xunit.Collection("Sequential")>]
module Sbre.Test._04_DerivativeTests

#if DEBUG

open Sbre
open Sbre.Regex
open Sbre.Pat
open Sbre.Types
open Xunit

let getDerivative (matcher: Matcher, input: string) =
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input 0)
    // let matchCache = RegexMatchCache(cache,node)
    createDerivative (cache, location, cache.MintermForLocation(location), node)



let testFullDerivative (pattern: string, input: string, expectedDerivative: string) =
    let matcher = Matcher(pattern)
    let cache = matcher.Cache
    let node = matcher.DotStarredPattern
    let location = (Location.create input 0)
    let result = createDerivative(cache, location, cache.MintermForLocation(location), node) |> cache.PrintNode

    Assert.Equal(expectedDerivative, result)


let testFullDerivativeMultiple (pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Matcher(pattern)
    let cache = matcher.Cache
    let node = matcher.DotStarredPattern
    let location = (Location.create input 0)
    let result = createDerivative(cache, location, cache.MintermForLocation(location), node) |> cache.PrintNode

    Assert.Contains(result, expectedDerivatives)


let test2ndDerivative (pattern: string, input: string, expectedDerivative: string) =
    let matcher = Matcher(pattern)
    let cache = matcher.Cache
    let node = matcher.DotStarredPattern
    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let der1 =
        createDerivative (cache, location, cache.MintermForLocation(location), node)

    let der2 =
        createDerivative (cache, location1, cache.MintermForLocation(location1), der1)

    let result = der2 |> cache.PrintNode
    let a = 1

    Assert.Equal(expectedDerivative, result)


let test2ndDerivatives (pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Matcher(pattern)
    let cache = matcher.Cache
    let node = matcher.DotStarredPattern
    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let der1 =
        createDerivative (cache, location, cache.MintermForLocation(location), node)

    let der2 =
        createDerivative (cache, location1, cache.MintermForLocation(location1), der1)

    let result = der2 |> cache.PrintNode

    Assert.Contains(result, expectedDerivatives)






let testRawDerivative (pattern: string, input: string, expectedDerivative: string) =
    let matcher = Matcher(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input 0)
    let result =
        createDerivative(cache,  location, cache.MintermForLocation(location), node)
        |> cache.PrintNode

    Assert.Equal(expectedDerivative, result)




let testPartDerivative (pattern: string, input: string, expectedDerivative: string) =
    let matcher = Matcher(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input 0)
    let result =
        createDerivative(cache, location, cache.MintermForLocation(location), node)

    let print node = node |> cache.PrintNode

    Assert.Equal(expectedDerivative, print result)


let testPartDerivatives (pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Matcher(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input 0)
    let result =
        createDerivative(cache, location, cache.MintermForLocation(location), node)

    let print node = node |> cache.PrintNode

    Assert.Contains(print result,expectedDerivatives)




let testPartDerivativeFromLocation (pattern: string, input: string, position:int, expectedDerivative: string) =
    let matcher = Matcher(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input position)
    let result =
        match createDerivative(cache, location, cache.MintermForLocation(location), node) with
        | result -> cache.PrintNode result

    Assert.Equal(expectedDerivative, result)


let testPartDerivativeFromLocationMultiple (pattern: string, input: string, position:int, expectedDerivatives: string list) =
    let matcher = Matcher(pattern)
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input position)
    let result =
        match createDerivative(cache, location, cache.MintermForLocation(location), node) with
        | result -> cache.PrintNode result

    Assert.Contains(result, expectedDerivatives)



[<Fact>]
let ``concat derivative nullability``()  =
    let pattern = @"(?=.*A)(?=.*a)(?=.*1).{2,2}"
    let location = Pat.Location.create "1aA" 0
    let matcher = Matcher(pattern)
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
let ``raw derivative of ab`` () = testRawDerivative ("ab", "ab", "b")

[<Fact>]
let ``derivative of ab`` () = testFullDerivativeMultiple ("ab", "ab", ["(b|⊤*ab)"; @"(⊤*ab|b)"])


[<Fact>]
let ``derivative of true`` () = testPartDerivative ("⊤", "324", "ε")


[<Fact>]
let ``derivative of true ismatch`` () =

    testPartDerivative ("⊤", "324", "ε")



// [<Fact>]
// let ``derivative of Twain`` () =
//     testFullDerivative ("Twain", "Twain", "(wain|⊤*Twain)")


[<Fact>]
let ``2 derivative of Twain`` () =
    test2ndDerivatives ("Twain", "Twain", ["(ain|⊤*Twain)"; @"(⊤*Twain|ain)"])
    // test2ndDerivative ("Twain", "Twain", "(⊤*Twain|ain)")

[<Fact>]
let ``derivative lookaround 1`` () = testPartDerivatives (@"^\d$", "1", [@"((?=\n)|(?!⊤))"; @"((?!⊤)|(?=\n))"])

[<Fact>]
let ``derivative lookaround 1.2`` () = testPartDerivative (@"(?<!\w)11", "11", "1")

[<Fact>]
let ``derivative lookaround 1.3`` () = testPartDerivative (@"(?=1)11", "11", "1")



[<Fact>]
let ``derivative lookaround 2`` () = testPartDerivative (@"\b11", "11", "1")

//([]1|1)

[<Fact>]
let ``derivative boundary 1`` () = testPartDerivativeFromLocation (@"(?<=\s)22", "1 2", 2, "2")


[<Fact>]
let ``derivative boundary 2`` () = testPartDerivativeFromLocation (@"\b22", "1 2", 2, "2")


// [<Fact>]
// let ``derivative boundary 3`` () = testPartDerivativeFromLocation (@"\b1\b", "1 ", 0, "((?<!φ)(?=φ)|(?<=φ)(?!φ))")


[<Fact>]
let ``derivative boundary 4`` () = testPartDerivativeFromLocation (@"(?<=\d)a", "1a", 1, "ε")


[<Fact>]
let ``derivative boundary 5`` () = testPartDerivativeFromLocation (@"(?=\w)a", "1a", 1, "ε")



[<Fact>]
let ``derivative or tail`` () = testPartDerivative (@"(310|0[1-9]2|452)", "002", "[1-9]2")


[<Fact>]
let ``derivative of plus`` () = testPartDerivatives (@"^\d+$", "123", [@"\d*((?=\n)|(?!⊤))";@"\d*((?!⊤)|(?=\n))"])


[<Fact>]
let ``derivative concat lookaround`` () = testPartDerivatives (@"^\d+$", "123", [@"\d*((?=\n)|(?!⊤))"; @"\d*((?!⊤)|(?=\n))"])


// [<Fact>]
// let ``derivative conj lookaround`` () =
//     testPartDerivative (""".*rain.*&.*dogs.*""", "raining cats and dogs", @"\d*((?=\n)|(?!⊤))")
//
//

[<Fact>]
let ``derivative lookback 1`` () =
    // testPartDerivativeFromLocation (@"..(?<=A.*)", "Aa", 2,  @"\d*((?=\n)|(?!⊤))")
    // testPartDerivativeFromLocation (@".(?<=A.*)", "Aa", 1,  @"\d*((?=\n)|(?!⊤))")
    testPartDerivativeFromLocation (@".(?<=A.*)", "Aa", 1,  @"(?<=A.*)")



[<Fact>]
let ``subsumption or loop `` () = testPartDerivative (@"(a*|.*)", "aaa", @".*")


[<Fact>]
let ``subsumption and loop `` () = testPartDerivative (@"(.*&.*s)", "aaa", @".*s")


[<Fact>]
let ``subsumption or loop limited 1`` () = test2ndDerivatives ("[a-z]{0,10}y", "ccccc", [
    @"(⊤*[a-z]{0,10}y|[a-z]{0,9}y)"
    @"([a-z]{0,9}y|⊤*[a-z]{0,10}y)"
])


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
let ``derivative eats node from set`` () =
    testPartDerivativeFromLocationMultiple (
        @"^((0?[13578]a)|(0?[13456789]a))$",
        "4a", 0,
        [@"a((?=\n)|(?!⊤))"; @"a((?!⊤)|(?=\n))" ])




// [<Fact>]
// let ``subsumption true star epsilon`` () = testPartDerivative (@"(aa&⊤*)", "aa", @"a")



#endif