[<Xunit.Collection("Sequential")>]
module Sbre.Test.NegationTests

open Sbre
open Sbre.Pat
open Xunit
//


[<Fact>]
let ``negation test 1: password``() =
    let matcher = Matcher(@"~(.*\d\d.*)")
    let result = matcher.MatchWithoutOptimizations("Aa11aBaAA")
    Assert.Equal(result, Some "Aa1")

[<Fact>]
let ``negation test 2: password``() =
    let matcher = Matcher(@"~(.*\d\d.*)&^.*$")
    let result = matcher.MatchWithoutOptimizations("Aa11aBaAA")
    Assert.Equal(None, result)


[<Fact>]
let ``negation test 2.1: range``() =
    let matcher = Matcher(@"a~(.*bc⊤*)")
    let result = matcher.MatchWithoutOptimizations("a    bc dasdad")
    Assert.Equal(Some("a    b"), result)


[<Fact>]
let ``negation test 3: shortest match from right?``() =
    let matcher = Matcher(@"asd~(⊤*nl⊤*)")
    let result = matcher.MatchWithoutOptimizations("cvbcbcvasdasd nl adasd asdasd")
    Assert.Equal(Some("asdasd n"), result)




[<Fact>]
let ``negation test 4: until semantics``() =
    let matcher = Matcher(@"a~(⊤*e⊤*)")
    let result = matcher.MatchWithoutOptimizations("abcdefghijklmnop")
    Assert.Equal(result, Some("abcd"))


let twainExample1 = """


Thursday, April 16.  Went ashore in the forenoon at Port Louis, a little
town, but with the largest variety of nationalities and complexions we
have encountered yet.  French, English, Chinese, Arabs, Africans with
wool, blacks with straight hair, East Indians, half-whites, quadroons--
and great varieties in costumes and colors.

"""



[<Fact>]
let ``negation test 5: range``() =
    let matcher = Matcher(@"Thursday~(⊤*Indians⊤*)")
    let result = matcher.MatchWithoutOptimizations(twainExample1)
    Assert.True(result.Value.StartsWith("Thursday") && result.Value.EndsWith("Indian"), $"got: {result}")


[<Fact>]
let ``negation test 6: range``() =
    let matcher = Matcher(@"King~(⊤*\d\d⊤*)Paris")
    let result = matcher.MatchWithoutOptimizations("The King in Paris asd def")
    Assert.Equal(Some "King in Paris",result)


let twainExample2 = """French, English, Chinese, Arabs, African"""

[<Fact>]
let ``negation test 7: range does not contain``() =
    let matcher = Matcher(@"French~(⊤*Chinese⊤*)Arabs")
    let result = matcher.MatchWithoutOptimizations(twainExample2)
    Assert.Equal(result, None)

[<Fact>]
let ``negation test 8: range does not contain - pos example``() =
    let matcher = Matcher(@"French~(⊤*SomethingElse⊤*)Arabs")
    let result = matcher.MatchWithoutOptimizations(twainExample2)
    Assert.Equal(result, Some("French, English, Chinese, Arabs"))


[<Fact>]
let ``negation test 9: mixing conjunction and until``() =
    let matcher = Matcher(@"English~(⊤*Ara⊤*)&⊤*Chinese⊤*")
    let result = matcher.MatchWithoutOptimizations(twainExample2)
    Assert.Equal(result, Some("English, Chinese, Ar"))


// TODO: too expensive
// [<Fact>]
// let ``negation test 10: mtwain english french 2 endpoint``() =
//     let matcher = Matcher(@"\n\n~(⊤*\n\n⊤*)\n\n&⊤*English⊤*&⊤*French⊤*")
//     let result = matcher.FindMatchEnd(Samples.MarkTwainText)
//     Assert.Equal(result, ValueSome 17746)



let loremIpsum = """ Lorem Ipsum."""

[<Fact>]
let ``negation lorem ipsum test``() =
    let matcher = Matcher(@"~(Lorem⊤*) ")
    let result = matcher.MatchText(loremIpsum)
    Assert.Equal(Some "orem ", result)





// [<Fact>]
// let ``negation test 11: mtwain english french 4 endpoint``() =
//     let matcher = Matcher(@"\n\n~(⊤*\n\n⊤*)\n\n&⊤*English⊤*&⊤*French⊤*&⊤*Joan⊤*&⊤*prisoner⊤*")
//     let result = matcher.FindMatchEnd(PerformanceTests.MarkTwainText)
//     Assert.Equal(result, ValueSome 9425056)

// takes long
// [<Fact>]
// let ``negation test 11: mtwain endpoint``() =
//     let matcher = Matcher(@"\n\n~(⊤*\n\n⊤*)\n\n&⊤*untraveled⊤*&⊤*alias⊤*")
//     let result = matcher.FindMatchEnd(PerformanceTests.MarkTwainText)
//     Assert.Equal(result, ValueSome 1228814)


// [<Fact>]
// let ``negation test 12: mtwain endpoint``() =
//     let matcher = Matcher(@"\n\n~(⊤*\n\n⊤*)\n\n&⊤*run⊤*&⊤*gentle⊤*")
//     let result = matcher.FindMatchEnd(PerformanceTests.MarkTwainText)
//     Assert.Equal(result, ValueSome 69768)
//
