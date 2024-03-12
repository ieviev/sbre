[<Xunit.Collection("Sequential")>]
module Sbre.Test.NegationTests

open Sbre
open Xunit
open Common
//
#if DEBUG

// TODO: reimplement
[<Fact>]
let ``negation test 1: password``() =
    assertFirstMatchText @"~(.*\d\d.*)" "Aa11aBaAA" "Aa1"

[<Fact>]
let ``negation test 2: password``() =
    assertNoMatch @"~(.*\d\d.*)&^.*$" "Aa11aBaAA"


[<Fact>]
let ``negation test 2.1: range``() =
    assertFirstMatchText @"a~(.*bc⊤*)" ("a    bc dasdad") "a    b"



[<Fact>]
let ``negation test 3: shortest match from right?``() =
    assertFirstMatchText (@"asd~(⊤*nl⊤*)") "cvbcbcvasdasd nl adasd asdasd" "asdasd n"


[<Fact>]
let ``negation skipping test``() =
    assertAllLLmatches @"~(.*\d\d.*)" "Aa11aBabb115454vbsdvfv45AAZ" [
        (0, 3)
        (3, 7)
        (10, 1)
        (11, 1)
        (12, 1)
        (13, 1)
        (14, 9);(23, 4);(27, 0)
    ]






//
//
//


let twainExample1 = """


Thursday, April 16.  Went ashore in the forenoon at Port Louis, a little
town, but with the largest variety of nationalities and complexions we
have encountered yet.  French, English, Chinese, Arabs, Africans with
wool, blacks with straight hair, East Indians, half-whites, quadroons--
and great varieties in costumes and colors.

"""



// [<Fact>]
// let ``negation test 5: range``() =
//     let matcher = Regex(@"Thursday~(⊤*Indians⊤*)")
//     let result = matcher.MatchText(twainExample1)
//     Assert.True(result.Value.StartsWith("Thursday") && result.Value.EndsWith("Indian"), $"got: {result}")
//
//
// [<Fact>]
// let ``negation test 6: range``() =
//     let matcher = Regex(@"King~(⊤*\d\d⊤*)Paris")
//     let result = matcher.MatchText("The King in Paris asd def")
//     Assert.Equal(Some "King in Paris",result)
//
//
// let twainExample2 = """French, English, Chinese, Arabs, African"""
//
// [<Fact>]
// let ``negation test 7: range does not contain``() =
//     let matcher = Regex(@"French~(⊤*Chinese⊤*)Arabs")
//     let result = matcher.MatchText(twainExample2)
//     Assert.Equal(result, None)
//
// [<Fact>]
// let ``negation test 8: range does not contain - pos example``() =
//     let matcher = Regex(@"French~(⊤*SomethingElse⊤*)Arabs")
//     let result = matcher.MatchText(twainExample2)
//     Assert.Equal(Some("French, English, Chinese, Arabs"), result)
//
//
// [<Fact>]
// let ``negation test 9: mixing conjunction and until``() =
//     let matcher = Regex(@"English~(⊤*Ara⊤*)&⊤*Chinese⊤*")
//     let result = matcher.MatchText(twainExample2)
//     Assert.Equal(result, Some("English, Chinese, Ar"))
//

// TODO: too expensive
// [<Fact>]
// let ``negation test 10: mtwain english french 2 endpoint``() =
//     let matcher = Matcher(@"\n\n~(⊤*\n\n⊤*)\n\n&⊤*English⊤*&⊤*French⊤*")
//     let result = matcher.FindMatchEnd(Samples.MarkTwainText)
//     Assert.Equal(result, ValueSome 17746)


// [<Fact>]
// let ``negation lorem ipsum test``() =
//     let matcher = Regex(@"~(⊤*Lorem⊤*)")
//     let result = matcher.MatchText(loremIpsum)
//     Assert.Equal(Some " Lore", result)


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


let sampleCode = """type [<AllowNullLiteral>] GraphStyleSpecification =
    abstract version: int with get, set
    abstract layers: ResizeArray<U2<GraphNodeLayerStyle, GraphEdgeLayerStyle>> with get, set

type [<AllowNullLiteral>] EntityFilter =
    abstract property: EntityFilterProperty with get, set
    abstract operator: EntityFilterOperator with get, set
    abstract value: U2<string, float> with get, set

type [<AllowNullLiteral>] InterpolatorContext =
    abstract zoom: float with get, set

type [<AllowNullLiteral>] Interpolation<'OutputT> =
    abstract interpolation: InterpolationInterpolation with get, set
    abstract interpolationParameters: ResizeArray<float> option with get, set
    abstract input: KeyOf<InterpolatorContext> with get, set
    abstract inputStops: ResizeArray<float> with get, set
    abstract outputStops: ResizeArray<'OutputT> with get, set

type [<AllowNullLiteral>] GraphLayerStyle =
    abstract id: string option with get, set
    abstract ``type``: string with get, set
    abstract filter: U2<EntityFilter, ResizeArray<EntityFilter>> option with get, set
    abstract visible: bool option with get, set
    abstract minZoom: float option with get, set
    abstract maxZoom: float option with get, set"""

let sampleCode2 = """
type Context1 =
    get, set
    get, set

type Context2 =
    get, set
    get, set
    get, set

type Context3=
    get, set

type Context4 =
    get, set
    get, set

"""





[<Fact>]
let ``test1``() =
    let pattern = """~(\T*\n\n\T*)&(?<=Context1~(\T*\n\n\T*))\T*&get, set"""
    assertAllLLmatchTexts pattern sampleCode2 [
        "get, set" ; "get, set"
    ]


[<Fact>]
let ``test2``() =
    let pattern = """(?<=Context1~(\T*\n\n\T*)).*&get, set"""
    assertAllLLmatchTexts pattern sampleCode2 [
        "get, set" ; "get, set"
    ]


[<Fact>]
let ``test3``() =
    let pattern = """(?<=Context1~(\T*\n\n\T*))(.*&get, set)"""
    assertAllLLmatchTexts pattern sampleCode2 [
        "get, set" ; "get, set"
    ]













#endif