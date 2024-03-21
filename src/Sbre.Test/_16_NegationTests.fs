[<Xunit.Collection("Sequential")>]
module Sbre.Test._16_NegationTests

open Sbre
open Xunit
open Common
//
#if DEBUG

[<Fact>]
let ``negation 1: ~(.*b) can cross over multiple lines``() =
    assertAllLLmatches
        """(a⊤*&(~(a.*)|.*b>))"""
        "a_____a____________________b\n" [ (0, 29) ]


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