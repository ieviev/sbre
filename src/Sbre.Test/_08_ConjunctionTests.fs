[<Xunit.Collection("Sequential")>]
module Sbre.Test._08_ConjunctionTests

open NuGet.Frameworks
open Sbre
open Sbre.Benchmarks.Jobs
open Xunit
open Common

#if DEBUG


[<Fact>]
let ``conjunction match test 1`` () =
    assertFirstMatchText
        """c...&...s"""
        "raining cats and dogs"
        "cats"


[<Fact>]
let ``conjunction match test 2`` () =

    let pattern = """c.*&.*s"""
    let input = "cats blah blah blah"
    assertFirstMatchText
        pattern
        input
        "cats"

//
//
[<Fact>]
let ``conjunction match test 3`` () =
    let pattern = """.*rain.*&.*dogs.*"""
    let input = "raining cats and dogs"
    assertFirstMatchText
        pattern
        input
        input


[<Fact>]
let ``conjunction match test 4`` () =
    let pattern = """and.*&.*dogs.*"""
    let input = "raining cats and dogs"
    assertFirstMatchText
        pattern
        input
        "and dogs"


[<Fact>]
let ``conjunction match test 5`` () =
    let pattern = """⊤*A⊤*&⊤*B""" // [\s\S]*
    let input = "B  A"
    assertNoMatch
        pattern
        input


// [<Fact>]
// let ``conjunction match test 7`` () =
//     let pattern = """.*rain.*&.*dogs.*&.*ogs.*""" // [\s\S]*
//     let input = "raining cats and dogs"
//     let matcher = Matcher(pattern)
//     let result = matcher.Match(input)
//     Assert.Equal(Some "", result) // English occurs after French
//




let marktwainsample =
    """

    The portals of the great western front are bisected by square pillars.
    They took the central one away in 1852, on the occasion of thanksgivings
    for the reinstitution of the presidential power--but precious soon they
    had occasion to reconsider that motion and put it back again!  And they
    did.

    We loitered through the grand aisles for an hour or two, staring up at
    the rich stained-glass windows embellished with blue and yellow and
    crimson saints and martyrs, and trying to admire the numberless great
    pictures in the chapels, and then we were admitted to the sacristy and
    shown the magnificent robes which the Pope wore when he crowned Napoleon
    I; a wagon-load of solid gold and silver utensils used in the great
    public processions and ceremonies of the church; some nails of the true
    cross, a fragment of the cross itself, a part of the crown of thorns.
    We had already seen a large piece of the true cross in a church in the
    Azores, but no nails.  They showed us likewise the bloody robe which that
    archbishop of Paris wore who exposed his sacred person and braved the
    wrath of the insurgents of 1848, to mount the barricades and hold aloft
    the olive branch of peace in the hope of stopping the slaughter.  His
    noble effort cost him his life.  He was shot dead.  They showed us a cast
    of his face taken after death, the bullet that killed him, and the two
    vertebrae in which it lodged.  These people have a somewhat singular
    taste in the matter of relics.  Ferguson told us that the silver cross
    which the good archbishop wore at his girdle was seized and thrown into
    the Seine, where it lay embedded in the mud for fifteen years, and then
    an angel appeared to a priest and told him where to dive for it; he did
    dive for it and got it, and now it is there on exhibition at Notre Dame,
    to be inspected by anybody who feels an interest in inanimate objects of
    miraculous intervention.

    """


let twainExampleShort =
    """

    Thursday, April 16.  Went ashore in the forenoon at Port Louis, a little
    town, but with the largest variety of nationalities and complexions we
    have encountered yet.  French, English, Chinese, Arabs, Africans with
    wool, blacks with straight hair, East Indians, half-whites, quadroons--
    and great varieties in costumes and colors.

    """

[<Fact>]
let ``more than 3 cases short`` () =
    let pattern =
        """⊤*Thursday⊤*&⊤*April⊤*&⊤*Went⊤*&⊤*ashore⊤*""" // [\s\S]*

    let input = twainExampleShort
    assertFirstMatchText
        pattern
        input
        input

[<Fact>]
let ``more than 3 cases short 2`` () =
    let pattern =
        """[\s\S]*French[\s\S]*&[\s\S]*English[\s\S]*&[\s\S]*Chinese[\s\S]*&[\s\S]*Arabs[\s\S]*""" // [\s\S]*

    let input = "French English Chinese Arabs asdasd"
    assertFirstMatchText
        pattern
        input
        input


[<Fact>]
let ``twain match test 1`` () =
    let pattern =
        """[\s\S]*English[\s\S]*&[\s\S]*French""" // [\s\S]*

    let input = twainExampleShort
    assertNoMatch
        pattern
        input


[<Fact>]
let ``twain match test more than 3 cases 1`` () =
    let pattern =
        """[\s\S]*French[\s\S]*&[\s\S]*English[\s\S]*&[\s\S]*Chinese[\s\S]*&[\s\S]*Arabs[\s\S]*""" // [\s\S]*
    let input = twainExampleShort
    assertFirstMatchText pattern input input







let ``twain match test 2`` () =
    let pattern =
        """[\s\S]*French[\s\S]*&[\s\S]*English""" // [\s\S]*

    let input = twainExampleShort
    let matcher = Regex(pattern)
    // let result = matcher.FindMatchEnd(input)
    assertFirstMatch pattern input (0,195)
    // Assert.Equal(ValueSome 195, result) // English occurs after French


[<Fact>]
let ``twain match test 3`` () =
    let pattern =
        """[\s\S]*French[\s\S]*&[\s\S]*English""" // [\s\S]*
    let input = twainExampleShort
    let result =
        getFirstLLmatchText
            pattern
            input
    assertTrue (result.EndsWith("English"))

[<Fact>]
let ``twain match test 4`` () =
    let pattern =
        """[\s\S]*French[\s\S]*&English[\s\S]*""" // [\s\S]*

    let input = "English, French asdasd"
    assertFirstMatchText
        pattern
        input
        input

let twainExample2 =
    """

But behold how annoyances repeat themselves.  We had no sooner gotten rid
of the Spain distress than the Gibraltar guides started another--a
tiresome repetition of a legend that had nothing very astonishing about
it, even in the first place: "That high hill yonder is called the Queen's
Chair; it is because one of the queens of Spain placed her chair there
when the French and Spanish troops were besieging Gibraltar, and said she
would never move from the spot till the English flag was lowered from the
fortresses.  If the English hadn't been gallant enough to lower the flag
for a few hours one day, she'd have had to break her oath or die up
there."


"""


// [<Fact>]
// let ``conjunction match test 5``() =
//     let pattern = """.*Huck.*&.*Tom.*"""
//     let input = twainExample2
//     let matcher = Matcher(pattern)
//     let result = matcher.Match(input)
//     Assert.Equal(Some input,result)


// [<Fact>]
// let ``conjunction match test 5``() =
//     let pattern = """.* it .*&.* to .*"""
//     let matcher = Matcher(pattern)
//     let result = matcher.Match(marktwainsample)
//     Assert.Equal(Some "and dogs", result)
//


let twainExample3 =
    """

"I think, m'm, they--"

"Now, Tom Sawyer, what kind of a lie are you fixing YOUR
mouth to contribit to this mess of rubbage? Speak out--and
I warn you before you begin, that I don't believe a word
of it.  You and Huck's been up to something you no business
to--I know it perfectly well; I know you, BOTH of you.
Now you explain that dog, and them blackberries,
and the lantern, and the rest of that rot--and mind you
talk as straight as a string--do you hear?"

Tom he looked considerable hurt, and says, very dignified:

"It is a pity if Huck is to be talked to that way,
just for making a little bit of a mistake that anybody
could make."

"What mistake has he made?"

"Why, only the mistake of saying blackberries when
of course he meant strawberries."

"Tom Sawyer, I lay if you aggravate me a little more, I'll--"

"Aunt Sally, without knowing it--and of course without
intending it--you are in the wrong.  If you'd 'a' studied
natural history the way you ought, you would know that
all over the world except just here in Arkansaw they
ALWAYS hunt strawberries with a dog--and a lantern--"

But she busted in on him there and just piled into him
and snowed him under.  She was so mad she couldn't get
the words out fast enough, and she gushed them out
in one everlasting freshet.  That was what Tom Sawyer
was after.  He allowed to work her up and get her started
and then leave her alone and let her burn herself out.
Then she would be so aggravated with that subject
that she wouldn't say another word about it, nor let AAAAAAAAAAA
anybody else.  Well, it happened just so.  When she
was tuckered out and had to hold up, he says, quite ca'm:

"And yet, all the same, Aunt Sally--"

"Shet up!" she says, "I don't want to hear another word
out of you."


"""



// [<Fact>]
// let ``twain paragraph test 1``() =
//     let pattern = @"\n\n~(⊤*\n\n⊤*)\n\n&(⊤*(Huck|Finn)⊤*)"
//     let input = twainExample3
//     let matcher = Regex(pattern)
//     let result = matcher.FindMatchEnd(input)
//     Assert.Equal(ValueSome 464, result)
//
// [<Fact>]
// let ``twain paragraph test 2``() =
//     let pattern = @"\n\n~(⊤*\n\n⊤*)\n\n&⊤*Huck⊤*&⊤*Sawyer⊤*"
//     let input = twainExample3
//     let matcher = Regex(pattern)
//     let result = matcher.FindMatchEnd(input)
//     Assert.Equal(ValueSome 464, result)


//
//
// [<Fact>]
// let ``twain paragraph test 4``() =
//     let pattern = @"\n\n~(⊤*\n\n⊤*)\n\n&⊤*(Arkansaw)⊤*"
//     let input = twainExample3
//     let matcher = Regex(pattern)
//     let result = matcher.FindMatchEnd(input)
//     Assert.Equal(ValueSome 1098, result)


[<Fact>]
let ``twain paragraph test 5``() =
    let pattern = @"\n\n~(⊤*\n\n⊤*)\n\n&⊤*(Arkansaw)⊤*"
    let input = twainExample3
    let expectedParagraph = """

"Aunt Sally, without knowing it--and of course without
intending it--you are in the wrong.  If you'd 'a' studied
natural history the way you ought, you would know that
all over the world except just here in Arkansaw they
ALWAYS hunt strawberries with a dog--and a lantern--"

"""
    assertFirstMatchText
        pattern
        input
        expectedParagraph




[<Fact>]
let ``reverse startset test 1``() =
    let m = Regex(Permutations.permuteConjInParagraph [ "c[abci]*i";])
    let r = m.MatchPositions("aaa caaiaa bbb\n\n") |> Seq.toArray
    Assert.Equal(1,r.Length)


[<Fact>]
let ``reverse startset test 2``() =
    let pat = Permutations.permuteConjInParagraph [ @"[a-z]*a[a-z]*";]
    let result=getAllLLmatches pat marktwainsample
    Assert.Equal(2,result.Count)


let shortPg = """

"What mistake has he made?"

"mistake of saying strawberries."

"""


let shortPg2 = """
"honor of
name "
"""


[<Fact>]
let ``implication 1 ``() =
    let pattern = @"(?<=\n\n|\A)(~(⊤*\n\n⊤*)&(~(⊤*mistake⊤*)|(⊤*strawberries⊤*)))(?=\n\n)"
    assertAllLLmatchTexts pattern shortPg ["";"\"mistake of saying strawberries.\""]


[<Fact>]
let ``implication 2 ``() =
    let pattern = @"\n~(⊤*\n\n⊤*)\n&~(⊤*honor⊤*)"
    let matcher1 = Regex(pattern)
    let result1 =
        matcher1.MatchPositions(shortPg2)
        |> Seq.toArray
    Assert.Equal(1, result1.Length)

[<Fact>]
let ``implication 3 ``() =
    let pattern = @"~(⊤*\n\n⊤*)\n&~(⊤*honor⊤*)"
    let result1 =
        getAllLLmatches pattern shortPg2
    Assert.Equal(2, result1.Count)

// indicates problem with second startset
[<Fact>]
let ``script test 1``() =
    let pattern =
        [ "THE.*LIFE"; @".*FIVE.*" ]
        |> String.concat "&"

    let input = @"
      EDWARD MILLS AND GEORGE BENTON:  A TALE
      THE FIVE BOONS OF LIFE
      THE FIRST WRITING-MACHINES
"

    let matcher = Regex(pattern)
    let result = matcher.Match(input)
    Assert.Equal(result.Value, "THE FIVE BOONS OF LIFE")






#endif















