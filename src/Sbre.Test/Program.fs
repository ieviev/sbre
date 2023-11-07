// module Program = let [<EntryPoint>] main _ = 0

module Program

open System
open System.Globalization
open System.IO
open System.Text.RuntimeRegexCopy
open Sbre
open Sbre.Pat
open Sbre.Algorithm
open Sbre.Test
open Sbre.Types
open Sbre.Test._04_DerivativeTests
//
// let matcher = Matcher(@"^\d$")
//
// let input = "1"
// let result = matcher.MatchFromLocation(input, Location.create input 0 )





let w = 1

let twainExampleLong =
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
that she wouldn't say another word about it, nor let
anybody else.  Well, it happened just so.  When she
was tuckered out and had to hold up, he says, quite ca'm:

"And yet, all the same, Aunt Sally--"

"Shet up!" she says, "I don't want to hear another word
out of you."

"""

let twainExampleShort = """


Thursday, April 16.  Went ashore in the forenoon at Port Louis, a little
town, but with the largest variety of nationalities and complexions we
have encountered yet.  French, English, Chinese, Arabs, Africans with
wool, blacks with straight hair, East Indians, half-whites, quadroons--
and great varieties in costumes and colors.


"""

module Test1 =

    let inputText = """Lorem Ipsum is simply dummy text of the printing and typesetting industry.
Lorem Ipsum has been the Aa11aBaAA standard dfgdfgr since the 1500s,
when an unknown versions of Lorem Ipsum."""

    let passwordRegex = @".{6,12}&.*[A-Z].*&.*[a-z].*&.*[0-1].*"
    // // let input = "4."
    let matcher = Regex(passwordRegex)
    let result = matcher.FindMatchEnd(inputText)
    let a = 1



// TWAIN2 -----------------------------
let pattern2 = "Twain"
// let pattern = "[a-z]shing"
// let pattern = "Tom|Sawyer|Huckleberry|Finn"
// let pattern = "Huck[a-zA-Z]+|Saw[a-zA-Z]+"

// let result2 = System.Text.RegularExpressions.Regex(pattern2).Match(PerformanceTests.MarkTwainText)
// let pm1 = Matcher(pattern2)
// let pm2 = pm1.FindMatchEnd(PerformanceTests.MarkTwainText)

// let runs =
//     for i = 1 to 100 do
//         let pm2 = pm1.FirstMatchEndOptimized(PerformanceTests.MarkTwainText)
//         ()

let d2 = 1

// TWAIN END -------------------------------


