[<Xunit.Collection("Sequential")>]
module Sbre.Test.BenchmarkTests

open System
open System.IO
open System.Text.RegularExpressions
open Sbre
open Sbre.Benchmarks.Jobs
open Xunit



let paragraph = """

Joan answered in level, passionless tones:

"One may describe it without hunting far for words. I knew of this
poor comedy, my lord, although it was not intended that I should
know. It is to the credit of the devisers of it that they tried to
conceal it--this comedy whose text and impulse are describable in
two words."

The Chancellor spoke up with a fine irony in his manner:

"Indeed? And will your Excellency be good enough to utter them?"

"Cowardice and treachery!"

The fists of all the generals came down this time, and again the
King's eye sparkled with pleasure. The Chancellor sprang to his
feet and appealed to his Majesty:

"Sire, I claim your protection."

But the King waved him to his seat again, saying:

"Peace. She had a right to be consulted before that thing was
undertaken, since it concerned war as well as politics. It is but just
that she be heard upon it now."

The Chancellor sat down trembling with indignation, and
remarked to Joan:

"Out of charity I will consider that you did not know who devised
this measure which you condemn in so candid language."

"Save your charity for another occasion, my lord," said Joan, as
calmly as before. "Whenever anything is done to injure the
interests and degrade the honor of France, all but the dead know
how to name the two conspirators-in-chief--"

"Sir, sire! this insinuation--"

"It is not an insinuation, my lord," said Joan, placidly, "it is a
charge. I bring it against the King's chief minister and his
Chancellor."

Both men were on their feet now, insisting that the King modify
Joan's frankness; but he was not minded to do it. His ordinary
councils were stale water--his spirit was drinking wine, now, and
the taste of it was good. He said:

It was delivered into the hands of a courier, and he galloped away
with it. The Joan dismissed me, and told me to go to the inn and
stay, and in the morning give to her father the parcel which she
had left there. It contained presents for the Domremy relatives and
friends and a peasant dress which she had bought for herself. She
said she would say good-by to her father and uncle in the morning
if it should still be their purpose to go, instead of tarrying awhile to
see the city.

I didn't say anything, of course, but I could have said that wild
horses couldn't keep those men in that town half a day. They waste
the glory of being the first to carry the great news to
Domremy--the taxes remitted forever!--and hear the bells clang
and clatter, and the people cheer and shout? Oh, not they. Patay
and Orleans and the Coronation were events which in a vague way
these men understood to be colossal; but they were colossal mists,
films, abstractions; this was a gigantic reality!

"""

let permuteWithLookaround (words: string list) =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    let prefix = @"\n\n((?!\n\n)[\s\S])*?"
    let suffix = @"((?!\n\n)[\s\S])*?\n\n"
    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                yield (String.concat @"((?!\n\n)[\s\S])*?" permutation)
        ]
    $"{prefix}({altpermutations}){suffix}"


let permuteWithLoop (words: string list) =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    let prefix = @"\n\n((.+\n)+?"
    let suffix = @"(.+\n)+?)\n"
    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                yield
                    permutation
                    |> List.map (fun v -> $".*{v}.*")
                    |> String.concat @"(.+\n)+?"
        ]
    $"{prefix}({altpermutations}){suffix}"


let permuteAlt (words: string list) =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    let prefix = @"[\s\S]*"
    let suffix = @"[\s\S]*"
    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                yield (String.concat @"[\s\S]*" permutation)
        ]
    $"{prefix}({altpermutations}){suffix}"

let testBacktracking (pattern:string) =
    System.Text.RegularExpressions.Regex(pattern).Match(paragraph)

let testNonBacktracking (pattern:string) =
    System.Text.RegularExpressions.Regex(pattern,RegexOptions.NonBacktracking).Match(paragraph)

let testSbre (pattern:string) =
    Matcher(pattern).MatchText(paragraph)


let words = ["King";"Paris";"English";"would";"rise";"struck"; "council"; "march"; "war"; "May"; "Orleans"; "work"]





let twainPatterns = [
    @"Twain"
    @"(?i)Twain"
    @"[a-z]shing"
    @"Huck[a-zA-Z]+|Saw[a-zA-Z]+"
    @"\b\w+nn\b"
    @"[a-q][^u-z]{13}x"
    @"Tom|Sawyer|Huckleberry|Finn"
    @"(?i)Tom|Sawyer|Huckleberry|Finn"
    @".{0,2}(Tom|Sawyer|Huckleberry|Finn)"
    @".{2,4}(Tom|Sawyer|Huckleberry|Finn)"
    @"Tom.{10,25}river|river.{10,25}Tom"
    @"[a-zA-Z]+ing"
    @"\s[a-zA-Z]{0,12}ing\s"
    @"([A-Za-z]awyer|[A-Za-z]inn)\s"
    @"[""'][^""']{0,30}[?!\.][""']"
]


let twain_input =
    File.ReadAllText(__SOURCE_DIRECTORY__ + "/data/input-text.txt")


let twain_20k = twain_input[..19999] // 10k chars limit

//
[<Fact>]
let ``loop subsumption test``() =
    let m = Matcher(Permutations.permuteConjInParagraph [ @"[a-zA-Z]{0,12}ing\s";])
    let r = m.Matches(paragraph) |> Seq.toArray
    Assert.Equal(r.Length,9)


[<Fact>]
let ``reversal test``() =
    let m = Matcher(Permutations.permuteConjInParagraph [  @"[a-z]*a[a-z]*"])
    let r = m.Matches(paragraph[..50]) |> Seq.toArray
    Assert.Equal(1, r.Length)

//
// [<Fact>]
// let ``bench test 1``() =
//     let m = Matcher(twainPatterns[1])
//     let r = m.MatchPositions(twain_input) |> Seq.toArray
//
//     Assert.Equal(r.Length,965)

