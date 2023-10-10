[<Xunit.Collection("Sequential")>]
module Sbre.Test.BenchmarkTests

open System
open System.Text.RegularExpressions
open Sbre
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

"Sit--and be patient. What is fair for one must in fairness be
allowed the other. Consider--and be just. When have you two
spared her? What dark charges and harsh names have you withheld
when you spoke of her?" Then he added, with a veiled twinkle in
his eyes, "If these are offenses I see no particular difference
between them, except that she says her hard things to your faces,
whereas you say yours behind her back."

He was pleased with that neat shot and the way it shriveled those
two people up, and made La Hire laugh out loud and the other
generals softly quake and chuckle. Joan tranquilly resumed:

"From the first, we have been hindered by this policy of
shilly-hally; this fashion of counseling and counseling and
counseling where no counseling is needed, but only fighting. We
took Orleans on the 8th of May, and could have cleared the region
round about in three days and saved the slaughter of Patay. We
could have been in Rheims six weeks ago, and in Paris now; and
would see the last Englishman pass out of France in half a year.
But we struck no blow after Orleans, but went off into the
country--what for? Ostensibly to hold councils; really to give
Bedford time to send reinforcements to Talbot--which he did; and
Patay had to be fought. After Patay, more counseling, more waste
of precious time. Oh, my King, I would that you would be
persuaded!" She began to warm up, now. "Once more we have our
opportunity. If we rise and strike, all is well. Bid me march upon
Paris. In twenty days it shall be yours, and in six months all
France! Here is half a year's work before us; if this chance be
wasted, I give you twenty years to do it in. Speak the word, O
gentle King--speak but the one--"

"I cry you mercy!" interrupted the Chancellor, who saw a
dangerous enthusiasm rising in the King's face. "March upon
Paris? Does your Excellency forget that the way bristles with
English strongholds?"

"That for your English strongholds!" and Joan snapped her fingers
scornfully. "Whence have we marched in these last days? From
Gien. And whither? To Rheims. What bristled between? English
strongholds. What are they now? French ones--and they never cost
a blow!" Here applause broke out from the group of generals, and
Joan had to pause a moment to let it subside. "Yes, English
strongholds bristled before us; now French ones bristle behind us.
What is the argument? A child can read it. The strongholds
between us and Paris are garrisoned by no new breed of English,
but by the same breed as those others--with the same fears, the
same questionings, the same weaknesses, the same disposition to
see the heavy hand of God descending upon them. We have but to
march!--on the instant--and they are ours, Paris is ours, France is
ours! Give the word, O my King, command your servant to--"

"Stay!" cried the Chancellor. "It would be madness to put our
affront upon his Highness the Duke of Burgundy. By the treaty
which we have every hope to make with him--"

"Oh, the treaty which we hope to make with him! He has scorned
you for years, and defied you. Is it your subtle persuasions that
have softened his manners and beguiled him to listen to proposals?
No; it was blows!--the blows which we gave him! That is the only
teaching that that sturdy rebel can understand. What does he care
for wind? The treaty which we hope to make with him--alack! He
deliver Paris! There is no pauper in the land that is less able to do
it. He deliver Paris! Ah, but that would make great Bedford smile!
Oh, the pitiful pretext! the blind can see that this thin pour-parler
with its fifteen-day truce has no purpose but to give Bedford time
to hurry forward his forces against us. More treachery--always
treachery! We call a council of war--with nothing to council about;
but Bedford calls no council to teach him what our course is. He
knows what he would do in our place. He would hang his traitors
and march upon Paris! O gentle King, rouse! The way is open,
Paris beckons, France implores, Speak and we--"

"Sire, it is madness, sheer madness! Your Excellency, we cannot,
we must not go back from what we have done; we have proposed
to treat, we must treat with the Duke of Burgundy."

"And we will!" said Joan.

"Ah? How?"

"At the point of the lance!"

The house rose, to a man--all that had French hearts--and let go a
crach of applause--and kept it up; and in the midst of it one heard
La Hire growl out: "At the point of the lance! By God, that is
music!" The King was up, too, and drew his sword, and took it by
the blade and strode to Joan and delivered the hilt of it into her
hand, saying:

"There, the King surrenders. Carry it to Paris."

And so the applause burst out again, and the historical co9uncil of
war that has bred so many legends was over.

Chapte 39 We Win, but the King Balks

IT WAS away past midnight, and had been a tremendous day in
the matter of excitement and fatigue, but that was no matter to
Joan when there was business on hand. She did not think of bed.
The generals followed her to her official quarters, and she
delivered her orders to them as fast as she could talk, and they sent
them off to their different commands as fast as delivered;
wherefore the messengers galloping hither and thither raised a
world of clatter and racket in the still streets; and soon were added
to this the music of distant bugles and the roll of drums--notes of
preparation; for the vanguard would break camp at dawn.

The generals were soon dismissed, but I wasn't; nor Joan; for it
was my turn to work, now. Joan walked the floor and dictated a
summons to the Duke of Burgundy to lay down his arms and make
peace and exchange pardons with the King; or, if he must fight, go
fight the Saracens. "Pardonnez-vous l'un � l'autre de bon
c&oelig;ur, enti�rement, ainsi que doivent faire loyaux chr�tiens,
et, s'il vous plait de guerroyer, allez contre les Sarrasins." It was
long, but it was good, and had the sterling ring to it. It is my
opinion that it was as fine and simple and straightforward and
eloquent a state paper as she ever uttered.

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


// enlistment
// military
// traitor
// war
// soldier
// deserter
// army

let permuteConj (words: string list) =
    let prefix = @"\n\n~(⊤*\n\n⊤*)\n"
    let permutations =
        String.concat "&" [
            for word in words do
                yield $"⊤*{word}⊤*"
        ]
    $"{prefix}&{permutations}"

let words = ["King";"Paris";"English";"would";"rise";"struck"; "council"; "march"; "war"; "May"; "Orleans"; "work"]




[<Fact>]
let ``para 3``() =
    let pat_conj_neg = permuteConj words[..2]
    let r_SBRE = Matcher(pat_conj_neg)
    let res = r_SBRE.MatchText(paragraph)
    Assert.Equal(res, Some """

"From the first, we have been hindered by this policy of
shilly-hally; this fashion of counseling and counseling and
counseling where no counseling is needed, but only fighting. We
took Orleans on the 8th of May, and could have cleared the region
round about in three days and saved the slaughter of Patay. We
could have been in Rheims six weeks ago, and in Paris now; and
would see the last Englishman pass out of France in half a year.
But we struck no blow after Orleans, but went off into the
country--what for? Ostensibly to hold councils; really to give
Bedford time to send reinforcements to Talbot--which he did; and
Patay had to be fought. After Patay, more counseling, more waste
of precious time. Oh, my King, I would that you would be
persuaded!" She began to warm up, now. "Once more we have our
opportunity. If we rise and strike, all is well. Bid me march upon
Paris. In twenty days it shall be yours, and in six months all
France! Here is half a year's work before us; if this chance be
wasted, I give you twenty years to do it in. Speak the word, O
gentle King--speak but the one--"

""" )

    ()



// [<Fact>]
// let ``raw match 2``() =
//     do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
//     let pat_alt = permuteAlt [ "enlistment"; "military" ]
//     let pat_conj = @"⊤*enlistment⊤*&⊤*military⊤*"
//
//     let r1 = testBacktracking(pat_alt)
//     let r2 = testNonBacktracking(pat_alt)
//     let r3 = testSbre(pat_conj)
//
//     let lengths =
//         [
//             r1.Length
//             r2.Length
//             r3.Value.Length
//         ]
//     lengths
//     |> Seq.reduce (fun l r ->
//         if l <> r then failwith "invalid lengths"
//         else l
//     )


// [<Fact>]
// let ``conjunctions test paragraph 2``() =
//     do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)
//     let pat_lookaround = permuteWithLookaround [ "enlistment"; "military" ]
//     let pat_loop = permuteWithLoop [ "enlistment"; "military" ]
//     let pat_conj_neg = @"\n\n~(⊤*\n\n⊤*)\n&⊤*enlistment⊤*&⊤*military⊤*"
//
//     let r1 = testBacktracking(pat_lookaround)
//     let r2 = testNonBacktracking(pat_loop)
//     let r3 = testSbre(pat_conj_neg)
//
//     let lengths =
//         [
//             r1.Length
//             r2.Length
//             r3.Value.Length
//         ]
//     lengths
//     |> Seq.reduce (fun l r ->
//         if l <> r then failwith "invalid lengths"
//         else l
//     )



