#I "../src/Sbre/bin/Release/net7.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
#r "nuget: FSharp.Data"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Text.RuntimeRegexCopy
open System.Globalization
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ 


let sampleParagraph = """
Well, there wasn't any way now to settle which was the greatest traveler;
some said it was Nat, some said it was Tom. Everybody allowed that Nat
had seen the most longitude, but they had to give in that whatever Tom
was short in longitude he had made up in latitude and climate. It was
about a stand-off; so both of them had to whoop up their dangerous
adventures, and try to get ahead THAT way. That bullet-wound in Tom's leg
was a tough thing for Nat Parsons to buck against, but he bucked the best
he could; and at a disadvantage, too, for Tom didn't set still as he'd
orter done, to be fair, but always got up and sauntered around and worked
his limp while Nat was painting up the adventure that HE had in
Washington; for Tom never let go that limp when his leg got well, but
practiced it nights at home, and kept it good as new right along.
"""

let sampleParagraph2 = """
We were pleasantly situated in a small two-storied inn, in an empty large
compound which was surrounded by a mud wall as high as a man's head.  The
inn was kept by nine Hindoo brothers, its owners.  They lived, with their
families, in a one-storied building within the compound, but off to one
side, and there was always a long pile of their little comely brown
children loosely stacked in its veranda, and a detachment of the parents
wedged among them, smoking the hookah or the howdah, or whatever they
call it.  By the veranda stood a palm, and a monkey lived in it, and led
a lonesome life, and always looked sad and weary, and the crows bothered
him a good deal.
"""






let permuteSimple (words: string list) =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                yield (String.concat @"[\s\S]*" permutation)
        ]
    $"{altpermutations}"


let words = ["Tom"; "longitude"; "tough"]

let test_single_regex = 
    let reg = Regex(permuteSimple words)
    reg.IsMatch(sampleParagraph)
    
let test_multiple_regex = 
    let regexes = [|
        for word in words do 
            Regex(word)
    |]
    regexes
    |> Array.forall (fun regex -> 
        regex.IsMatch(sampleParagraph)
    )
    
