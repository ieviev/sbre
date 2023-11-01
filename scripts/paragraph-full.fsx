#I "../src/Sbre.Benchmarks/bin/Release/net7.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
#r "Sbre.Benchmarks.dll"
#r "nuget: FSharp.Data"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Text.RuntimeRegexCopy
open System.Globalization
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let longSample = __SOURCE_DIRECTORY__ + "/input-text.txt" |> System.IO.File.ReadAllText
let shortSample = longSample[..10000]


let r1s = 
    Sbre.Benchmarks.Jobs.Permutations.permuteConjInParagraph [ "Huck"; "from"; "you"; ]


// "\n\n~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*"

let inputText = shortSample

// type Combined1() =
//     inherit
//         Sbre.Benchmarks.Jobs.RuntimeCombinedSearch(
//             [ "compilation"; "smaller" ],
//             shortSample,
//             RegexOptions.None
//         )

// let r = Combined1()
// r.Setup
// let res =
//     r.MultipleIsMatches() // [|struct (834, 353)|]


let view (results: MatchPosition array) (idx) =
    let lens = results[idx]
    longSample[lens.Index .. lens.Index + lens.Length]


// type FullSbre() =
//     inherit Sbre.Benchmarks.Jobs.SbreCombinedSearch(patterns, longSample)

let pat1 = @"\n\n~(⊤*\n\n⊤*)\n&⊤*Twain⊤*"


let res_1 =
    Matcher(@"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*";).MatchPositions(longSample) |> Seq.toArray





// let pat2=  permuteWithLoop [ "you"; "Huck"; "from"]

let results =
    let pat = Sbre.Benchmarks.Jobs.Permutations.permuteConjInParagraph [ "you"; "Huck"; "from" ]
    Matcher(pat).MatchPositions(longSample) |> Seq.toArray

results.Length

let _ =
    let i = 0
    [ i .. i + 4 ] |> List.iter (fun v -> view results v |> stdout.WriteLine)



// --

let twainPgs = Matcher(pat1).MatchPositions(shortSample) |> Seq.toArray

let t2 =
    Matcher(@"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*";).MatchPositions(shortSample)
    |> Seq.toArray







// DEBUG
// let es =
//     let mutable e = r.ParagraphSearchRegex.EnumerateMatches(inputText)
//     while e.MoveNext() do
//         let currentString = inputText[e.Current.Index .. e.Current.Index + e.Current.Length]
//         let currRange = $"{e.Current.Index}-{e.Current.Index + e.Current.Length}"
//         stdout.WriteLine $":: {currRange}"
//         match
//             r.MultipleIsMatchRegexes |> Array.forall (fun regex -> regex.IsMatch(currentString))
//         with
//         | true -> stdout.WriteLine $"IS MATCH: {currRange}\n{currentString}"
//         | false -> ()




// RUNTIME

// SAME!
// type NonBacktracking_Full() =
//     inherit
//         Sbre.Benchmarks.Jobs.RuntimeCombinedSearch(
//             [ "you"; "Huck"; "from" ],
//             longSample,
//             RegexOptions.None
//         )

// let rt = NonBacktracking_Full()

// rt.Setup()


// let results2 = 
//     rt.MultipleIsMatches()
//     |> Seq.toArray

// results2.Length




// let permuteWithLoop (words: string list) =
//     let rec distribute e = function
//       | [] -> [[e]]
//       | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
//     let rec permute = function
//       | [] -> [[]]
//       | e::xs -> List.collect (distribute e) (permute xs)
//     let prefix = @"(?:(?:.+\n)+?"
//     let suffix = @"(?:.+\n)+?)\n"
//     let altpermutations =
//         String.concat "|" [
//             for permutation in permute words do
//                 yield
//                     permutation
//                     |> List.map (fun v -> $".*{v}.*")
//                     |> String.concat @"(?:.+\n)+?"
//         ]
//     $"{prefix}(?:{altpermutations}){suffix}"

#r "nuget: Fs.Scripting"
open Fs.Scripting



let str12 =
    let i = 4059049
    longSample[i - 200.. i + 500]




let permuteWithLoop (words: string list) =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    let prefix = @"(?:.+\n)*?" // standard line loop
    let suffix = @"(?:.+\n)*?\n" // wrong
    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                yield
                    permutation
                    |> List.map (fun v -> $".*{v}.*")
                    |> String.concat @"(?:.+\n)*?"
        ]
    $"{prefix}(?:{altpermutations}){suffix}"

// let single =  permuteWithLoop [ "and"; ]
// let single =  permuteWithLoop [ "and"; "may" ]
let single =  permuteWithLoop [ "you"; "Huck"; "from"]
// let single =  Permutations. [ "Huck"; "from"; "you"; ]
// let single =  permuteWithLoop [ "you"; "Huck"; ]

// Os.copyToClipboard str12
Os.copyToClipboard single


let results3 = 
    System.Text.RegularExpressions.Regex(single, RegexOptions.NonBacktracking)
        .Matches(longSample)
        |> Seq.map (fun v -> 
            (v.Index,v.Length)
        )
        |> Seq.toArray
        

results3.Length
