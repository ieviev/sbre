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


let view (results: MatchPosition array) (idx) =
    let lens = results[idx]
    longSample[lens.Index .. lens.Index + lens.Length]

let viewn n (results: MatchPosition array) =
    stdout.WriteLine $"Total: {results.Length}"
    for i = 0 to n do
        let lens = results[i]
        stdout.WriteLine longSample[lens.Index .. lens.Index + lens.Length]



let inputText = shortSample

// with

// Length:1812
let res =
    // ["Huck"; "Finn"; "from"; "Saw[a-z]+"; "[a-q][^u-z]{13}x" ]
    // ["(?:Tom|Sawyer|Huckleberry|Finn)"; "Twain" ]
    // [ "Huck"; ] // c: 411
    // [ @"H[a-z]*berry\s+F[a-z]*\s+(was)"; ] // c: 64
    // 12 ok

    // [ "(?:Tom|Sawyer|Huckleberry|Finn)" ] // c: 1812
    // [ "(?:Tom|Sawyer)"; "(?:Huckleberry|Finn)" ] // c: 32
    // [ "(?:Tom|Sawyer)"; "(?:Huckleberry|Finn)"; "from" ] // c: 14
    // [ "(?:Tom|Sawyer|before)"; "(?:Huckleberry|Finn|legs)"; @"old[\s\S]*thing" ] // c: 13
    // [  @"(?i)[a-z]{0,12}ing to the (?:d[a-z]+)" ] // c: 19
    // [ @"\sthe\s"; @"\sand\s"; @"\sof\s"; @"\sthat\s"; @"\sHuck\s" ] // c: 11
    [ @"\s([a-zA-Z]{0,12}ing&⊤*b⊤*&⊤*r⊤*&⊤*e⊤*)" ] // c: 11
    // [  @"\s([a-z]*a[a-z]*&[a-z]*b[a-z]*&[a-z]*c[a-z]*&[a-z]*d[a-z]*)\s" ] // c: 11
    |> Sbre.Benchmarks.Jobs.Permutations.permuteConjInParagraph
    |> Matcher
    |> (fun v -> v.MatchPositions(longSample))
    |> Seq.toArray
    |> viewn 3




let twostepSearch words =
    let opts  = System.Text.RegularExpressions.RegexOptions.None
    let regexes = [|
        for word in words do
            yield
                System.Text.RegularExpressions.Regex(
                    word,
                    options = opts,
                    matchTimeout = TimeSpan.FromMilliseconds(10_000.)
                )
    |]
    let results = ResizeArray()
    let inputSpan = longSample.AsSpan()
    let paragraphRegex = System.Text.RegularExpressions.Regex(@"(?:.+\n)+\n", opts)

    let mutable entireParagraphIsMatch = true
    let mutable e = paragraphRegex.EnumerateMatches(longSample)

    // enumerate paragraphs during match
    while e.MoveNext() do
        entireParagraphIsMatch <- true

        let paragraphSpan = inputSpan.Slice(e.Current.Index, e.Current.Length)
        // run multiple ismatch regexes on each paragraph
        for reg in regexes do
            if not (reg.IsMatch(paragraphSpan)) then
                entireParagraphIsMatch <- false

        if entireParagraphIsMatch then
            results.Add({Index=e.Current.Index; Length= e.Current.Length - 1})

    results

let test2 = 
    // twostepSearch ["(?i)Tom|Sawyer|Huckleberry|Finn";"[a-z]+shing"; ""] |> Seq.toArray |> viewn 3
    // twostepSearch [@"\s[a-zA-Z]{0,12}ing\s";] 
    [@"\s(?=[a-zA-Z]*a)(?=[a-zA-Z]*b)(?=[a-zA-Z]*c)(?=[a-zA-Z]*d)[a-zA-Z]{0,12}ing"]
    |> twostepSearch
    |> Seq.toArray |> viewn 3



type DebugRuntime() =
    inherit
        Sbre.Benchmarks.Jobs.RuntimeFullSearch(
            // [  @"(?i)[a-z]{2,12}ing (?:d[a-z]+)\s" ], // 1s
            // [  @"(?i)[a-z]{2,12}ing to the (?:d[a-z]+)\s" ], // 1s
            // [  @"(?i)[a-z]{2,12}ing to the (?:d[a-z]+)\s" ], // 1s
            // [  @"Jim[\s\S]*had[\s\S]*been" ], // 1s
            // [  @"Jim[\s\S]*had[\s\S]*been[\s\S]*[a-z]*ing"  ], // 1s
            // [  @"(?:(?i)[a-z]{0,12}ing to the (?:d[a-z]{0,12})\s)" ],
            [ @"(a\S*)" ],
            // [  @"(?:(?i)[a-z]{0,12}ing to the (?:d[a-z]{0,12})\s)"; "Huck" ],
            longSample,
            // RegexOptions.None
            RegexOptions.Multiline
        )

let v = DebugRuntime()
v.Setup()
let rs = v.TwoStepSearch() |> Seq.length



let r2 =
    let pat =
        Sbre.Benchmarks.Jobs.Permutations.permuteConjInParagraph [ "[a-z]shing"; "from"; "you" ]

    let m = Matcher(pat)
    m.MatchPositions(longSample) |> Seq.toArray


r2 |> viewn 3

r2.Length

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



// type FullSbre() =
//     inherit Sbre.Benchmarks.Jobs.SbreCombinedSearch(patterns, longSample)

let pat1 = @"\n\n~(⊤*\n\n⊤*)\n&⊤*Twain⊤*"


let res_1 =
    Matcher(@"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*").MatchPositions(longSample)
    |> Seq.toArray





// let pat2=  permuteWithLoop [ "you"; "Huck"; "from"]

let results =
    let pat =
        Sbre.Benchmarks.Jobs.Permutations.permuteConjInParagraph [ "you"; "Huck"; "from" ]

    Matcher(pat).MatchPositions(longSample) |> Seq.toArray

results.Length

let _ =
    let i = 0
    [ i .. i + 4 ] |> List.iter (fun v -> view results v |> stdout.WriteLine)



// --

let twainPgs = Matcher(pat1).MatchPositions(shortSample) |> Seq.toArray

let t2 =
    Matcher(@"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*&⊤*from⊤*&⊤*you⊤*").MatchPositions(shortSample)
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
    longSample[i - 200 .. i + 500]




let permuteWithLoop(words: string list) =
    let rec distribute e =
        function
        | [] -> [ [ e ] ]
        | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

    let rec permute =
        function
        | [] -> [ [] ]
        | e :: xs -> List.collect (distribute e) (permute xs)

    let prefix = @"(?:.+\n)*?" // standard line loop
    let suffix = @"(?:.+\n)*?\n" // wrong

    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                yield permutation |> List.map (fun v -> $".*{v}.*") |> String.concat @"(?:.+\n)*?"
        ]

    $"{prefix}(?:{altpermutations}){suffix}"

// let single =  permuteWithLoop [ "and"; ]
// let single =  permuteWithLoop [ "and"; "may" ]
let single = permuteWithLoop [ "you"; "Huck"; "from" ]
// let single =  Permutations. [ "Huck"; "from"; "you"; ]
// let single =  permuteWithLoop [ "you"; "Huck"; ]

// Os.copyToClipboard str12
Os.copyToClipboard single


let results3 =
    System.Text.RegularExpressions
        .Regex(single, RegexOptions.NonBacktracking)
        .Matches(longSample)
    |> Seq.map (fun v -> (v.Index, v.Length))
    |> Seq.toArray


results3.Length
