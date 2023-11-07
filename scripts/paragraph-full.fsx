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
let shortSample = longSample[..50000]


let view (results: MatchPosition array) (idx) =
    let lens = results[idx]
    longSample[lens.Index .. lens.Index + lens.Length]

let viewn n (results: MatchPosition array) =
    stdout.WriteLine $"Total: {results.Length}"

    results
    |> Seq.iteri (fun idx lens ->
        stdout.WriteLine longSample[lens.Index .. lens.Index + lens.Length]
    )




// with

// let pats = @"~(⊤*\n\n⊤*)&⊤*ing&occ⊤*"
// let pats = @"occ~(⊤*\n\n⊤*)ing" // 844

let ts = "⊤*"

// let pats =
//     // String.concat "&" [ @"[a-zA-Z]*"; $@".*b.*b.*"; $@".*i.*i.*"; $@".*e.*e.*"; $@"~({ts}x{ts})" ]
//     // String.concat "&" [ @"[a-zA-Z]*"; $@"{ts}b{ts}b{ts}"; $@"{ts}i{ts}i{ts}"; $@"{ts}e{ts}e{ts}"; $@"~({ts}x{ts})" ]
//     String.concat "&" [
//         // @"[a-zA-Z ]{8,20}"
//         // @"T[a-zA-Z ]{0,20}"
//         // @"[a-zA-Z ]{0,20}"
//         @".*"
//         $@"{ts}nn{ts}"
//         $@"{ts}[Ii]{ts}[Ii]{ts}"
//         $@"{ts}[Ee]{ts}[Ee]{ts}"
//         $@"{ts}ee{ts}"
//         $@"{ts}F{ts}F{ts}"
//         $@"~({ts}\n\n{ts})"
//     ]

let conj_line(words: string list) =
    words
    |> List.map (fun v -> $"{ts}{v}{ts}")
    |> String.concat "&"
    |> (fun v -> v + "&.*")


let pattern = conj_line [ "have"; "there"; "other" ] // "nature" "referred";


let pats =
    String.concat "&" [
        $@"whispered~({ts}\n\n{ts})without~({ts}\n\n{ts})invention" 
    ]   

let res =
    pats
    |> Matcher
    |> (fun v -> v.MatchPositions(longSample))
    |> Seq.toArray
    |> viewn 0

// let res =
//     // |> Sbre.Benchmarks.Jobs.Permutations.permuteConjInParagraph
//     // pattern
//     pats
//     |> Matcher
//     |> (fun v -> v.MatchPositions(longSample))
//     |> Seq.toArray
//     |> viewn 10




fsi.PrintWidth <- 150

// let test2323 =
//     // [ "[a-zA-Z]*b[a-zA-Z]*b"; "[a-zA-Z]*i[a-zA-Z]*i"; "[a-zA-Z]*e[a-zA-Z]*e" ]
//     // [ ".*b.*b"; ".*i.*i"; ".*e.*e"; ".*F.*F" ]
//     [ ".*b.*b"; ".*i.*i"; ".*e.*e"; ".*F.*F" ]
//     |> List.map (fun v -> $"(?={v})")
//     |> String.concat ""
//     |> (fun v -> v + "(?!.*x)" + $".*")



let permuteAltInLine(words: string list) =
    let rec distribute e =
        function
        | [] -> [ [ e ] ]
        | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

    let rec permute =
        function
        | [] -> [ [] ]
        | e :: xs -> List.collect (distribute e) (permute xs)

    let altpermutations =
        String.concat "|" [
            for permutation in permute words do
                let inner = (String.concat @".*" permutation)
                yield $".*{inner}.*"
        ]

    $"{altpermutations}"

let asdasds =
    Sbre.Benchmarks.Jobs.Permutations.permuteAltInParagraph [ "a"; "b"; "c"; "d" ]

let test2323 =
    [ "(.*[Ee]){2}"; "(.*[Ii]){2}"; @".*F.*F" ]
    |> List.map (fun v -> $"(?={v})")
    |> String.concat ""
    |> (fun v -> v + "(?!.*x)" + $".*")

let asdasds2 = permuteAltInLine [ "have"; "there"; "other" ]

File.writeTo "test.txt" asdasds2


let matches =
    let reg =
        System.Text.RegularExpressions.Regex(
            asdasds2,
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    reg.Matches(longSample) |> Seq.map (fun v -> v.Value) |> Seq.toArray


matches.Length // 16
// reg.Matches(shortSample) |> Seq.map (fun v -> v.Value) |> Seq.toArray


let twostepSearch words =
    let opts = System.Text.RegularExpressions.RegexOptions.None

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
            results.Add({ Index = e.Current.Index; Length = e.Current.Length - 1 })

    results

let test2 =
    // twostepSearch ["(?i)Tom|Sawyer|Huckleberry|Finn";"[a-z]+shing"; ""] |> Seq.toArray |> viewn 3
    // twostepSearch [@"\s[a-zA-Z]{0,12}ing\s";]
    [
        @"\s(?=[a-zA-Z]*a)(?=[a-zA-Z]*b)(?=[a-zA-Z]*c)(?=[a-zA-Z]*d)[a-zA-Z]{0,12}ing"
    ]
    |> twostepSearch
    |> Seq.toArray
    |> viewn 3



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
