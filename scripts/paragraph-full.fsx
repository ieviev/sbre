#I "../src/Sbre.Benchmarks/bin/Release/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
#r "Sbre.Benchmarks.dll"
#r "nuget: FSharp.Data"

open System
open System.Threading
open FSharp.Data
open System.Text.RuntimeRegexCopy
open System.Globalization
open System.Text.RegularExpressions
open Sbre
open Sbre.Benchmarks.Jobs

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__



// let rebar_input =
//     "/mnt/g/repos/rebar/benchmarks/haystacks/opensubtitles/en-sampled.txt"
//     |> File.readAllText

// let results =
//     Sbre.Regex("((?i)Sherlock Holmes)").Count(rebar_input)


let longSample = __SOURCE_DIRECTORY__ + "/input-text.txt" |> System.IO.File.ReadAllText
let shortSample = longSample[..50000]


let viewn n (results: MatchPosition array) =
    stdout.WriteLine $"Total: {results.Length}"

    results
    |> Seq.truncate n
    |> Seq.iteri (fun idx lens ->
        stdout.WriteLine longSample[lens.Index .. lens.Index + lens.Length]
    )


let fast1 =

    // @".*Huck.*&~(.*the.*)&.*" // 283
    // @".*Huck.*&.*Finn.*&~(.*the.*)&.*" // 283
    // @"[a-z][a-z]shing" // 283
    @"[a-z][a-z]sh(?=ing)" // 283
    |> Sbre.Regex
    // |> (fun v -> v.CountMatches(longSample))
    // |> (fun v -> v.MatchPositions(shortSample))
    |> (fun v -> v.MatchPositions(longSample[..5000_000]))
    |> Seq.toArray
    |> viewn 10





let pgConj words =
    Sbre.Benchmarks.Jobs.Permutations.permuteConjInParagraph words

let lineConj words =
    Sbre.Benchmarks.Jobs.Permutations.permuteConjInLine words

let pgConjICase words =
    Sbre.Benchmarks.Jobs.Permutations.permuteConjInParagraph words
    |> (fun v -> $"(?:(?i){v})")

let allRegexes =
    [
        // standard benchmark from 2019
        "Twain"
        "(?i)Twain"
        @"[a-z]shing"
        @"Huck[a-zA-Z]+|Saw[a-zA-Z]+"
        @"[a-q][^u-z]{13}x"
        @"Tom|Sawyer|Huckleberry|Finn"
        // ^ --- OK


        //
        pgConj [ "Huck" ]
        pgConj [ "Huck"; "Finn" ]
        pgConj [ "Huck"; "Finn"; "Tom" ]
        pgConj [ "Huck"; "Finn"; "Tom"; "Sawyer" ]

        pgConjICase [ "Huck" ]
        pgConjICase [ "Huck"; "Finn" ]
        pgConjICase [ "Huck"; "Finn"; "Tom" ]
        pgConjICase [ "Huck"; "Finn"; "Tom"; "Sawyer" ]
        pgConjICase [ "Huck"; "Finn"; "Tom"; "Sawyer" ]

        pgConj [ "H[a-z]*k" ]
        pgConj [ "H[a-z]*k"; "F[a-z]*n" ]
        pgConj [ "H[a-z]*k"; "F[a-z]*n"; "T[a-z]*m" ]
        pgConj [ "H[a-z]*k"; "F[a-z]*n"; "T[a-z]*m"; "S[a-z]*r" ]
    ]
    |> File.writeLinesTo "regexes.txt"






// with
// let pats = @"~(⊤*\n\n⊤*)&⊤*ing&occ⊤*"
// let pats = @"occ~(⊤*\n\n⊤*)ing" // 844

// let pat= "⊤*have⊤*&⊤*there⊤*&⊤*other⊤*&.*"
// let pat= ".*city.*&.*town.*" // 15
// let pat= ".*thing.*&.*great.*" // 133
// let pat= ".*thing.*&.*again.*" // 3k, 187
// let pat= ".*ever.*&.*back.*" // 2,5k, 122
// let pat = @"~(⊤*\n\n⊤*)\n&⊤*Huck⊤*"


let fast =

    // @".*Huck.*&~(.*the.*)&.*" // 283
    // @".*Huck.*&.*Finn.*&~(.*the.*)&.*" // 283
    @"[a-z][a-z]shing" // 283
    |> Sbre.Regex
    // |> (fun v -> v.CountMatches(longSample))
    // |> (fun v -> v.MatchPositions(shortSample))
    |> (fun v -> v.MatchPositions(longSample[..5000_000]))
    |> Seq.toArray
    |> viewn 10

let ts = "⊤*"

let requirements =
    String.concat "&" [
        // 11
        // $@"{ts}nn{ts}"
        @".{0,16}"

        $@"{ts}[Ii]{ts}"
        $@"{ts}[Ee]{ts}"
        $@"{ts}[Ff]{ts}"
        $@"{ts}[bB]{ts}"

    // $@"{ts}[Ii]{ts}[Ii]{ts}"
    // $@"{ts}[Ee]{ts}[Ee]{ts}"
    // $@"{ts}[Ff]{ts}[Ff]{ts}"
    // $@"~({ts}\n\n{ts})"
    ]
    |> Sbre.Regex
    |> (fun v -> v.MatchPositions(shortSample))
    |> Seq.toArray
    |> viewn 4

// 6426

fsi.PrintWidth <- 150

let permutePasswordSearch(words: string list) =
    words
    |> List.map (fun v -> $"(?=.{{0,15}}{v})")
    |> String.concat ""
    |> (fun v -> v + ".{0,16}")


let pwd = permutePasswordSearch [ "[Ii]"; "[Ee]"; "[Ff]"; "[Bb]" ]

File.writeTo "test.txt" "asd"


let matches2way =
    let reg1 =
        System.Text.RegularExpressions.Regex(
            Permutations.permuteAltInLine [ "Twain" ],
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    let reg2 =
        System.Text.RegularExpressions.Regex(
            @"(?<!.*Clemens.*)(.*)(?!.*Clemens.*)",
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    reg1.Matches(shortSample)
    |> Seq.choose (fun v ->
        let res = reg2.Match(v.Value)
        if res.Success then Some res.Value else None
    )
    |> Seq.toArray



let input = "/home/ian/f/ieviev/sbre/scripts/input-text.txt" |> File.readAllText

let search pattern =
    let reg1 =
        System.Text.RegularExpressions.Regex(
            pattern,
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    reg1.Matches(input) |> Seq.toArray

let searchParagraph(words: string list) =
    let reg1 = Sbre.Regex(pgConj words)
    reg1.Matches(input) |> Seq.toArray

let searchLine(words: string list) =
    let reg1 = Sbre.Regex(lineConj words)
    reg1.MatchPositions(input) |> Seq.toArray



let count1 pattern =
    let reg1 =
        System.Text.RegularExpressions.Regex(
            pattern,
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    reg1.Count(input)

let count2 pattern =
    let reg1 = Sbre.Regex(pattern)
    reg1.Count(input)


// let lines = searchLine [ "Huck" ] |> viewn 4

// searchLine [ "Huck" ] |> viewn 4

// [ @"[a-qA-Q][^u-z]{0,13}l [a-qA-Q][^u-z]{0,13}l [a-qA-Q][^w-z]{0,13}t"]

// let test =
    // let pattern = ".*Huck.*&~(.*Finn.*)"
    // let pattern = Permutations.permuteConjInLine ["Huck"; "Finn"; "Tom"; "Sawyer"; "Usually"; ]
    // let pattern = Permutations.permuteConjInLine [ @"(?i)[a-z]{2,12}ing to the (?:d[a-z]+)\s" ]
    // let pattern = Permutations.permuteConjInLine [ @" [gw][a-z]{0,8}ing to the [a-z]{0,8}[a-z] " ]
    // let pattern = Permutations.permuteConjInLine [ @"ing to the [a-z]{0,8}[a-z] "; "Huck" ]
    // let pattern = Permutations.permuteConjInLine [ @"ing to"; "Huck" ] // 17
    // let pattern = Permutations.permuteConjInLine [ @"ing to"; "Huck" ] // 17
    // let pattern =
    //     Permutations.permuteConjInLine
    //     // Permutations.permuteAltInLine

    //     [
    //         @"[a-qA-Q][^u-z]{0,13}l [a-qA-Q][^u-z]{0,13}l [a-qA-Q][^w-z]{0,13}t"
    //         "[a-qA-Q][^u-z]{0,13}g to be an [a-qA-Q][^u-z]{0,13}n"
    //         "into"
    //         "that"
    //     // "Huck"
    //     ]
    // [ @" [gw][a-z]{0,8}ing to [a-z]{0,8}[a-z] "; "Huck"; "Finn" ]

    // Regex(pattern).MatchPositions(input) |> Seq.toArray |> viewn 10


let pattern = Permutations.permuteConjInLine [ "Huck"; "Finn"; "Tom" ]

count1 "Twain"
count2 "Twain"

count1 @"(?:(?!F).)*Huck(?:(?!F).)*"
count2 @".*Huck.*&~(.*F.*)"

// searchParagraph ["Huck"]

let results = search "Twain"

results.Length

do AppContext.SetData("REGEX_NONBACKTRACKING_MAX_AUTOMATA_SIZE", 1_000_000)


let matches =
    let reg1 =
        System.Text.RegularExpressions.Regex(
            // "Twain"
            // ".*Twain.*"
            Permutations.permuteAltInLine [
                @" [gw][a-z]{0,8}ing to [a-z]{0,8}o "
                "H[a-z]*k"
                "F[a-z]*n"
                "f[a-z]*y"
                "H[a-z]*e"
                "h[a-z]*n"
            ],
            System.Text.RegularExpressions.RegexOptions.NonBacktracking
        )

    reg1.Matches(longSample) |> Seq.map (fun v -> v.Value) |> Seq.toArray

matches.Length
matches.Length



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







// ["which";"could"] 247
// ["which";"could"; "these"] // 4
// ["which";"could"; "other"] // 11
// ["with";"they"; "that"] // 128
// ["with";"they"; "that"; "have"] // 10
// ["with";"they"; "that"; "were"] // 23
// ["the";"and"] // 61749
// ["the";"and";"was"] // 8938
// ["the";"and";"was";"that"] // 1420
// ["the";"and";"was";"for"] // 1243
// ["the";"and";"was";"with"] // 719
