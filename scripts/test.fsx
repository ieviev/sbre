#I "../src/Sbre/bin/Debug/net7.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
#r "nuget: FSharp.Data"

open System
open System.Threading
open Sbre
open FSharp.Data
open System.Text.RuntimeRegexCopy
open System.Globalization

let r2 = @".*\wb"

// let test = Sbre.Matcher("asdasd")

let sampleText = " \n  author={Capp, Bernard Stuart and Capp, Bernard},\n  y"


let pattern = // "(?<=\{).*&.*(?=\})&.*(?=[\s\S]* y)&[a-zA-Z ,]*&Capp.*&.*nard&.*Stuart.*&~(.*Berg.*)&~(app.*)&~(.*ar)&.*\p{Ll}"
    String.Join("&",[
        @"(?<=\{).*"   // { required before match on same line
        @".*(?=\})"    // } required after match on same line
        @".*(?=[\s\S]* y)" // " y" required after match anywhere in the text
        "[a-zA-Z ,]*"  // only these symbols used in match
        "Capp.*"       // must start with Capp  
        ".*nard"       // must end with with nard
        ".*Stuart.*"   // must contain Stuart
        "~(.*Berg.*)"  // must not contain Berg
        "~(app.*)"     // must not start with app
        "~(.*ar)"      // must not end with ar
        @".*\p{Ll}"    // must end with a character in the Ll unicode category 
    ])

let regex = Sbre.Regex(pattern)

let res = regex.Match(sampleText)


let s = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/textsample.txt")

// experimental `&` and `~` parser in Matcher.fs
let test1a = Matcher("c...&...s").MatchText("raining cats and dogs") // Some "cats"
let test1b = Matcher(".*rain.*&.*dogs.*").MatchText("raining cats and dogs") // Some "raining cats and dogs"
let test1c = Matcher("and.*&.*dogs.*").MatchText("raining cats and dogs") // Some "and dogs"

// .{0,5} is the maximum distance between 2 constraints
let test2a = Matcher(".{0,5}&(?<=cats).*&.*(?=dogs)").MatchText("raining cats and dogs") // Some " and "
let test2b = Matcher(".{0,4}&(?<=cats).*&.*(?=dogs)").MatchText("raining cats and dogs") // None
let test2c = Matcher(".{0,4}&(?<=cats).*&.*(?=dogs)").MatchText("cats-dogs") // Some "-"

/// find password
let test3a =
    Matcher(
        [
            ".{10,16}" // length 10-16
            ".*[A-Z].*" // uppercase
            ".*[a-z].*" // lowercase
            ".*[$%!}?].*" // special char
            @"(?<=\b)\S*(?=\b)" // between boundaries
        ]
        |> String.concat "&"
    )
        .MatchText(
            """
Lorem Ipsum is simply dummy text of the printing and typesetting industry.
Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
when an unknown printer took a galley of 4t=!R}F39?.$tnh% type and scrambled it to make a type specimen book. <== hidden password on this line
It has survived not only five centuries, but also the leap into electronic typesetting,
remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets
containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker i
ncluding versions of Lorem Ipsum.

    """
        ) // Some "4t=!R}F39?.$tnh"


// A B C in any order between parentheses, without & every disjunction is dependent on each other
let test4a =

    let matcher =
        System.Text.RegularExpressions.Regex(
            [
                @"\([^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*B[^ABC]*A[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*B[^ABC]*C[^ABC]*A[^ABC]*\)"
                @"\([^ABC]*C[^ABC]*A[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*C[^ABC]*B[^ABC]*A[^ABC]*\)"
            ]
            |> String.concat "|"
        )

    [
        matcher.Match("(A----B----C)").Value
        matcher.Match("(B____A____C)").Value
        matcher.Match("(C    B    A)").Value
    ]

// A B C in any order between parentheses, with & conjunctions can be composed independently
let test4b =
    // A B C in any order between parentheses
    let matcher =
        Matcher([ @"\([^A]*A[^A]*\)"; @"\([^B]*B[^B]*\)"; @"\([^C]*C[^C]*\)" ] |> String.concat "&")

    [
        matcher.MatchText("(A----B----C)")
        matcher.MatchText("(B____A____C)")
        matcher.MatchText("(C    B    A)")
    ]


// A B C each twice in any order between parentheses, without & pattern size explodes factorially
let test5a =

    let matcher =
        System.Text.RegularExpressions.Regex(
            [
                // AA positions: 6! / (2!(6-2)!) = 15
                // BB positions: 4! / (2!(4-2)!) = 6
                // 15 x 6 = 90 permutations
                @"\([^ABC]*A[^ABC]*A[^ABC]*B[^ABC]*B[^ABC]*C[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*B[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*C[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*B[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*C[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*A[^ABC]*C[^ABC]*C[^ABC]*B[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*A[^ABC]*C[^ABC]*C[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*B[^ABC]*A[^ABC]*C[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*B[^ABC]*C[^ABC]*A[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*B[^ABC]*C[^ABC]*C[^ABC]*A[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*B[^ABC]*B[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*B[^ABC]*C[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*C[^ABC]*A[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*A[^ABC]*B[^ABC]*B[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*B[^ABC]*A[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*B[^ABC]*C[^ABC]*A[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*B[^ABC]*C[^ABC]*B[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*C[^ABC]*A[^ABC]*B[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*C[^ABC]*B[^ABC]*A[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*A[^ABC]*C[^ABC]*C[^ABC]*B[^ABC]*B[^ABC]*B[^ABC]*\)"
                @"\([^ABC]*B[^ABC]*A[^ABC]*A[^ABC]*B[^ABC]*C[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*B[^ABC]*A[^ABC]*B[^ABC]*A[^ABC]*C[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*B[^ABC]*B[^ABC]*A[^ABC]*A[^ABC]*C[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*B[^ABC]*B[^ABC]*A[^ABC]*C[^ABC]*A[^ABC]*C[^ABC]*\)"
                @"\([^ABC]*B[^ABC]*B[^ABC]*A[^ABC]*C[^ABC]*C[^ABC]*A[^ABC]*\)"
            // ... 60 more permutations
            ]
            |> String.concat "|"
        )

    [
        matcher.Match("(A-A-B-B-C--C)").Value
        matcher.Match("(B-B-A-C-C--A)").Value
        matcher.Match("(C-A-A-C-B--B)").Value
        matcher.Match("(A-B-C-B-A--C)").Value
    ]

// A B C each twice in any order between parentheses, with & pattern remains short
let test5b =
    let matcher =
        Matcher(
            [ @"\([^A]*A[^A]*A[^A]*\)"; @"\([^B]*B[^B]*B[^B]*\)"; @"\([^C]*C[^C]*C[^C]*\)" ]
            |> String.concat "&"
        )

    [
        matcher.MatchText("(A-A-B-B-C--C)").Value
        matcher.MatchText("(B-B-A-C-C--A)").Value
        matcher.MatchText("(C-A-A-C-B--B)").Value
        matcher.MatchText("(A-B-C-B-A--C)").Value
    ]



/// negations with `~`

let password =
    Matcher(
        [ @".*\d.*"; @".*[a-z].*"; @".*[A-Z].*"; @"~(.*\d\d.*)"; ".{5,6}" ]
        |> String.concat "&"
    )


password.MatchText("Aa1aa")
password.MatchText("Aa11aaAA")


let until1 = System.Text.RegularExpressions.Regex(@"ab(?!\b)..").Match("abcde fgahij")
let until2 = System.Text.RegularExpressions.Regex(@"ab(?=\B)..").Match("abcde fgahij")
