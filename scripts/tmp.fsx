#I "../src/Sbre/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
#r "nuget: Thoth.Json.Net"

open System
open System.Threading
open Sbre
open FSharp.Data
open Sbre
open Sbre.Pat
open Thoth.Json.Net



// let twain_full =


let engine = Regex("ab|cd")
let matcher = engine.Matcher :?> RegexMatcher<uint64>

let pat1 = matcher.RawPattern


pat1.ToString() // "(ab|cd)"

type SimpleRegexNode =
    | Concat of  // RE.RE
        head: SimpleRegexNode *
        tail: SimpleRegexNode
    | Epsilon // ε
    | Or of  // RE|RE
        nodes: list<SimpleRegexNode>
    | Singleton of string // 𝜓 predicate
    | Loop of  // RE{𝑚, 𝑛}
        node: SimpleRegexNode *
        low: int *
        up: int
    | And of  // RE&RE ..
        nodes: list<SimpleRegexNode>
    | Not of node: SimpleRegexNode // ~RE
    | Lookahead         of node: SimpleRegexNode
    | Lookbehind        of node: SimpleRegexNode
    | NegLookahead      of node: SimpleRegexNode
    | NegLookbehind     of node: SimpleRegexNode




// (ab|cd)

// (a{0,9} | a{0,10}) ==> a{0,10}

// [abc]
// [a-z]

// [a-q][^u-z]{13}x
// [u0000-uFFFF] (around 60k chars)
// ^u-z // inverse set of u-z
let char1 = int 'u', int 'z' // exclude (117, 122) from 60k chars

// trying to match [^u-z] symbolically
let match1 (someChar:char) =
    let charCode = int someChar // get the char code/ minterm
    let minterm =
        if charCode < 117 && charCode > 122 then
            1
        else 0
    if minterm = 1 then true else false

// trying to match [^u-z] in a regular dfa
let match2 (someChar:char) =
    let charCode = int someChar // get the char code
    if charCode = 0 then true
    elif charCode = 1 then true
    elif charCode = 2 then true
    elif charCode = 3 then true
    else failwith "todo"




// u-z
// around 60k chars (exclude (uvwxyz))



// a{5,5} = a{5} ==> aaaaa

// let pat2 = Epsilon |> Encode.Auto.toString |> File.writeTo "/home/ian/Desktop/temp-disk/encoded-samples/epsilon.json"
// let pat2 =
//     (Or(
//         [
//             Concat(Singleton("a"), Singleton("b"))
//             Concat(Singleton("c"), Singleton("d"))
//         ]
//     ) : SimpleRegexNode )
//     |> Encode.Auto.toString |> File.writeTo "/home/ian/Desktop/temp-disk/encoded-samples/(ab|cd).json"
//
// let pat3 =
//     (Loop(Concat(Singleton("a"), Singleton("b")) , 1, 2 ) : SimpleRegexNode )
//     |> Encode.Auto.toString |> File.writeTo "/home/ian/Desktop/temp-disk/encoded-samples/(ab){1,2}.json"
//
// let pat4 =
//     (Loop(Concat(Singleton("a"), Singleton("b")) , 2, 2 ) : SimpleRegexNode )
//     |> Encode.Auto.toString |> File.writeTo "/home/ian/Desktop/temp-disk/encoded-samples/(ab){2}.json"
//
// let pat5 =
//     (Loop(Concat(Singleton("a"), Singleton("b")) , 0, Int32.MaxValue ) : SimpleRegexNode )
//     |> Encode.Auto.toString |> File.writeTo "/home/ian/Desktop/temp-disk/encoded-samples/(ab)*.json"
//
// let pat6 =
//     (And([Concat(Singleton "ab", Singleton("cd")); Concat(Singleton "a", Singleton("c")) ]) : SimpleRegexNode )
//     |> Encode.Auto.toString |> File.writeTo "/home/ian/Desktop/temp-disk/encoded-samples/([ab][cd]&ac).json"


let encoded = Encode.Auto.toString (pat1)

let jsonString = """["Or",[["Concat",["Singleton","a"],["Singleton","b"]],["Concat",["Singleton","c"],["Singleton","d"]]]]"""

let jsonValue2 = System.Text.Json.JsonDocument.Parse jsonString

let jsonRoot = jsonValue2.RootElement

jsonRoot[1]


// let structure =
//     match pat1 with
//     | Concat(head, tail, info) -> failwith "todo"
//     | Epsilon -> failwith "todo"
//     | Or(nodes, info) -> failwith "todo"
//     | Singleton(set) -> failwith "todo"
//     | Loop(node, low, up, info) -> failwith "todo"
//     | And(nodes, info) -> failwith "todo"
//     | Not(node, info) -> failwith "todo"
//     | LookAround(node, lookback, negate) -> failwith "todo"


let ex2 = Regex("[ab]b")

// "abc" => "1a1b1c"

// 1. concat (OK)
// 2. char classes -> ([abc] to "abc".characterClass)
// 3. OR - ab|bc -> "ab" ⋓ "cd"
// 4. AND - ab&bc -> "ab" ⋒ "cd"
// 5. NEG - ~(ab) -> ~"ab"
// 6. STAR - a* -> "a"*
// 7. LOOP (future todo) - a{1,9} -> ?
// 8. LOOKAROUND (future todo) - (?=a) -> ?="abc"




// skipping - eliminating redundant transitions
let reg16 = Regex("a.*b.*c")
let res17 = reg16.Match("___a_________b_______abababababab____c___")
// "___|a_________b_______abababababab____c___"
// .*b.*c
// "___a|_________b_______abababababab____c___"
// "___a_|________b_______abababababab____c___" ==> .*b.*c
// "___a_________|b_______abababababab____c___"
// .*c
// "___a_________b_______abababababab____|c___"

// .*ababa.*c
// prefix: ababa
let rm = reg16.Matcher :?> RegexMatcher<uint64>

let d1 =
    let loc = Location.create "a" 0
    let pred = rm.Cache.MintermForLocation(loc)
    Algorithm.createDerivative (rm.Cache, &loc, pred, rm.RawPattern)

rm.Cache.PrettyPrintNode(d1)

let d2 =
    let loc = Location.create "_" 0
    let pred = rm.Cache.MintermForLocation(loc)
    Algorithm.createDerivative (rm.Cache, &loc, pred, d1)

rm.Cache.PrettyPrintNode(d1)
rm.Cache.PrettyPrintNode(d2)
obj.ReferenceEquals(d1, d2)


let allMinterms = rm.Cache.Minterms()
let prettyMinterms = allMinterms |> Array.map rm.Cache.PrettyPrintMinterm

// before a match has started (not ⊥ && not same)
// a.*b.*c
// [|"[^\na-c]"; "\n"; "a"; "b"; "c"|]
// [|"⊥"; "⊥"; ".*b.*c"; "⊥"; "⊥"|]

// a* is already in this state
// after a match has started (not same)
// .*b.*c
// [|"[^\na-c]"; "\n"; "a"; "b"; "c"|]
// [|".*b.*c"; "⊥"; ".*b.*c"; ".*c"; ".*b.*c"|]

let allInitialDerivatives =
    allMinterms
    |> Array.map (fun minterm ->
        let loc = Location.create "_" 0 // irrelevant without lookarounds
        Algorithm.createDerivative (rm.Cache, &loc, minterm, rm.RawPattern)
    )

let initialpats = allInitialDerivatives |> Array.map string

let all2ndDerivatives =
    allMinterms
    |> Array.map (fun minterm ->
        let loc = Location.create "_" 0
        Algorithm.createDerivative (rm.Cache, &loc, minterm, d1)
    )

let tmp52 = all2ndDerivatives |> Array.map string

let startset =
    allMinterms
    |> Array.where (fun minterm ->
        let loc = Location.create "_" 0
        let der = Algorithm.createDerivative (rm.Cache, &loc, minterm, d1)
        not (refEq der d1)
    )
    |> Array.reduce (|||) // bitwise merge all minterms

rm.Cache.PrettyPrintMinterm(startset)

// subsumption - eliminating redundant branches

// (.*&.*s)                 ==> .*s
// [a-z]{0,9}y|[a-z]{0,10}y ==> [a-z]{0,10}y   // (ε|[a-z]){10}y
// (a*|.*)                  ==> .*                (POSIX)
// 12.*3|12.*4 ==> 12.*[34]                       (POSIX)
// abc&ab                   ==> ⊥
// (ε&⊤*English⊤*&⊤*Paris⊤*&⊤*King⊤*) ==> ⊥
// ..a&a.a ==>                       a.a



// (.*t.*hat.*&.*w.*as.*&.*t.*he.*&.*nd.*)|((s.*|.*as.*)&.*t.*hat.*&.*t.*he.*&.*nd.*) ==> ((s.*|.*as.*)&.*t.*hat.*&.*t.*he.*&.*nd.*)
