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
open Sbre.Types
open Thoth.Json.Net

[<RequireQualifiedAccess>]
type LeanRegexNode =
    | Concat of  // RE.RE
        head: LeanRegexNode *
        tail: LeanRegexNode
    | Epsilon // ε
    | Or of  // RE|RE
        nodes: list<LeanRegexNode>
    | Singleton of string // 𝜓 predicate
    | Loop of  // RE{𝑚, 𝑛}
        node: LeanRegexNode *
        low: int *
        up: int
    | And of  // RE&RE ..
        nodes: list<LeanRegexNode>
    | Not of node: LeanRegexNode // ~RE
    | Lookahead of node: LeanRegexNode
    | Lookbehind of node: LeanRegexNode
    | NegLookahead of node: LeanRegexNode
    | NegLookbehind of node: LeanRegexNode

let mkLeanRegexNode (cache:RegexCache<_>) (tsetToLean:TSet -> string) (node: RegexNode<_>) =
    let trueNode = LeanRegexNode.Singleton(tsetToLean cache.Solver.Full)
    let rec loop node =
        match node with
        | RegexNode.Concat(head = head; tail = tail) ->
            LeanRegexNode.Concat(loop head, loop tail)
        | RegexNode.Epsilon -> LeanRegexNode.Epsilon
        | RegexNode.Or(nodes = nodes) -> LeanRegexNode.Or(Seq.map loop nodes |> List.ofSeq)
        | RegexNode.And(nodes = nodes) -> LeanRegexNode.And(Seq.map loop nodes |> List.ofSeq)
        | RegexNode.Singleton(set) -> LeanRegexNode.Singleton(tsetToLean set)
        | RegexNode.Loop(node=node; low=low; up=up) -> LeanRegexNode.Loop(loop node, low, up)
        | RegexNode.Not(node=node) -> LeanRegexNode.Not(loop node)
        | RegexNode.LookAround(node=node; lookBack=lookback; pendingNullables=pendingnullables) ->
            assert (pendingnullables.IsEmpty) // this changes the lookaround body
            match lookback with
            | false -> LeanRegexNode.Lookahead(loop node)
            | true -> LeanRegexNode.Lookbehind(loop node)
        | RegexNode.Begin -> LeanRegexNode.NegLookbehind(trueNode)
        | RegexNode.End -> LeanRegexNode.NegLookahead(trueNode)
    loop node


let pat2lean (pattern:string) =  
    let m = Regex(pattern).TSetMatcher
    let mts = m.Cache.Minterms()
    let leanMtLookup =
        mts
        |> Seq.mapi (fun i v -> v, char (97 + i))
        |> Map
    let tsetToLean (ts: TSet) =
        mts
        |> Seq.where (Solver.elemOfSet ts)
        |> Seq.map (fun v -> (Map.find v leanMtLookup))
        |> Seq.toArray
        |> System.String
    mkLeanRegexNode m.Cache (tsetToLean) (m.RawPattern)

let encodePat (leanNode:LeanRegexNode) = Encode.Auto.toString leanNode 

let p1 = pat2lean "abc"
let p2 = pat2lean "(ab|cd)"


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
// let reg16 = Regex("a.*b.*c")
// let res17 = reg16.Match("___a_________b_______abababababab____c___")
// // "___|a_________b_______abababababab____c___"
// // .*b.*c
// // "___a|_________b_______abababababab____c___"
// // "___a_|________b_______abababababab____c___" ==> .*b.*c
// // "___a_________|b_______abababababab____c___"
// // .*c
// // "___a_________b_______abababababab____|c___"
//
//
// // "a.*b.*c"
// let rm = reg16.Matcher :?> RegexMatcher<uint64>
// let state = RegexState()

// let d1 =
//     let loc = Location.create "a___b____c_" 0
//     let pred = rm.Cache.MintermForLocation(loc)
//     Algorithm.createDerivative (rm.Cache, state, &loc, pred, rm.RawPattern)

// rm.Cache.PrettyPrintNode(d1)

// let d2 =
//     let loc = Location.create "_" 0
//     let pred = rm.Cache.MintermForLocation(loc)
//     Algorithm.createDerivative (rm.Cache, state, &loc, pred, d1)

// rm.Cache.PrettyPrintNode(d1)
// rm.Cache.PrettyPrintNode(d2)
// obj.ReferenceEquals(d1, d2)


// let allMinterms = rm.Cache.Minterms()
// let prettyMinterms = allMinterms |> Array.map rm.Cache.PrettyPrintMinterm

//
// ^ - not
//     \n - not \n
//     a-c - not a,b,c
// not (\n, a ,b ,c)

// a.*b.*c , "a" ==> .*b.*c

// before a match has started (not ⊥ && not same)
// a.*b.*c
// [|"[^\na-c]"; "\n";  "a";        "b";      "c"|]
// [|"⊥";        "⊥";   ".*b.*c";   "⊥";      "⊥"|]

// a* is already in this state
// after a match has started (not same)
// .*b.*c
// [|"[^\na-c]"; "\n"; "a"; "b"; "c"|]
// [|".*b.*c"; "⊥"; ".*b.*c"; ".*c"; ".*b.*c"|]

// let allInitialDerivatives =
//     allMinterms
//     |> Array.map (fun minterm ->
//         let loc = Location.create "_" 0 // irrelevant without lookarounds
//         Algorithm.createDerivative (rm.Cache, &loc, minterm, rm.RawPattern)
//     )

// let initialpats = allInitialDerivatives |> Array.map string

// let all2ndDerivatives =
//     allMinterms
//     |> Array.map (fun minterm ->
//         let loc = Location.create "_" 0
//         Algorithm.createDerivative (rm.Cache, &loc, minterm, d1)
//     )

// let tmp52 = all2ndDerivatives |> Array.map string

// let startset =
//     allMinterms
//     |> Array.where (fun minterm ->
//         let loc = Location.create "_" 0
//         let der = Algorithm.createDerivative (rm.Cache, &loc, minterm, d1)
//         not (refEq der d1)
//     )
//     |> Array.reduce (|||) // bitwise merge all minterms

// rm.Cache.PrettyPrintMinterm(startset)

// subsumption - eliminating redundant branches

// (.*&.*s)                 ==> .*s
// [a-z]{0,9}y|[a-z]{0,10}y ==> [a-z]{0,10}y   // (ε|[a-z]){10}y
// (a*|.*)                  ==> .*                (POSIX)
// 12.*3|12.*4 ==> 12.*[34]                       (POSIX)
// abc&ab                   ==> ⊥
// (ε&⊤*English⊤*&⊤*Paris⊤*&⊤*King⊤*) ==> ⊥
// ..a&a.a ==>                       a.a



// (.*t.*hat.*&.*w.*as.*&.*t.*he.*&.*nd.*)|((s.*|.*as.*)&.*t.*hat.*&.*t.*he.*&.*nd.*) ==> ((s.*|.*as.*)&.*t.*hat.*&.*t.*he.*&.*nd.*)




// abc
// Concat(a, Concat(b, c))  // structure

// 1. collect all sets
// {a,b,c}
// 2. minterms, all 4 unique cases
// "anything else"; "a"; "b"; "c"

// 3. compute all possible cases (the dfa)

// minterms minimize the size of this table
// abc, "anything else"; ==> "⊥"
// abc, "a"; ==> "bc"
// abc, "b"; ==> "⊥"
// abc, "c"; ==> "⊥"
// --------------
// bc, "anything else"; ==> "⊥"
// bc, "a"; ==> "⊥"
// bc, "b"; ==> "c"
// bc, "c"; ==> "⊥"


// [|"[^\na-c]"; "\n";  "a";        "b";      "c"|]
// [|"⊥";        "⊥";   ".*b.*c";   "⊥";      "⊥"|]


// let r = Regex("[1-3].*[2-7].*[5-9]")
// // 1___2__5
// // 2___2__5
// // 2___2__5
// let m = r.Matcher :?> RegexMatcher<uint64>
//
// let minterms2 = m.Cache.Minterms()
// let prettyminterms2 =
//     minterms2
//     |> Array.map (fun v -> m.Cache.PrettyPrintMinterm(v))

// [|"[^\n1-9]"; "\n"; "1"; "[23]"; "4"; "[5-7]"; "[89]"; |]

// possible overlap
// "1" - only included in [1-3] but not [2-7] or [5-9]
// "[23]" - included in both [1-3] [2-7] but not [5-9]
// ..

// these have a lot of overlap
// . - any non \n char
// \w - any non whitespace char
// \s - any whitespace char
// \d - any digit char (included in \w too) (370 digits actually)
// [0-9] - (arabic) digits 0 to 9
// [2800-28FF] - braille characters (unicode set)

// let res17 = reg16.Match("___a_________b_______abababababab____c___")
