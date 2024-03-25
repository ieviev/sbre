#I "../src/Sbre/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"
#r "nuget: Thoth.Json.Net"

open System
open System.Threading
open Sbre
open FSharp.Data
open Sbre
open Sbre.Types
open Thoth.Json.Net
open Sbre.Cache
open Sbre.Common

[<RequireQualifiedAccess>]
type LeanRegexNode =
    | Concat of  // RE.RE
        head: LeanRegexNode *
        tail: LeanRegexNode
    | Epsilon // ε
    | Or of  // RE|RE
        nodes: list<LeanRegexNode>
    | Singleton of string // 𝜓 predicate
    | Loop of  // RE{𝑚}
        node: LeanRegexNode *
        ub: int
    | Star of node: LeanRegexNode // RE*
    | And of  // RE&RE ..
        nodes: list<LeanRegexNode>
    | Not of node: LeanRegexNode // ~RE
    | Lookahead of node: LeanRegexNode
    | Lookbehind of node: LeanRegexNode
    | NegLookahead of node: LeanRegexNode
    | NegLookbehind of node: LeanRegexNode

let mkLeanRegexNode (cache: RegexCache<_>) (tsetToLean: TSet -> string) (node: RegexNode<_>) =
    let trueNode = LeanRegexNode.Singleton(tsetToLean cache.Solver.Full)

    let rec loop node =
        match node with
        | RegexNode.Concat(head = head; tail = tail) -> LeanRegexNode.Concat(loop head, loop tail)
        | RegexNode.Epsilon -> LeanRegexNode.Epsilon
        | RegexNode.Or(nodes = nodes) -> LeanRegexNode.Or(Seq.map loop nodes |> List.ofSeq)
        | RegexNode.And(nodes = nodes) -> LeanRegexNode.And(Seq.map loop nodes |> List.ofSeq)
        | RegexNode.Singleton(set) -> LeanRegexNode.Singleton(tsetToLean set)
        | RegexNode.Loop(node = node; low = low; up = up) ->
            match low, up with
            | 0, Int32.MaxValue -> LeanRegexNode.Star(loop node)
            | 1, Int32.MaxValue -> LeanRegexNode.Concat(loop node, LeanRegexNode.Star(loop node))
            | _ ->
                // (ab){2,5}
                // (ab){2} . (ab){0,3}
                // (ab){2} . (ab|ε){3}
                let requiredLoopLength = low

                let optionalPart =
                    match up - requiredLoopLength with
                    | 0 -> LeanRegexNode.Epsilon
                    | optionalLength ->
                        LeanRegexNode.Loop(
                            LeanRegexNode.Or([ LeanRegexNode.Epsilon; loop node ]),
                            optionalLength
                        )

                let requiredLoop = LeanRegexNode.Loop(loop node, requiredLoopLength)
                LeanRegexNode.Concat(optionalPart, requiredLoop)
        | RegexNode.Not(node = node) -> LeanRegexNode.Not(loop node)
        | RegexNode.LookAround(node = node; lookBack = lookback; pendingNullables = pendingnullables) ->
            assert (pendingnullables.IsEmpty) // this changes the lookaround body

            match lookback with
            | false -> LeanRegexNode.Lookahead(loop node)
            | true -> LeanRegexNode.Lookbehind(loop node)
        | RegexNode.Begin -> LeanRegexNode.NegLookbehind(trueNode)
        | RegexNode.End -> LeanRegexNode.NegLookahead(trueNode)

    loop node


let pat2lean(pattern: string) =
    let m = Regex(pattern).TSetMatcher
    let mts = m.Cache.Minterms()
    let leanMtLookup = mts |> Seq.mapi (fun i v -> v, char (96 + i)) |> Map

    let tsetToLean(ts: TSet) =
        mts
        |> Seq.where (m.Cache.Solver.elemOfSet ts)
        |> Seq.map (fun v -> (Map.find v leanMtLookup))
        |> Seq.toArray
        |> System.String

    mkLeanRegexNode m.Cache (tsetToLean) (m.RawPattern)

let encodePat(leanNode: LeanRegexNode) = Encode.Auto.toString leanNode

let oraclePath = "/mnt/sdc4/repos/MatchingExperiments/.lake/build/bin/oracle"

let getLeanMatch (pattern:string) (input:string) =
    let message = String.concat "\n" [
        pattern |> pat2lean |> encodePat
        input //  __abc__bcd
    ]
    Process.runToString(oraclePath, "", stdin=message)

let t1 = getLeanMatch "abc" "____abc"
let t2 = getLeanMatch "ab{1,3}" "___ababab__"
let t3 = getLeanMatch "(ab|cd)" "___ababab__"



















