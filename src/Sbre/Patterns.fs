module rec Sbre.Patterns

open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open System
open System.Collections.Generic


[<Flags>]
type LoopKind =
    | EmptyLoop = 0uy
    | Single = 1uy
    | Star = 2uy
    | Normal = 4uy

/// loop recognizer from lower and upper
let (|LoopKind|) (x:int,y:int) =
    match x,y with
    | 0, 0 -> LoopKind.EmptyLoop
    | 1, 1 -> LoopKind.Single
    | 0, Int32.MaxValue -> LoopKind.Star
    | _,_ -> LoopKind.Normal


[<return: Struct>]
let inline (|RefNull|_|) (node: obj) =
    match obj.ReferenceEquals(null,node) with
    | true -> ValueSome()
    | _ -> ValueNone


[<return: Struct>]
let inline (|Single|_|) (node: NodeSet<'tset>) =
    match node.Count with
    | 1 -> ValueSome(head node)
    | _ -> ValueNone

[<return: Struct>]
let inline (|ToplevelOrNullable|_|) (isNullable:RegexNode<_> list -> bool) (hashset: HashSet<ToplevelOR>) =
    match hashset.Count with
    | 0 -> ValueNone
    | _ -> if hashset |> Seq.exists (fun v -> isNullable v.Node )  then ValueSome() else ValueNone



[<return: Struct>]
let (|ValueRefEquals|_|) (y:'t list) (x:'t list voption) =
    match x with
    | ValueNone -> ValueNone
    | ValueSome x ->
        match obj.ReferenceEquals(x,y) with
        | true -> ValueSome()
        | _ -> ValueNone

module Solver =
    let inline mapAnd (s:ISolver<^t>) ([<InlineIfLambda>]f: 'a -> ^t) (coll: seq<'a>): ^t =
        let mutable ss = s.Full
        for x in coll do
            ss <- s.And(ss,f x)
        ss

    let inline and' (s:ISolver< ^t>) (v: ^t) (mt: ^t): ^t = s.And(v,mt)

    [<return: Struct>]
    let (|TrueStar|_|) (_solver:ISolver<'t>) (node: RegexNode<'t>) =
        match node with
        | Loop([Singleton pred],0,Int32.MaxValue,info) ->
            if _solver.IsFull(pred) then ValueSome()
            else ValueNone
        | _ -> ValueNone

[<return:Struct>]
let (|ZeroWidthNode|_|) (node: RegexNode<'t>) : unit voption =
    match node with
    | Or(ExistsZeroWidthNode,_)  -> ValueSome()
    | Singleton _ -> ValueNone
    | Loop(_,low,_,info) ->
        if low = 0 then ValueSome()
        else ValueNone
    | Not _ -> ValueNone
    | LookAround _ -> ValueSome()
    | And(AllZeroWidthNodes,_) -> ValueSome()
    | And(xs, info) -> ValueNone
    | Or(xs, info) -> ValueNone

[<return:Struct>]
let (|ExistsZeroWidthNode|_|) (nodes: NodeSet<'t>) : unit voption =
    nodes |> exists (fun list ->
      list |> seqforall (fun v ->
        match v with
        | ZeroWidthNode -> true
        | _ -> false
    )
    ) |> vopt

[<return:Struct>]
let (|AllZeroWidthNodes|_|) (nodes: NodeSet<'t>) : unit voption =
    nodes |> flatten |> seqforall (fun v ->
        match v with
        | ZeroWidthNode -> true
        | _ -> false
    ) |> vopt

[<return: Struct>]
let (|SharedTail|_|) (node: RegexNode<'t> list) : NodeSet<'t> voption  =
    match node with
    | [] -> ValueNone
    | [And(nodes,info)] -> ValueSome(nodes |> filter (List.isEmpty >> not) |> map tail )
    | [Or(nodes,info)] -> ValueSome(nodes |> filter (List.isEmpty >> not) |> map tail )
    | Singleton _::tail -> ValueSome(singleton tail)
    // TBD: these could be optimized, but return none for now
    | Loop _::tail -> ValueNone
    | And _::tail -> ValueNone
    | Not _::tail -> ValueNone
    | Or _::tail -> ValueNone
    | LookAround _::_ -> ValueNone //ValueSome()


module Location =
    let inline create (str: string) (p: int32) : Location = { Input = str; Position = p; Reversed = false }

    // The reverse s ⟨i⟩r of a valid location s ⟨i⟩ in s is the valid location s r⟨|s |−i⟩ in s r.
    let inline rev (loc: Location) =
        {
            Input = loc.Input
            Position = loc.Position
            Reversed = not loc.Reversed
        }


    let inline increment (loc: Location) =
        match loc.Reversed with
        | true -> Location.create loc.Input (loc.Position - 1)
        | _ -> Location.create loc.Input (loc.Position + 1)

    let inline nextPosition (loc: Location) =
        match loc.Reversed with
        | true -> (loc.Position - 1)
        | _ -> (loc.Position + 1 )
    let inline isFinal (loc: Location) =
        match loc.Reversed with
        | true -> loc.Position = 0
        | _ -> loc.Position = loc.Input.Length

    let inline isPreFinal (loc: Location) =
        match loc.Reversed with
        | true -> loc.Position = 1
        | _ -> loc.Position = (loc.Input.Length - 1)
    let inline currentChar (loc: Location) =
        match loc.Reversed with
        | false -> loc.Input[loc.Position]
        | true -> loc.Input[loc.Position - 1]
    let inline currentPos (loc: Location) = loc.Position
    let inline str (loc: Location) = loc.Input
    let inline remainingString (loc: Location) = loc.Input[loc.Position ..]
    let inline endPos (loc: Location) =
        match loc.Reversed with
        | false -> loc.Input.Length
        | true -> 0

    // 2. let = ε ⟨−1⟩ used to represent match failure and also as a pre-initial location
    let inline isFailure (loc: Location) = loc.Position = -1
    let defaultLocation : Location = { Position = -1; Input = ""; Reversed = false }



