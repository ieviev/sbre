module rec Sbre.Pat

open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open System
open System.Collections.Generic


[<AutoOpen>]
module Extensions =
    type ISolver<'t> with

        /// si ∈ [[ψ]]
        /// - i.e. location si is elem of Singleton ψ
        /// - (location is smaller than singleton)
        /// - predicate matches location
        member inline this.isElemOfSet(predicate: 't, locationMinterm: 't) =
            not (this.IsEmpty(this.And(locationMinterm, predicate)))


[<Flags>]
type LoopKind =
    | EmptyLoop = 0uy
    | Single = 1uy
    | Star = 2uy
    | Plus = 4uy
    | Normal = 8uy

/// loop recognizer from lower and upper
let (|LoopKind|) struct(x:int,y:int) =
    match x,y with
    | 0, 0 -> LoopKind.EmptyLoop
    | 1, 1 -> LoopKind.Single
    | 0, Int32.MaxValue -> LoopKind.Star
    | 0, 1 -> LoopKind.Plus
    | _,_ -> LoopKind.Normal


let isSubsumedFromAnd pred (nodes:RegexNode<_>[]) =
    nodes
    |> Array.exists
        (fun other ->
        match other with
        | Concat(SingletonStarLoop(cpred),t,_) when cpred = pred -> true
        | _ -> false
    )

let rec loopSubsumesBranch (solver:ISolver<'t>) (pred: 't) (node:RegexNode<'t>) =
    match node with
    | Epsilon -> false
    | Singleton hpred ->
        solver.isElemOfSet(pred,hpred)
    | Loop(node=Singleton pred2) ->
        if pred = pred2 then false
        elif solver.isElemOfSet(pred,pred2) then true
        else false
    | Concat (head,tail,info) ->
        if loopSubsumesBranch solver pred head then
            loopSubsumesBranch solver pred tail
        else false
    | Or (nodes,_) -> nodes |> Seq.forall (loopSubsumesBranch solver pred)
    | Loop(node, low, up, info) ->
        // TODO: proper loop subsumption
        false
    | _ ->
        false




[<return: Struct>]
let (|SingletonStarLoop|_|) (node: RegexNode<_>) =
    match node with
    | Loop(node=Singleton pred;low=0;up=Int32.MaxValue) -> ValueSome(pred)
    | _ -> ValueNone

[<return: Struct>]
let (|SingletonLoop|_|) (node: RegexNode<_>) =
    match node with
    | Loop(node=Singleton pred) -> ValueSome(pred)
    | _ -> ValueNone



[<return: Struct>]
let inline (|RefNull|_|) (node: obj) =
    match obj.ReferenceEquals(null,node) with
    | true -> ValueSome()
    | _ -> ValueNone


[<return: Struct>]
let inline (|RefEq|_|) (v) (node: obj) =
    match obj.ReferenceEquals(node, v) with
    | true -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let inline (|Single|_|) (node: NodeSet<'tset>) =
    match node.Count with
    | 1 -> ValueSome(head node)
    | _ -> ValueNone

[<return: Struct>]
let inline (|ToplevelOrNullable|_|) (isNullable:RegexNode<_> -> bool) (hashset: ToplevelORCollection) =
    match hashset.Count with
    | 0 -> ValueNone
    | _ -> if hashset.Items |> Seq.exists isNullable  then ValueSome() else ValueNone


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

    let inline mapOr (s:ISolver<^t>) ([<InlineIfLambda>]f: 'a -> ^t) (coll): ^t =
        let mutable ss = s.Empty
        for x in coll do
            ss <- s.Or(ss,f x)
        ss

    let inline mergeOrWithEnumerator
        (s:ISolver<^t>)
        ([<InlineIfLambda>]f: RegexNode<'t> -> ^t)
        (coll:byref<Collections.Immutable.ImmutableHashSet<RegexNode<'t>>.Enumerator>): ^t =
        let mutable ss = s.Empty
        // for x in coll do
        while coll.MoveNext() = true do
            ss <- s.Or(ss,f coll.Current)
        ss

    [<return: Struct>]
    let inline (|TrueStar|_|) (_solver:ISolver<'t>) (node: RegexNode<'t>) =
        match node with
        | Loop(Singleton pred,0,Int32.MaxValue,info) ->
            if _solver.IsFull(pred) then ValueSome()
            else ValueNone
        | _ -> ValueNone


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

    let inline posIsPreFinal (pos:int, loc: Location) =
        match loc.Reversed with
        | true -> pos = 1
        | _ -> pos = (loc.Input.Length - 1)
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



