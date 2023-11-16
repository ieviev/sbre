module rec Sbre.Pat

open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open System

[<AutoOpen>]
module Extensions =
    type ISolver<'t> with
        /// si ∈ [[ψ]]
        /// - i.e. location si is elem of Singleton ψ
        /// - (location is smaller than singleton)
        /// - predicate matches location
        member inline this.isElemOfSet(predicate: 't, locationMinterm: 't) =
            not (this.IsEmpty(this.And(locationMinterm, predicate)))

        /// marginally faster variant that skips the middleman
        member inline this.isElemOfSetu64(predicate: uint64, locationMinterm: uint64) =
            predicate &&& locationMinterm <> 0uL


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


let rec loopSubsumesBranch (solver:ISolver<'t>) (largePred: 't) (node:RegexNode<'t>) =
    match node with
    | Epsilon -> false
    | Singleton hpred -> solver.isElemOfSet(largePred,hpred)
    | Loop(node=Singleton pred2; low=0; up = Int32.MaxValue) ->
        let containsv = solver.And(largePred, pred2) = pred2
        if largePred = pred2 then true
        elif containsv then true
        else false
    | Concat (head,tail,info) ->
        if loopSubsumesBranch solver largePred head then
            loopSubsumesBranch solver largePred tail
        else false
    | Or (nodes,_) -> nodes |> Seq.forall (loopSubsumesBranch solver largePred)
    | Loop(node, low, up, info) ->
        // TODO: proper loop subsumption
        false
    | _ ->
        false

let isSubsumedFromAnd (solver:ISolver<'t>) (pred) (singletonLoop) (nodes:RegexNode<_>seq) =
    nodes
    |> Seq.exists
        (fun other ->
        not (obj.ReferenceEquals(other,singletonLoop)) && loopSubsumesBranch solver pred other
    )

[<return: Struct>]
let (|SingletonStarLoop|_|) (node: RegexNode<_>) =
    match node with
    | Loop(node=Singleton pred;low=0;up=Int32.MaxValue) -> ValueSome(pred)
    | _ -> ValueNone

module Solver =
    let inline isElemOfSetU64 predicate locationMinterm = predicate &&& locationMinterm <> 0uL
    let inline elemOfSet predicate locationMinterm =
        predicate &&& locationMinterm <> LanguagePrimitives.GenericZero
    let inline notElemOfSet predicate locationMinterm =
        (predicate &&& locationMinterm) = LanguagePrimitives.GenericZero
    let inline contains larger smaller = (larger &&& smaller) = smaller
    let inline notElemOfSetU64 predicate locationMinterm = predicate &&& locationMinterm = 0uL
    let inline not predicate = ~~~predicate
    let inline mapOr (s:ISolver<^t>) ([<InlineIfLambda>]f: 'a -> ^t) (xs): ^t =
        let mutable startset = s.Empty
        for x in xs do
            startset <- s.Or(startset,f x)
        startset

    let inline mergeOrWithEnumerator
        (s:ISolver<^t>)
        ([<InlineIfLambda>]f: RegexNode<'t> -> ^t)
        (coll:byref<Collections.Immutable.ImmutableHashSet<RegexNode<'t>>.Enumerator>): ^t =
        let mutable ss = s.Empty
        while coll.MoveNext() = true do
            ss <- s.Or(ss,f coll.Current)
        ss

    let inline mergeNonFullWithEnumerator
        (s:ISolver<^t>)
        ([<InlineIfLambda>]f: RegexNode<'t> -> ^t)
        (coll:byref<Collections.Immutable.ImmutableHashSet<RegexNode<'t>>.Enumerator>): ^t =
        let mutable ss = s.Empty
        while coll.MoveNext() = true do
            let pot = f coll.Current
            if s.IsFull(pot) then () else
            ss <- s.Or(ss,pot)

        if s.IsEmpty(ss) then s.Full else
        ss


module Location =
    let inline create (str: string) (p: int32) : Location = { Input = str; Position = p; Reversed = false }
    let inline rev (loc: Location) =
        {
            Input = loc.Input
            Position = loc.Position
            Reversed = not loc.Reversed
        }
    let inline nextPosition (loc: Location) =
        match loc.Reversed with
        | true -> (loc.Position - 1)
        | _ -> (loc.Position + 1 )
    let inline isFinal (loc: Location) =
        loc.Reversed && loc.Position = 0
        || not loc.Reversed && loc.Position = loc.Input.Length
    let inline final (loc: Location) =
        match loc.Reversed with
        | true -> 0
        | _ -> loc.Input.Length


let inline refEq x y = obj.ReferenceEquals(x, y)
