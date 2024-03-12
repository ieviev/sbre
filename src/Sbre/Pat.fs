/// patterns, helpers
module rec Sbre.Pat

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
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
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.isElemOfSet(predicate: 't, locationMinterm: 't) =
            not (this.IsEmpty(this.And(locationMinterm, predicate)))

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.elemOfSet(predicate: 't) (locationMinterm: 't) =
            not (this.IsEmpty(this.And(locationMinterm, predicate)))

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.notElemOfSet(predicate: 't) (locationMinterm: 't) =
            this.IsEmpty(this.And(locationMinterm, predicate))

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.contains(larger: 't) (smaller: 't) =
            let overlapped = this.And(smaller,larger)
            // smaller = overlapped
            match box overlapped, box smaller with
            | (:? uint64 as ov), (:? uint64 as sm) -> ov = sm
            | (:? BDD as ov), (:? BDD as sm) -> ov = sm
            | (:? BitVector as ov), (:? BitVector as sm) -> ov = sm
            | _ -> failwith "invalid set"


#nowarn "42"

module Solver =
    // let inline elemOfSet predicate locationMinterm =
    //     predicate &&& locationMinterm <> LanguagePrimitives.GenericZero
    // let inline il_and (x:'t) (y:'t) =
    //     (# "and" x y : 't #)
    // let inline notElemOfSet predicate locationMinterm =
    //     (predicate &&& locationMinterm) = LanguagePrimitives.GenericZero
    // let inline contains
    //     (larger: ^d when ^d : struct)
    //     (smaller: ^d): bool =
    //         (larger &&& smaller) = smaller

    let containsS
        (solver:ISolver<'d>)
        (larger: 'd)
        (smaller: 'd): bool =
            let overlapped = solver.And(smaller,larger)
            obj.ReferenceEquals((box smaller),(box overlapped)) || smaller = overlapped



    // let inline not' predicate = ~~~predicate
    // // let inline isEmpty (predicate:TSet) = predicate = LanguagePrimitives.GenericZero
    // let inline mapOr (s:ISolver<^t>) ([<InlineIfLambda>]f: 'a -> ^t) xs: ^t =
    //     let mutable startset = s.Empty
    //     for x in xs do
    //         startset <- s.Or(startset,f x)
    //     startset

    let inline mergeOrWithEnumerator
        (s:ISolver<^t>)
        ([<InlineIfLambda>]f: RegexNode<'t> -> ^t)
        (coll:byref<Collections.Immutable.ImmutableHashSet<RegexNode<'t>>.Enumerator>): ^t =
        let mutable ss = s.Empty
        while (not (s.IsFull(ss))) && coll.MoveNext() do
            ss <- s.Or(ss,f coll.Current)
        ss

    let inline mergeSets
        (s:ISolver<^t>)
        (coll:seq<^t>): ^t =
        let mutable ss = s.Empty
        use mutable coll = coll.GetEnumerator()
        while (not (s.IsFull(ss))) && coll.MoveNext() do
            ss <- s.Or(ss,coll.Current)
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

[<Flags>]
type LoopKind =
    | EmptyLoop = 0uy
    | Single = 1uy
    | Star = 2uy
    | Plus = 4uy
    | Normal = 8uy

/// loop recognizer from lower and upper
let inline (|LoopKind|) struct(x:int,y:int) =
    match struct(x,y) with
    | 0, 0 -> LoopKind.EmptyLoop
    | 1, 1 -> LoopKind.Single
    | 0, Int32.MaxValue -> LoopKind.Star
    | 1, Int32.MaxValue -> LoopKind.Plus
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
    | Concat (head,tail,k_) ->
        if loopSubsumesBranch solver largePred head then
            loopSubsumesBranch solver largePred tail
        else false
    | Or (nodes,_) -> nodes |> Seq.forall (loopSubsumesBranch solver largePred)
    | Loop(node, low, up, info) ->
        // TODO: proper loop subsumption
        false
    | _ ->
        false

let isSubsumedFromAnd (solver:ISolver<'t>) pred singletonLoop (nodes:RegexNode<_>seq) =
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


[<return: Struct>]
let (|LookbackPrefix|_|) (node: RegexNode<_>) =
    match node with
    | LookAround(lookBack=true) -> ValueSome(node)
    | Concat(head=LookAround(lookBack=true)) -> ValueSome(node)
    | _ -> ValueNone

[<return: Struct>]
let (|HasInfo|_|) (node: RegexNode<_>) =
    match node with
    | Concat(info=info) | Or(info=info) | Not(info=info) | And(info=info) | LookAround(info=info) -> ValueSome info
    | _ -> ValueNone

[<return: Struct>]
let (|HasPrefixLookback|_|) (node: RegexNode<_>) =
    match node with
    | LookAround(lookBack=true) -> ValueSome()
    | HasInfo info ->
        if info.NodeFlags.HasPrefixLookbehind then ValueSome()
        else ValueNone
    | _ -> ValueNone

[<return: Struct>]
let (|HasSuffixLookahead|_|) (node: RegexNode<_>) =
    match node with
    | LookAround(lookBack=false) -> ValueSome()
    | HasInfo info ->
        if info.NodeFlags.HasSuffixLookahead then ValueSome()
        else ValueNone
    | _ -> ValueNone


[<return: Struct>]
let (|HasPrefixOrSuffix|_|) (node: RegexNode<_>) =
    match node with
    | LookAround(lookBack=true) -> ValueSome()
    | Concat(head=LookAround(lookBack=true)) -> ValueSome()
    | Concat(tail=LookAround(lookBack=true)) -> ValueSome()
    | Concat(tail=HasPrefixOrSuffix()) -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|StarLoop|_|) (node: RegexNode<_>) =
    match node with
    | Loop(node=starNode;low=0;up=Int32.MaxValue) -> ValueSome(starNode)
    | _ -> ValueNone

let (|SplitTail|) (node: RegexNode<_>) =
    let tmp = ResizeArray()
    let rec loop node =
        match node with
        | Concat(head=h;tail=tail) ->
            tmp.Add(h)
            loop(tail)
        | _ -> tmp, node
    loop node

[<return: Struct>]
let (|TrueStar|_|) (solver: ISolver<_>) (node: RegexNode<_>) =
    match node with
    | Loop(node=Singleton pred;low=0;up=Int32.MaxValue) when solver.IsFull(pred) -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|BoundedLoop|_|) (node: RegexNode<_>) =
    match node with
    | Concat(head=Loop(node=Singleton pred;low=low;up=up); tail=tail) when up <> Int32.MaxValue -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|CounterNode|_|) (node: RegexNode<_>) =
    match node with
    | (BoundedLoop as loop) | Concat(head=(BoundedLoop) as loop; tail=_)  -> ValueSome(loop)
    | _ -> ValueNone


[<return: Struct>]
let (|ZeroboundSetLoop|_|) (node: RegexNode<_>) =
    match node with
    | Loop(node=Singleton pred;low=0;) -> ValueSome(pred)
    | _ -> ValueNone


[<return: Struct>]
let (|AllSameHead|_|) (nodes: seq<RegexNode<_>>) =
    let mutable allsame = true
    let mutable headRef : RegexNode<_> = Unchecked.defaultof<_>
    nodes
    |> Seq.pairwise
    |> Seq.iter (fun (prev, v) ->
        match prev, v with
        | Concat(head = head1; tail = _), Concat(head = head2; tail = _) ->
            if not (obj.ReferenceEquals(head1, head2)) then
                allsame <- false
            else
                headRef <- head1
        | _ -> allsame <- false
    )
    if allsame then
        ValueSome(headRef)
    else ValueNone


module Location =
    let getDefault() : Location = { Input = ReadOnlySpan.Empty; Reversed = false; Position = 0 }
    let getNonInitial() : Location = { Input = "abc".AsSpan() ; Reversed = false; Position = 1 }
    let inline create (str: string) (p: int32) : Location = { Input = str.AsSpan(); Position = p; Reversed = false }
    let inline createSpan (str: ReadOnlySpan<char>) (p: int32) : Location = { Input = str; Position = p; Reversed = false }
    let inline clone (loc:inref<Location>) : Location =
        { Input = loc.Input ; Position = loc.Position ; Reversed = loc.Reversed }
    let inline createSpanRev (str: ReadOnlySpan<char>) (p: int32) (forwards:bool) : Location = {
        Input = str; Position = p; Reversed = not forwards
    }
    let inline createReversedSpan (str: ReadOnlySpan<char>) : Location = {
        Input = str; Position = str.Length; Reversed = true
    }

    let inline isFinal (loc: Location) =
        loc.Reversed && loc.Position = 0
        || not loc.Reversed && loc.Position = loc.Input.Length

    let inline nextPosition (loc: Location) =
        match loc.Reversed with
        | true -> (loc.Position - 1)
        | _ -> (loc.Position + 1 )

    let inline final (loc: Location) =
        match loc.Reversed with
        | true -> 0
        | _ -> loc.Input.Length

/// same as obj.ReferenceEquals(x, y)
let inline refEq x y =
    // obj.ReferenceEquals(x, y)
    // obj.ReferenceEquals(x, y)
    LanguagePrimitives.PhysicalEquality x y

/// same pointer location
let inline same(x:inref<_>, y:inref<_>) =
    Runtime.CompilerServices.Unsafe.AreSame(&x,&y)
