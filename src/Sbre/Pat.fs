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
        member this.notElemOfSet(predicate: 't, locationMinterm: 't) =
            this.IsEmpty(this.And(locationMinterm, predicate))



#nowarn "42"

module Solver =
    let inline elemOfSet predicate locationMinterm =
        predicate &&& locationMinterm <> LanguagePrimitives.GenericZero
    let inline il_and (x:'t) (y:'t) =
        (# "and" x y : 't #)
    let inline notElemOfSet predicate locationMinterm =
        (predicate &&& locationMinterm) = LanguagePrimitives.GenericZero
    let inline contains
        (larger: ^d when ^d : struct)
        (smaller: ^d): bool =
            (larger &&& smaller) = smaller
    let inline not' predicate = ~~~predicate
    let inline isEmpty predicate = predicate = 0UL
    let inline mapOr (s:ISolver<^t>) ([<InlineIfLambda>]f: 'a -> ^t) xs: ^t =
        let mutable startset = s.Empty
        for x in xs do
            startset <- s.Or(startset,f x)
        startset

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
let (|TrueStarredConcat|_|) (solver: ISolver<_>) (node: RegexNode<_>) =
    match node with
    | Concat(head=TrueStar solver; tail=tail; info=info) when not info.NodeFlags.HasCounter -> ValueSome(tail)
    | _ -> ValueNone


[<return: Struct>]
let (|CounterNode|_|) (node: RegexNode<_>) =
    match node with
    | (BoundedLoop as loop) | Concat(head=(BoundedLoop) as loop; tail=_)  -> ValueSome(loop)
    | _ -> ValueNone


// [<return: Struct>]
// let (|MonadicNode|_|) (node: RegexNode<_>) =
//     match node with
//     | (BoundedLoop as loop) | Concat(head=(BoundedLoop) as loop; tail=_)  -> ValueSome(loop)
//     | _ -> ValueNone


// private bool IsMonadic(string regex)
// 	{
// 		Predicate<SymbolicRegexNode<ulong>> pred = (SymbolicRegexNode<ulong> node) => node.Kind == SymbolicRegexKind.Loop && node.Left.Kind != SymbolicRegexKind.Singleton && !node.IsStar && !node.IsMaybe && !node.IsPlus;
// 		return !(new Regex(regex, RegexOptions.Singleline).Compile(true, false, false) as SymbolicRegexUInt64).Pattern.ExistsNode(pred);
// 	}



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
        | Concat(head = head1; tail = tail1), Concat(head = head2; tail = tail2) ->
            if not (obj.ReferenceEquals(head1, head2)) then
                allsame <- false
            else
                headRef <- head1
        | _ -> allsame <- false
    )
    if allsame then
        ValueSome(headRef)
    else ValueNone



let rec isSubSequence (bigger: RegexNode<TSet>) (smaller: RegexNode<TSet>): RegexNode<TSet> voption  =
    match bigger, smaller with
    | Concat(head = SingletonStarLoop(head1); tail = tail1), Or(nodes = xs) ->
        xs
        |> Seq.tryPick (fun v ->
            match isSubSequence tail1 v with
            | ValueSome v -> Some v
            | _ -> None
        )
        |> function
        | Some v -> ValueSome v
        | _ -> ValueNone
    | Concat(head = head1; tail = tail1), Concat(head = head2; tail = tail2) ->
        if obj.ReferenceEquals(head1, head2) then isSubSequence tail1 smaller else
        match head1, head2 with
        | _, Singleton head2 -> ValueNone
        | SingletonStarLoop(head1), SingletonStarLoop(head2) ->
            if obj.ReferenceEquals(tail1, tail2) then
                ValueSome smaller
            else
                isSubSequence tail1 smaller
        | Singleton spred, SingletonStarLoop(lpred) ->
            if Solver.elemOfSet lpred spred then
                if obj.ReferenceEquals(tail1, smaller) then
                    ValueSome smaller
                else
                    isSubSequence tail1 smaller
            else
                ValueNone
        | Or(nodes= headNodes), SingletonStarLoop(lpred) ->
            if setIsSubsumedSingle headNodes smaller then
                ValueSome smaller
            else
                ValueNone
        | _ ->
            ValueNone
    | Or(nodes = nodes), Concat(head = head2; tail = tail2) ->
        if setIsSubsumedSingle nodes smaller then
            ValueSome smaller
        else
            ValueNone

    | _, n when n.IsAlwaysNullable -> ValueSome smaller
    // | _, Epsilon -> ValueSome (smaller)
    | Loop _, _
    | Or _, Concat _
    | Or _, Or _
    | Not _, _
    | _, Not  _
    | Singleton _, _
     -> ValueNone
    | _ ->
        // let dbg = 1
        // failwith $"todo!:\n{bigger.ToStringHelper()}\n{smaller.ToStringHelper()}"
        ValueNone


let rec setIsSubsumed  (bigger: RegexNode<TSet> seq) (smaller: RegexNode<TSet> seq) : bool  =
    use mutable eSmaller = smaller.GetEnumerator()
    use mutable eBigger = bigger.GetEnumerator()
    let mutable found = false
    while eSmaller.MoveNext() do
        while not found && eBigger.MoveNext() do
            match isSubSequence eBigger.Current eSmaller.Current   with
            | ValueSome v ->
                found <- true
            | _ -> ()
    found


let rec setIsSubsumedSingle (bigger: RegexNode<TSet> seq) (smaller: RegexNode<TSet>) : bool  =
    use mutable eBigger = bigger.GetEnumerator()
    let mutable found = false
    while not found && eBigger.MoveNext() do
        match isSubSequence eBigger.Current smaller with
        | ValueSome v ->
            found <- true
        | _ -> ()
    found


[<return: Struct>]
let (|TrySubsumeSameTail|_|) (nodes: HashSet<RegexNode<TSet>>) =
    let arr =
        nodes
        |> Seq.toArray
    match arr with
    | [| e1; e2 |] ->
        match isSubSequence e1 e2 with
        | ValueSome s -> ValueSome(s)
        | _ ->
            match isSubSequence e2 e1 with
            | ValueSome s -> ValueSome s
            | _ -> ValueNone
    | _ ->
        // TODO:
        // failwith "todo optimize"
        ValueNone








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
    // System.Runtime.CompilerServices.RuntimeHelpers.Equals(x,y)
    // obj.ReferenceEquals(x, y)
    LanguagePrimitives.PhysicalEquality x y
