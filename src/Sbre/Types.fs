namespace rec Sbre.Types

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open FSharp.Data.Adaptive
open Sbre
open System.Diagnostics


/// 2. Preliminaries
/// A location in s is a pair ⟨s, i⟩, where −1 ≤ i ≤ |s |
[<DebuggerDisplay("{DebugDisplay()}")>]
[<Struct>]
type Location = {
    Input: string
    mutable Position: int32
    Reversed: bool
}
#if DEBUG
    with
    member this.DebugDisplay() =
        // display entire input if it is short
        if this.Input.Length < 60 then
            let inserted = this.Input.Insert(this.Position, "|")
            $"%s{this.Input[this.Position].ToString()}, %s{inserted}, %i{this.Position}"
        else
            $"%s{this.Input[this.Position].ToString()}, %i{this.Position}"

#endif

[<System.Flags>]
type RegexNodeFlags =
    | None = 0uy
    | CanBeNullable = 1uy
    | IsAlwaysNullable = 2uy
    | ContainsLookaround = 4uy
    | ContainsEpsilon = 8uy
    | CanSkip = 16uy
    | All = 31uy


[<Struct>]
type RegexNodeInfo<'tset> = {
    Flags: RegexNodeFlags
    Startset: 'tset
} with
    member this.IsAlwaysNullable = this.Flags.HasFlag(RegexNodeFlags.IsAlwaysNullable)
    member this.CanBeNullable = this.Flags.HasFlag(RegexNodeFlags.CanBeNullable)
    member this.ContainsLookaround = this.Flags.HasFlag(RegexNodeFlags.ContainsLookaround)


// TBD: experimenting with various other sets
type NodeSet<'tset when 'tset :> IEquatable<'tset> and 'tset: equality > = ImmutableHashSet<RegexNode<'tset>>

[<DebuggerDisplay("{ToStringHelper()}")>]
[<ReferenceEquality>]
type RegexNode<'tset when 'tset :> IEquatable<'tset> and 'tset: equality> =
    // ------------
    | Concat of  // RE.RE
        head: RegexNode<'tset> *
        tail: RegexNode<'tset> *
        info: RegexNodeInfo<'tset>
    | Epsilon // ε
    | Or of  // RE|RE
        nodes: NodeSet<'tset> *
        info: RegexNodeInfo<'tset>
    | Singleton of set: 'tset // 𝜓 predicate
    | Loop of  // RE{𝑚, 𝑛}
        node: RegexNode<'tset> *
        low: int *
        up: int *
        info: RegexNodeInfo<'tset>
    | And of  // RE&RE ..
        nodes: NodeSet<'tset> *
        info: RegexNodeInfo<'tset>
    | Not of
        node: RegexNode<'tset> *  // ~RE
        info: RegexNodeInfo<'tset>
    | LookAround of
        node: RegexNode<'tset> *  // anchors
        lookBack: bool *
        negate: bool


#if DEBUG
    member this.StartsetPretty(solver: ISolver<_>, css: CharSetSolver) =
        match this with
        | Or(info = info) -> solver.PrettyPrint(info.Startset, css)
        | Singleton pred -> solver.PrettyPrint(pred, css)
        | Loop(info = info) -> solver.PrettyPrint(info.Startset, css)
        | And(info = info) -> solver.PrettyPrint(info.Startset, css)
        | Not(info = info) -> solver.PrettyPrint(info.Startset, css)
        | LookAround _ -> "FULL"
        | Concat(info = info) -> solver.PrettyPrint(info.Startset, css)
        | Epsilon -> "FULL"
#endif


    member inline this.Startset =
        match this with
        | Or(info = info) -> info.Startset
        | Singleton pred -> pred
        | Loop(info = info) -> info.Startset
        | And(info = info) -> info.Startset
        | Not(info = info) -> info.Startset
        | LookAround _ -> Unchecked.defaultof<_>
        | Concat(info = info) -> info.Startset
        | Epsilon -> Unchecked.defaultof<_>

    member inline this.ContainsLookaround =
        match this with
        | Or(info = info) -> info.ContainsLookaround
        | Singleton _ -> false
        | Loop(info = info) -> info.ContainsLookaround
        | And(info = info) -> info.ContainsLookaround
        | Not(info = info) -> info.ContainsLookaround
        | LookAround _ -> true
        | Concat(info = info) -> info.ContainsLookaround
        | Epsilon -> false

    member inline this.IsAlwaysNullable =
        match this with
        | Or(info = info) -> info.IsAlwaysNullable
        | Singleton _ -> false
        | Loop(info = info) -> info.IsAlwaysNullable
        | And(info = info) -> info.IsAlwaysNullable
        | Not(info = info) -> info.IsAlwaysNullable
        | LookAround _ -> true
        | Concat(info = info) -> info.IsAlwaysNullable
        | Epsilon -> false

    member inline this.CanNotBeNullable =
        match this with
        | Or(info = info) -> not (info.CanBeNullable)
        | Singleton _ -> true
        | Loop(info = info) -> not (info.CanBeNullable)
        | And(info = info) -> not (info.CanBeNullable)
        | Not(info = info) -> not (info.CanBeNullable)
        | LookAround _ -> false
        | Concat(info = info) -> not (info.CanBeNullable)
        | Epsilon -> false


#if DEBUG



    override this.ToString() : string =
        match this with
        | Or(xs, _) -> $"Or({xs})"
        | Singleton _ -> this.ToStringHelper()
        | Loop _ -> this.ToStringHelper()
        | And _ -> this.ToStringHelper()
        | Not _ -> this.ToStringHelper()
        | LookAround _ -> this.ToStringHelper()
        | Concat(h, t, info) -> h.ToStringHelper() + t.ToStringHelper()
        | Epsilon -> "ε"

    member this.TagName() =
        match this with
        | Or _ -> "Or"
        | Singleton _ -> "φ"
        | Loop _ -> "Loop"
        | And _ -> "And"
        | Not _ -> "Not"
        | LookAround _ -> "Look"
        | Concat _ -> "Concat"
        | Epsilon -> "ε"

    member this.debuggerSolver =
        match Common.debuggerSolver with
        | None -> failwith "debugger solver not initialized"
        | Some solver -> solver

    member this.isFull(node: RegexNode<'t>) =
        match node with
        | Singleton v ->
            match box v with
            | :? System.Text.RuntimeRegexCopy.Symbolic.BDD as v -> v = debugcharSetSolver.Full
            | :? uint64 -> box v = this.debuggerSolver.Full
            | _ -> false
        | _ -> false

    /// used to display the node during debugging
    member this.ToStringHelper() =
        let display(nodes: RegexNode<'tset>) = nodes.ToStringHelper()
        let asString(nodes: RegexNode<'tset>) = nodes.ToStringHelper()
        let paren str = $"({str})"

        let tostr(v: 'tset) =
            match debuggerSolver with
            | None ->
                match box v with
                | :? System.Text.RuntimeRegexCopy.Symbolic.BDD as v ->
                    debugcharSetSolver.PrettyPrint(v)
                | _ -> $"{v}L"
            | Some db ->
                match box v with
                | :? BDD as v ->
                    if v = debugcharSetSolver.Full then
                        "⊤"
                    elif debugcharSetSolver.IsEmpty(unbox v) then
                        "⊥"
                    else
                        match debugcharSetSolver.PrettyPrint(v) with
                        | @"[^\n]" -> "."
                        | c when c.Length > 12 -> "φ" // dont expand massive sets
                        | c -> c
                | _ ->
                    if unbox v = db.Full then
                        "⊤"
                    elif db.IsEmpty(unbox v) then
                        "⊥"
                    else
                        match db.PrettyPrint(unbox (box v), debugcharSetSolver) with
                        | @"[^\n]" -> "."
                        | c when c.Length > 12 -> "φ" // dont expand massive sets
                        | c -> c

        match this with
        | Singleton v -> tostr v
        | Or(items, _) ->
            let setItems: string list =
                if not (obj.ReferenceEquals(items, null)) then
                    items |> Seq.map display |> Seq.toList
                else
                    []

            let combinedList = setItems

            combinedList |> String.concat "|" |> paren
        | And(items, _) ->
            let setItems: string list =
                if not (obj.ReferenceEquals(items, null)) then
                    items |> Seq.map display |> Seq.toList
                else
                    []

            setItems |> String.concat "&" |> paren
        | Not(items, info) ->
            let inner = items.ToStringHelper()

            $"~({inner})"
        | Loop(body, lower, upper, info) ->
            let inner = body.ToStringHelper()

            let isStar = lower = 0 && upper = Int32.MaxValue

            let inner = $"{inner}"

            let loopCount =
                if isStar then "*"
                elif lower = 1 && upper = Int32.MaxValue then "+"
                elif lower = 0 && upper = 1 then "?"
                else $"{{{lower},{upper}}}"

            match isStar with
            | true -> $"{inner}*"
            | false -> inner + loopCount

        | LookAround(body, lookBack, negate) ->
            let inner = body.ToStringHelper()

            match lookBack, negate with
            // | true, true when this.isFull body.Head -> "\\A"
            // | false, true when this.isFull body.Head -> "\\z"
            | false, true -> $"(?!{inner})"
            | false, false -> $"(?={inner})"
            | true, true -> $"(?<!{inner})"
            | true, false -> $"(?<={inner})"

        | Concat(h, t, info) -> h.ToStringHelper() + t.ToStringHelper()
        | Epsilon -> "ε"



#endif


[<Flags>]
type StartsetFlags =
    | None = 0uy
    | IsFull = 1uy
    | IsEmpty = 2uy
    | Inverted = 4uy

// [<Struct>]
type PredStartset = {
    Flags: StartsetFlags
    Chars: char[]
} with

    static member Of(inverted, startset) = { Flags = inverted; Chars = startset }

// todo: this could be optimized
[<Sealed>]
type ToplevelORCollection() =
    let lastNullableArray: ResizeArray<int> = ResizeArray()
    let nodeArray: ResizeArray<RegexNode<uint64>> = ResizeArray()

    member this.Add(node: RegexNode<uint64>, nullableState: int) =
        nodeArray.Add(node)
        lastNullableArray.Add(nullableState)

    member this.UpdateTransition
        (
            oldNode: RegexNode<uint64>,
            node: RegexNode<uint64>,
            nullableState: int
        ) =
        match nodeArray.Count with
        | 1 ->
            nodeArray[0] <- node
            lastNullableArray[0] <- nullableState
        | _ ->
            let oldIndex = nodeArray.IndexOf(oldNode)
            // todo: cannot do this: (top level duplicate test)
            // let existingIndex = nodeArray.IndexOf(node)
            // if existingIndex <> -1 then
            //     if nullableState = -1 then ()
            //     else lastNullableArray[existingIndex] <- nullableState
            //     nodeArray.RemoveAt(oldIndex)
            //     lastNullableArray.RemoveAt(oldIndex)
            // else
            nodeArray[oldIndex] <- node
            lastNullableArray[oldIndex] <- nullableState

    member this.BumpIsAlwaysNullable(oldNode: RegexNode<uint64>, nullableState: int) =
        let oldIndex = nodeArray.IndexOf(oldNode)
        lastNullableArray[oldIndex] <- nullableState

    // todo: generalize to clear duplicates
    member this.MergeIndexes(idx1: int, idx2:int) =
        let maxNullable =
            max lastNullableArray[idx1] lastNullableArray[idx2]
        lastNullableArray[idx1] <- maxNullable
        nodeArray.RemoveAt(idx2)
        lastNullableArray.RemoveAt(idx2)

    member this.Remove(oldNode: RegexNode<uint64>) =
        // match nodeArray.Count with
        // | 1 ->
        //     nodeArray.Clear()
        //     lastNullableArray.Clear()
        // | _ ->
        let oldIndex = nodeArray.IndexOf(oldNode)
        nodeArray.RemoveAt(oldIndex)
        lastNullableArray.RemoveAt(oldIndex)

    member this.GetLastNullPos(node: RegexNode<uint64>) =
        let idx = nodeArray.IndexOf(node)
        lastNullableArray[idx]

    member this.Count = nodeArray.Count

    member this.Items = nodeArray



[<AutoOpen>]
module Common =

    //
    let inline head coll = Seq.head coll
    let inline tail coll = List.tail coll
    let inline iter f coll = Seq.iter f coll
    let inline seqforall f coll = Seq.forall f coll

    [<return: Struct>]
    let inline (|Empty|_|)(node: Set<'tset>) =
        match node.IsEmpty with
        | true -> ValueSome()
        | _ -> ValueNone


    let equalityComparer =
        { new IEqualityComparer<RegexNode<_>> with
            member this.Equals(x, y) = obj.ReferenceEquals(x, y)
            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }


    // TBD: other implementations
    let inline singleton item =
        ImmutableHashSet.Create(equalityComparer, item = item)

    let inline of2(x, y) =
        ImmutableHashSet.CreateRange(
            equalityComparer,
            seq {
                yield x
                yield y
            }
        )

    let inline ofSeq coll =
        ImmutableHashSet.CreateRange(equalityComparer, coll)

    let inline filter f (coll: ImmutableHashSet<RegexNode<'t>>) =
        ImmutableHashSet.CreateRange(equalityComparer, Seq.filter f coll)

    let inline map f (coll: ImmutableHashSet<RegexNode<'t>>) =
        ImmutableHashSet.CreateRange(equalityComparer, Seq.map f coll)
    // todo inline
    let setequals(coll1: ImmutableHashSet<RegexNode<_>>, coll2: ImmutableHashSet<RegexNode<_>>) =
        coll1.SetEquals(coll2)

    let inline exists f (coll: ImmutableHashSet<'t>) = coll |> Seq.exists f
    let inline forall f (coll: ImmutableHashSet<'t>) = coll |> Seq.forall f
