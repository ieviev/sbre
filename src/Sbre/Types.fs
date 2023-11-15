namespace rec Sbre.Types

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre
open System.Diagnostics

/// A location in s is a pair ⟨s, i⟩, where −1 ≤ i ≤ |s |
[<DebuggerDisplay("{DebugDisplay()}")>]
[<Struct>] // todo: try as reference
type Location = {
    Input: string
    mutable Position: int32
    Reversed: bool
}
#if DEBUG
    with
    member this.DebugDisplay() =
        if this.Position = this.Input.Length then
            this.Input + "|"
        else if
            // display entire input if it is short
            this.Input.Length < 60
        then
            let inserted = this.Input.Insert(this.Position, "|")
            $"%s{this.Input[this.Position].ToString()}, %s{inserted}, %i{this.Position}"
        else
            $"%s{this.Input[this.Position].ToString()}, %i{this.Position}"

#endif

[<Flags>]
type RegexNodeFlags =
    | None = 0uy
    | CanBeNullable = 1uy
    | IsAlwaysNullable = 2uy
    | ContainsLookaround = 4uy
    | ContainsEpsilon = 8uy
    | CanSkip = 16uy
    | Prefix = 32uy

type RegexNodeInfo<'tset> = {
    Flags: RegexNodeFlags
    Startset: 'tset
    mutable InitialStartset: InitialStartset
} with

    member inline this.IsAlwaysNullable =
        (this.Flags &&& RegexNodeFlags.IsAlwaysNullable) <> RegexNodeFlags.None

    member inline this.CanBeNullable =
        (this.Flags &&& RegexNodeFlags.CanBeNullable) <> RegexNodeFlags.None

    member inline this.CanSkip = (this.Flags &&& RegexNodeFlags.CanSkip) <> RegexNodeFlags.None

    member inline this.CanNotBeNullable =
        (this.Flags &&& RegexNodeFlags.IsAlwaysNullable) = RegexNodeFlags.None

    member inline this.ContainsLookaround =
        this.Flags.HasFlag(RegexNodeFlags.ContainsLookaround)

    member inline this.ContainsEpsilon =
        (this.Flags &&& RegexNodeFlags.ContainsEpsilon) = RegexNodeFlags.None

    member inline this.HasPrefix = this.Flags.HasFlag(RegexNodeFlags.Prefix)

type InitialStartset =
    | Uninitialized
    | Unoptimized
    | MintermArrayPrefix of prefix: uint64[] * loopTerminator: uint64[]

type NodeSet<'tset when 'tset :> IEquatable<'tset> and 'tset: equality> =
    ImmutableHashSet<RegexNode<'tset>>

[<DebuggerDisplay("{ToStringHelper()}")>]
[<ReferenceEquality>]
type RegexNode<'tset when 'tset :> IEquatable<'tset> and 'tset: equality> =
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
        | LookAround _ -> "-"
        | Concat(info = info) -> solver.PrettyPrint(info.Startset, css)
        | Epsilon -> "-"
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
        | LookAround _ -> false
        | Concat(info = info) -> info.IsAlwaysNullable
        | Epsilon -> false

    member inline this.ContainsEpsilon =
        match this with
        | Or(info = info) -> info.ContainsEpsilon
        | Singleton _ -> false
        | Loop(info = info) -> info.ContainsEpsilon
        | And(info = info) -> info.ContainsEpsilon
        | Not(info = info) -> info.ContainsEpsilon
        | LookAround _ -> true
        | Concat(info = info) -> info.ContainsEpsilon
        | Epsilon -> false

    member inline this.HasPrefix =
        match this with
        | Or(info = info) -> info.HasPrefix
        | Singleton _ -> false
        | Loop(info = info) -> info.HasPrefix
        | And(info = info) -> info.HasPrefix
        | Not(info = info) -> info.HasPrefix
        | LookAround _ -> true
        | Concat(info = info) -> info.HasPrefix
        | Epsilon -> false

    member inline this.TryGetInfo =
        match this with
        | Or(info = info) -> ValueSome info
        | Singleton _ -> ValueNone
        | Loop(info = info) -> ValueSome info
        | And(info = info) -> ValueSome info
        | Not(info = info) -> ValueSome info
        | LookAround _ -> ValueNone
        | Concat(info = info) -> ValueSome info
        | Epsilon -> ValueNone
    member inline this.CanBeNullable =
        match this with
        | Or(info = info) -> info.CanBeNullable
        | Singleton _ -> false
        | Loop(info = info) -> info.CanBeNullable
        | And(info = info) -> info.CanBeNullable
        | Not(info = info) -> info.CanBeNullable
        | LookAround _ -> true
        | Concat(info = info) -> info.CanBeNullable
        | Epsilon -> true

    member inline this.CanNotBeNullable =
        match this with
        | Or(info = info) -> not info.CanBeNullable
        | Singleton _ -> true
        | Loop(info = info) -> not info.CanBeNullable
        | And(info = info) -> not info.CanBeNullable
        | Not(info = info) -> not info.CanBeNullable
        | LookAround _ -> false
        | Concat(info = info) -> not info.CanBeNullable
        | Epsilon -> false

    member inline this.CanSkip =
        match this with
        | Or(info = info) -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
        | Singleton _ -> false
        | Loop(info = info) -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
        | And(info = info) -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
        | Not(info = info) -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
        | LookAround _ -> false
        | Concat(info = info) -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
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
    member this.debuggerSolver =
        match debuggerSolver with
        | None -> failwith "debugger solver not initialized"
        | Some solver -> solver
    member this.isFull(node: RegexNode<'t>) =
        match node with
        | Singleton v ->
            match box v with
            | :? BDD as v -> v = debugcharSetSolver.Full
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
                | :? BDD as v ->
                    debugcharSetSolver.PrettyPrint(v)
                | _ -> $"{v}L"
            | Some db ->
                match box v with
                | :? BDD as v ->
                    if v = debugcharSetSolver.Full then
                        "⊤"
                    elif debugcharSetSolver.IsEmpty(v) then
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

/// collection of concrete startset chars for vectorization purposes
type PredStartset = {
    Flags: StartsetFlags
    Chars: char[]
} with
    static member Of(inverted, startset) = { Flags = inverted; Chars = startset }

[<Sealed>]
type ToplevelORCollection() =

    [<Literal>]
    let startSize = 2
    let mutable nodeArray: RegexNode<uint64>[] = Array.zeroCreate startSize
    let mutable _count = 0
    let mutable _capacity = startSize
    member this.IncreaseArraySize() =
        let newSize = _capacity * 2
        _capacity <- newSize
        Array.Resize(&nodeArray, newSize)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Add(node: RegexNode<uint64>) =
        if _count = _capacity then
            // does not happen in 99% cases
            this.IncreaseArraySize()
        nodeArray[_count] <- node
        _count <- _count + 1

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.UpdateTransition(i: int, node: RegexNode<uint64>) = nodeArray[i] <- node

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Remove(idx: int) =
        match idx = _count - 1 with
        | true -> _count <- _count - 1 // is last element
        | false ->
            // swap last element into current slot
            nodeArray[idx] <- nodeArray[_count - 1]
            _count <- _count - 1


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Reset() = _count <- 0
    member this.Count = _count

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Items() = nodeArray.AsSpan().Slice(0, _count)
    member this.First = nodeArray[0]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CanSkipAll() =
        let mutable canskip = true
        let mutable e = nodeArray.AsSpan().Slice(0, _count).GetEnumerator()

        while e.MoveNext() = true && canskip do
            canskip <- e.Current.CanSkip

        canskip

[<AutoOpen>]
module Common =
    let inline head coll = Seq.head coll

    let equalityComparer =
        { new IEqualityComparer<RegexNode<_>> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Equals(x, y) = obj.ReferenceEquals(x, y)

            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }
    let inline of2(x, y) =
        ImmutableHashSet.CreateRange(
            equalityComparer,
            seq {
                yield x
                yield y
            }
        )
    let inline ofSeq coll = ImmutableHashSet.CreateRange(equalityComparer, coll)
    let inline map ([<InlineIfLambda>] f) (coll: ImmutableHashSet<RegexNode<'t>>) =
        ImmutableHashSet.CreateRange(equalityComparer, Seq.map f coll)

    let inline exists ([<InlineIfLambda>] f) (coll: ImmutableHashSet<'t>) =
        use mutable e = coll.GetEnumerator()
        let mutable found = false
        while e.MoveNext() && not found do
            found <- f e.Current
        found

    let inline forall ([<InlineIfLambda>] f) (coll: ImmutableHashSet<'t>) =
        use mutable e = coll.GetEnumerator()
        let mutable forall = true
        while e.MoveNext() && forall do
            forall <- f e.Current
        forall


module Enumerator =
    let inline getSharedHash(e: RegexNode<_>[]) =
        let mutable found = false
        let mutable hash = 0
        for n in e do
            hash <- hash ^^^ LanguagePrimitives.PhysicalHash n
        hash
