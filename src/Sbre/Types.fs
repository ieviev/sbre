namespace rec Sbre.Types

open System
open System.Buffers
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open System.Diagnostics

// module Constants =
//     let [<Literal>] COUNTING_SET_THRESHOLD = 2


#if DEBUG
[<AutoOpen>]
module Debug =
    let debugcharSetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()
    let bddBuilder = SymbolicRegexBuilder<BDD>(debugcharSetSolver, debugcharSetSolver)


#endif



/// A location in s is a pair ⟨s, i⟩, where −1 ≤ i ≤ |s |
[<DebuggerDisplay("{DebugDisplay()}")>]
[<Struct; IsByRefLike>]
type Location = {
    Input: ReadOnlySpan<char>
    mutable Position: int32
    mutable Reversed: bool
}

#if DEBUG
    with
    member this.DebugDisplay() =
        if
            // display entire input if it is short
            this.Input.Length < 60
        then
            if this.Position = this.Input.Length then
                this.Input.ToString() + "|"
            else
                let inserted = this.Input.ToString().Insert(this.Position, "|")
                $"%s{this.Input[this.Position].ToString()}, %s{inserted}, %i{this.Position}"
        else
            $"%c{this.Input[this.Position]}, %i{this.Position}"

#endif

[<Struct;>]
type InitialStartset<'t> =
    | Uninitialized
    | Unoptimized
    | MintermArrayPrefix of prefix: Memory<'t> * loopTerminator: Memory<'t>


[<Flags>]
type RegexNodeFlags =
    | None = 0uy
    | CanBeNullableFlag = 1uy
    | IsAlwaysNullableFlag = 2uy
    | HasZerowidthHeadFlag = 64uy
    | ContainsLookaroundFlag = 4uy
    // | ContainsEpsilonFlag = 8uy
    | HasCounterFlag = 16uy
    | DependsOnAnchorFlag = 32uy
    // | IsImmediateLookaroundFlag = 64uy
    | IsAnchorFlag = 128uy


[<AutoOpen>]
module RegexNodeFlagsExtensions =
    type RegexNodeFlags with
        member this.IsAlwaysNullable = byte (this &&& RegexNodeFlags.IsAlwaysNullableFlag) <> 0uy
        member this.HasZerowidthHead = byte (this &&& RegexNodeFlags.HasZerowidthHeadFlag) <> 0uy
        member this.CanBeNullable = byte (this &&& RegexNodeFlags.CanBeNullableFlag) <> 0uy
        member this.ContainsLookaround = byte (this &&& RegexNodeFlags.ContainsLookaroundFlag) <> 0uy
        member this.HasCounter = (this &&& RegexNodeFlags.HasCounterFlag) = RegexNodeFlags.HasCounterFlag
        member this.DependsOnAnchor = (this &&& RegexNodeFlags.DependsOnAnchorFlag) = RegexNodeFlags.DependsOnAnchorFlag


//
[<Flags>]
type RegexStateFlags =
    | None = 0
    | InitialFlag = 1
    | DeadendFlag = 2
    | CanBeNullableFlag = 4
    | CanSkipFlag = 8
    | HasPrefixFlag = 16
    | ContainsLookaroundFlag = 32
    | HasCounterFlag = 64
    | UseDotnetOptimizations = 128
    | ContainsInitialFlag = 256
    | AlwaysNullableFlag = 512
    | ActiveBranchOptimizations = 1024
    | IsPendingNullableFlag = 2048
    // | IsRelativeNegatedNullableFlag = 4096
    // | CanHaveMultipleNullables = 8192
    | DependsOnAnchor = 16384
    // todo: fixed length
    // todo: can be subsumed
    // todo: singleton loop

[<AutoOpen>]
module RegexStateFlagsExtensions =
    type Sbre.Types.RegexStateFlags with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CanSkipInitial =
            this &&& (RegexStateFlags.InitialFlag ||| RegexStateFlags.DependsOnAnchor) = RegexStateFlags.InitialFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.IsDeadend =
            this &&& RegexStateFlags.DeadendFlag = RegexStateFlags.DeadendFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.IsAlwaysNullable = this &&& RegexStateFlags.AlwaysNullableFlag = RegexStateFlags.AlwaysNullableFlag

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CannotBeCached = this &&& (
            // RegexStateFlags.ContainsLookaroundFlag |||
            RegexStateFlags.DependsOnAnchor) <> RegexStateFlags.None

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CanBeNullable = this &&& RegexStateFlags.CanBeNullableFlag = RegexStateFlags.CanBeNullableFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ContainsLookaround = this &&& RegexStateFlags.ContainsLookaroundFlag = RegexStateFlags.ContainsLookaroundFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CanSkip = this &&& (
            RegexStateFlags.CanSkipFlag |||
            RegexStateFlags.InitialFlag |||
            RegexStateFlags.CanBeNullableFlag) = RegexStateFlags.CanSkipFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.HasPrefix = this &&& RegexStateFlags.HasPrefixFlag = RegexStateFlags.HasPrefixFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.IsPendingNullable = this &&& RegexStateFlags.IsPendingNullableFlag = RegexStateFlags.IsPendingNullableFlag
        // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        // member this.HasCounter = this &&& RegexStateFlags.HasCounterFlag = RegexStateFlags.HasCounterFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ContainsInitial = this &&& RegexStateFlags.ContainsInitialFlag = RegexStateFlags.ContainsInitialFlag



type Transition<'tset when 'tset :> IEquatable<'tset> and 'tset: equality > = {
    mutable Set : 'tset
    Node : RegexNode<'tset>
}

[<Sealed>]
type RegexNodeInfo<'tset when 'tset :> IEquatable<'tset> and 'tset: equality >() =

    member val NodeFlags: RegexNodeFlags = RegexNodeFlags.None with get, set
    // member val InitialStartset: InitialStartset<'tset> = InitialStartset.Uninitialized with get, set
    member val Transitions: Dictionary<'tset,RegexNode<'tset>> = Dictionary() with get, set
    member val EndTransitions: Dictionary<'tset,RegexNode<'tset>> = Dictionary() with get, set
    member val StartTransitions: Dictionary<'tset,RegexNode<'tset>> = Dictionary() with get, set
    member val Subsumes: Dictionary<RegexNode<'tset>,bool> = Dictionary() with get, set
    member val PendingNullables: Set<int> = Set.empty with get, set
    // todo: subsumedbyset

    // filled in later
    member val LookupPrev: bool = false with get, set
    member val MustStartWithWordBorder: bool option = None with get, set
    member val PrevCharRequired: 'tset option = None with get, set
    member val Minterms: 'tset = Unchecked.defaultof<'tset> with get, set
    member val Startset: 'tset = Unchecked.defaultof<'tset> with get, set
    member val StateFlags: 'tset = Unchecked.defaultof<'tset> with get, set

    member inline this.IsAlwaysNullable =
        this.NodeFlags &&& RegexNodeFlags.IsAlwaysNullableFlag = RegexNodeFlags.IsAlwaysNullableFlag

    member inline this.HasZerowidthHead =
        this.NodeFlags &&& RegexNodeFlags.HasZerowidthHeadFlag = RegexNodeFlags.HasZerowidthHeadFlag
        // (this.Flags &&& RegexNodeFlags.IsAlwaysNullable) <> RegexNodeFlags.None
    member inline this.CanBeNullable =
        this.NodeFlags &&& RegexNodeFlags.CanBeNullableFlag = RegexNodeFlags.CanBeNullableFlag

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CanNotBeNullable() =
         (this.NodeFlags &&& RegexNodeFlags.CanBeNullableFlag) = RegexNodeFlags.None
    member inline this.ContainsLookaround =
        this.NodeFlags &&& RegexNodeFlags.ContainsLookaroundFlag = RegexNodeFlags.ContainsLookaroundFlag
    // member inline this.DoesNotContainEpsilon =
    //     (this.NodeFlags &&& RegexNodeFlags.ContainsEpsilonFlag) = RegexNodeFlags.None

[<ReferenceEquality>]
type RegexAnchor =
    | End
    | Begin


[<ReferenceEquality>]
[<DebuggerDisplay("{ToString()}")>]
type RegexNode<'tset when 'tset :> IEquatable<'tset> and 'tset: equality> =
    | Concat of  // RE.RE
        head: RegexNode<'tset> *
        tail: RegexNode<'tset> *
        info: RegexNodeInfo<'tset>
    | Epsilon // ε
    | Or of  // RE|RE
        nodes: ImmutableHashSet<RegexNode<'tset>> *
        info: RegexNodeInfo<'tset>
    | Singleton of set: 'tset // 𝜓 predicate
    | Loop of  // RE{𝑚, 𝑛}
        node: RegexNode<'tset> *
        low: int *
        up: int *
        info: RegexNodeInfo<'tset>
    | And of  // RE&RE ..
        nodes: ImmutableHashSet<RegexNode<'tset>> *
        info: RegexNodeInfo<'tset>
    | Not of
        node: RegexNode<'tset> *  // ~RE
        info: RegexNodeInfo<'tset>
    | LookAround of
        node: RegexNode<'tset> *  // anchors
        lookBack: bool *
        relativeTo : int *
        pendingNullables : int list *
        info: RegexNodeInfo<'tset>
    | Anchor of RegexAnchor


#if DEBUG

    override this.ToString() =
        // if Debug.debuggerSolver.IsNone then
        //     Debug.debuggerSolver <- box Debug.debugcharSetSolver

        let print node =
            if typeof<'tset> = typeof<BDD> then
                Debug.debugcharSetSolver.PrettyPrint(unbox (box node))
            else
                Debug.debuggerSolver.Value.PrettyPrint(unbox (box node),debugcharSetSolver)

        let isFull (tset:'tset) =
            if typeof<'tset> = typeof<BDD> then
                Debug.debugcharSetSolver.IsFull(unbox (box tset))
            else Debug.debuggerSolver.Value.IsFull(unbox (box tset))
        let isEmpty (tset:'tset) =
            if typeof<'tset> = typeof<BDD> then
                Debug.debugcharSetSolver.IsEmpty(unbox (box tset))
            else Debug.debuggerSolver.Value.IsEmpty(unbox (box tset))
        // let _solver =
        //     if Debug.debuggerSolver.IsNone then
        //         box Debug.debugcharSetSolver :?> ISolver<'t>
        //     else
        //         (box Debug.debuggerSolver.Value) :?> ISolver<'t>

        let paren str = $"({str})"

        let tostr(v:'tset) =
            if isFull v then
                "⊤"
            elif isEmpty v then
                "⊥"
            else
                match print v with
                | @"[^\n]" -> "."
                | c when c.Length > 12 -> "φ" // dont expand massive sets
                | c -> c

        match this with
        | Singleton v -> tostr v
        | Or(items, _) ->
            let itlen = items.Count
            let isCaret =
                match itlen = 2 with
                | false -> None
                | true ->
                    let items2str = items |> Seq.map (_.ToString()) |> ResizeArray
                    let v1 = items2str.Contains(@"\A")
                    let v2 = items2str.Contains(@"(?<=\n)")
                    if v1 && v2 then Some "^" else None
            let isDollar =
                match itlen = 2 with
                | false -> None
                | true ->
                    let items2str = items |> Seq.map (_.ToString()) |> ResizeArray
                    let v1 = items2str.Contains(@"\z")
                    let v2 = items2str.Contains(@"(?=\n)")
                    if v1 && v2 then Some "$" else None
            match isCaret, isDollar with
            | Some v,_ -> v
            | _,Some v -> v
            | _ ->

            let setItems: string list =
                items |> Seq.map (_.ToString() ) |> Seq.toList
            let combinedList = setItems

            combinedList |> String.concat "|" |> paren
        | And(items, _) ->
            let setItems: string list =
                items |> Seq.map string |> Seq.toList


            setItems |> String.concat "&" |> paren
        | Not(items, info) ->
            let inner = items.ToString()

            $"~({inner})"
        | Loop(body, lower, upper, info) ->
            let inner =  body.ToString()

            let isStar = lower = 0 && upper = Int32.MaxValue

            let inner =
                if inner.Length = 1 then inner else $"({inner})"

            let loopCount =
                if isStar then "*"
                elif lower = 1 && upper = Int32.MaxValue then "+"
                elif lower = 0 && upper = 1 then "?"
                else $"{{{lower},{upper}}}"

            match isStar with
            | true -> $"{inner}*"
            | false -> inner + loopCount

        | LookAround(body, lookBack, relativeTo, pending, _) ->
            let inner = body.ToString()
            let pending =
                if pending.IsEmpty then ""
                else $"%A{pending}"
            match lookBack with
            | false-> $"(?={inner})"
            | true -> $"(?<={inner})"
            + pending

        | Concat(h, t, info) ->
            let body = h.ToString() + t.ToString()
            if info.NodeFlags.HasCounter then
                $"⟨{body}⟩"
            else body
        | Epsilon -> "ε"
        | Anchor regexAnchor ->
            match regexAnchor with
            | End -> @"\z"
            | Begin -> @"\A"

#endif
    member inline this.TryGetInfo =
        match this with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info) | LookAround( info=info ) ->
            ValueSome info
        | _ -> ValueNone


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetFlags() =
        this.TryGetInfo
        |> ValueOption.map (fun v -> v.NodeFlags)
        |> ValueOption.defaultWith (fun _ ->
            match this with
            | Epsilon ->
                RegexNodeFlags.CanBeNullableFlag |||
                RegexNodeFlags.IsAlwaysNullableFlag |||
                RegexNodeFlags.HasZerowidthHeadFlag
            | Singleton _ -> RegexNodeFlags.None
            | Anchor _ ->
                RegexNodeFlags.DependsOnAnchorFlag |||
                RegexNodeFlags.IsAnchorFlag |||
                RegexNodeFlags.CanBeNullableFlag |||
                RegexNodeFlags.HasZerowidthHeadFlag
            | _ -> failwith "impossible case"
        )

    member this.CanBeNullable =
        this.GetFlags().HasFlag(RegexNodeFlags.CanBeNullableFlag)
    member this.CanNotBeNullable =
        not (this.GetFlags().HasFlag(RegexNodeFlags.CanBeNullableFlag))
    member this.ContainsLookaround =
        this.GetFlags().HasFlag(RegexNodeFlags.ContainsLookaroundFlag)


    member this.HasCounter =
        this.GetFlags().HasFlag(RegexNodeFlags.HasCounterFlag)
    member this.DependsOnAnchor =
        this.GetFlags().HasFlag(RegexNodeFlags.DependsOnAnchorFlag)

    member this.PendingNullables =
        this.TryGetInfo
        |> ValueOption.map (fun v -> v.PendingNullables)
        |> ValueOption.defaultWith (fun _ -> Set.empty
        )


    member this.IsAlwaysNullable =
        this.TryGetInfo
        |> ValueOption.map (fun v -> v.IsAlwaysNullable)
        |> ValueOption.defaultWith (fun _ ->
            match this with
            | Singleton _ -> false
            | LookAround _ -> false
            | Epsilon -> true
            | Anchor regexAnchor -> false
            | _ -> failwith "impossible case"
        )

    member this.HasZerowidthHead =
        this.TryGetInfo
        |> ValueOption.map (fun v -> v.HasZerowidthHead)
        |> ValueOption.defaultWith (fun _ ->
            match this with
            | Singleton _ -> false
            | LookAround _ -> false
            | Epsilon -> true
            | Anchor regexAnchor -> false
            | _ -> failwith "impossible case"
        )
    member this.SubsumedByMinterm (solver:ISolver<'tset>) =
        this.TryGetInfo
        |> ValueOption.map (fun v -> v.Minterms)
        |> ValueOption.defaultWith (fun _ ->
            match this with
            | Epsilon -> solver.Full
            | Singleton pred -> pred
            | LookAround(node, lookBack, negate, _, _) -> node.SubsumedByMinterm solver
            | Anchor _ -> solver.Empty
            | _ -> failwith "impossible case"
        )


[<Flags>]
type StartsetFlags =
    | None = 0uy
    | IsFull = 1uy
    | IsEmpty = 2uy
    | Inverted = 4uy
    | TooBig = 8uy

/// collection of concrete startset chars for vectorization purposes
type PredStartset = {
    Flags: StartsetFlags
    Chars: char[]
} with
    static member Of(inverted, startset:char[]) = { Flags = inverted; Chars = startset }

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

    let tsetComparer =
        { new IEqualityComparer<RegexNode<TSet>> with
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
        while not found && e.MoveNext()  do
            found <- f e.Current
        found

    let inline forall ([<InlineIfLambda>] f) (coll: ImmutableHashSet<'t>) =
        use mutable e = coll.GetEnumerator()
        let mutable forall = true
        while forall && e.MoveNext()  do
            forall <- f e.Current
        forall


    let inline tryFindV ([<InlineIfLambda>] f) (coll: seq<_>) =
        use mutable e = coll.GetEnumerator()
        let mutable found = ValueNone
        while found.IsNone  && e.MoveNext() do
            found <- f e.Current
        found

    let inline chooseV ([<InlineIfLambda>] f) (coll: seq<_>) =
        seq {
            use mutable e = coll.GetEnumerator()
            while e.MoveNext() do
                match f e.Current with
                | ValueSome v -> yield v
                | _ -> ()
        }

    let zeroList = [0]




type NodeSet<'tset when 'tset :> IEquatable<'tset> and 'tset: equality> =
    ImmutableHashSet<RegexNode<'tset>>


module Enumerator =
    let inline getSharedHash(e: RegexNode<_>[]) =
        let mutable found = false
        let mutable hash = 0
        for n in e do
            hash <- hash ^^^ LanguagePrimitives.PhysicalHash n
        hash


type TSet = uint64
// type TSet = uint32
// type TSet = uint16

[<Sealed>]
type SharedResizeArray<'t>(initialSize:int) =
    let mutable size = 0
    let mutable limit = initialSize
    let mutable pool : 't array = ArrayPool.Shared.Rent(initialSize)
    member this.Add(item) =
        if size = limit then
            let newLimit = limit * 2
            let newArray = ArrayPool.Shared.Rent(newLimit)
            Array.Copy(pool,newArray,size)
            ArrayPool.Shared.Return(pool)
            pool <- newArray
            limit <- limit * 2
        pool[size] <- item
        size <- size + 1

    member this.Clear() =
        size <- 0
    member this.Contains(item) =
        let mutable e = pool.AsSpan(0, size).GetEnumerator()
        let mutable found = false
        while not found && e.MoveNext() do
            found <- obj.ReferenceEquals(e.Current,item)
        found
    member this.GetEnumerator() =
        let mutable e = pool.AsSpan(0, size).GetEnumerator()
        e
    member this.Length = size
    member this.AsSpan() = pool.AsSpan(0, size)
    member this.AsArray() = pool.AsSpan(0, size).ToArray()

    interface IDisposable with
        member this.Dispose() =
            ArrayPool.Shared.Return(pool)



[<Struct; IsByRefLike; >]
// [<Struct>]
type SharedResizeArrayStruct<'t> =
    val mutable size : int
    val mutable limit : int
    val mutable pool : 't array
    member this.Add(item) =
        if this.size = this.limit then
            let newLimit = this.limit * 2
            let newArray = ArrayPool.Shared.Rent(newLimit)
            Array.Copy(this.pool,newArray,this.size)
            ArrayPool.Shared.Return(this.pool)
            this.pool <- newArray
            this.limit <- this.limit * 2
        this.pool[this.size] <- item
        this.size <- this.size + 1
    member this.Clear() =
        this.size <- 0
    member this.Contains(item) =
        let mutable e = this.pool.AsSpan(0, this.size).GetEnumerator()
        let mutable found = false
        while not found && e.MoveNext() do
            found <- obj.ReferenceEquals(e.Current,item)
        found
    // member this.GetEnumerator() =
    //     let mutable e = this.pool.AsSpan(0, this.size).GetEnumerator()
    //     e
    member this.Length = this.size
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.AsSpan() = this.pool.AsSpan(0, this.size)
    member this.AsArray() = this.pool.AsSpan(0, this.size).ToArray()

    interface IDisposable with
        member this.Dispose() =
            ArrayPool.Shared.Return(this.pool)
    new(initialSize: int) =
        {
            size = 0
            limit = initialSize
            pool = ArrayPool.Shared.Rent(initialSize)
        }



