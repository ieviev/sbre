namespace rec Sbre.Types
open Sbre
open System
open System.Buffers
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open System.Diagnostics

// module Constants =
//     let [<Literal>] COUNTING_SET_THRESHOLD = 2

[<AutoOpen>]
module Static =
    let staticCharSetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()


#if DEBUG
[<AutoOpen>]
module Debug =
    let bddBuilder = SymbolicRegexBuilder<BDD>(staticCharSetSolver, staticCharSetSolver)


#endif

type LocationKind =
    | StartPos = 0uy
    | Center = 1uy
    | EndPos = 2uy

/// A location in s is a pair ⟨s, i⟩, where −1 ≤ i ≤ |s |
[<DebuggerDisplay("{DebugDisplay()}")>]
[<Struct; IsByRefLike>]
type Location = {
    Input: ReadOnlySpan<char>
    mutable Position: int32
    mutable Reversed: bool
}
    with
    member this.Kind =
        match this.Position with
        | 0 -> LocationKind.StartPos
        | n when n = this.Input.Length -> LocationKind.EndPos
        | _ -> LocationKind.Center

#if DEBUG

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
    | DependsOnAnchorFlag = 8uy
    | HasSuffixLookaheadFlag = 16uy
    | HasPrefixLookbehindFlag = 32uy
    // | IsSuffixLookahead = 16uy
    // | ContainsEpsilonFlag = 8uy
    // | HasCounterFlag = 16uy

    // | IsImmediateLookaroundFlag = 64uy
    // | IsAnchorFlag = 128uy


[<AutoOpen>]
module RegexNodeFlagsExtensions =
    type RegexNodeFlags with
        member this.IsAlwaysNullable = byte (this &&& RegexNodeFlags.IsAlwaysNullableFlag) <> 0uy
        member this.HasZerowidthHead = byte (this &&& RegexNodeFlags.HasZerowidthHeadFlag) <> 0uy
        member this.CanBeNullable = byte (this &&& RegexNodeFlags.CanBeNullableFlag) <> 0uy
        member this.ContainsLookaround = byte (this &&& RegexNodeFlags.ContainsLookaroundFlag) <> 0uy
        member this.HasSuffixLookahead = byte (this &&& RegexNodeFlags.HasSuffixLookaheadFlag) <> 0uy
        member this.HasPrefixLookbehind = byte (this &&& RegexNodeFlags.HasPrefixLookbehindFlag) <> 0uy
        // member this.HasCounter = (this &&& RegexNodeFlags.HasCounterFlag) = RegexNodeFlags.HasCounterFlag
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
        member this.IsInitial = this &&& RegexStateFlags.InitialFlag = RegexStateFlags.InitialFlag


        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CannotBeCached = this &&& (
            // RegexStateFlags.ContainsLookaroundFlag |||
            RegexStateFlags.DependsOnAnchor) <> RegexStateFlags.None

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.HasActiveBranchOptimizations = this &&& RegexStateFlags.ActiveBranchOptimizations = RegexStateFlags.ActiveBranchOptimizations

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CanBeNullable = this &&& RegexStateFlags.CanBeNullableFlag = RegexStateFlags.CanBeNullableFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ContainsLookaround = this &&& RegexStateFlags.ContainsLookaroundFlag = RegexStateFlags.ContainsLookaroundFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CanSkip = this &&& (
            RegexStateFlags.CanSkipFlag ||| RegexStateFlags.InitialFlag ) = RegexStateFlags.CanSkipFlag

        member this.CanSkipLeftToRight = this &&& (RegexStateFlags.CanSkipFlag) = RegexStateFlags.CanSkipFlag

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
    member val Transitions: Dictionary<'tset,RegexNode<'tset>> = Dictionary() with get, set
    member val EndTransitions: Dictionary<'tset,RegexNode<'tset>> = Dictionary() with get, set
    member val StartTransitions: Dictionary<'tset,RegexNode<'tset>> = Dictionary() with get, set
    member val Subsumes: Dictionary<RegexNode<'tset>,bool> = Dictionary() with get, set
    member val PendingNullables: RefSet<int> = RefSet.Create(Set.empty) with get, set

    // filled in later
    member val IsCanonical: bool = false with get, set
    member val HasCanonicalForm: RegexNode<'tset> option = None with get, set
    member val Minterms: 'tset = Unchecked.defaultof<'tset> with get, set
    member val Startset: 'tset = Unchecked.defaultof<'tset> with get, set
    member inline this.IsAlwaysNullable =
        this.NodeFlags &&& RegexNodeFlags.IsAlwaysNullableFlag = RegexNodeFlags.IsAlwaysNullableFlag
    member inline this.HasZerowidthHead =
        this.NodeFlags &&& RegexNodeFlags.HasZerowidthHeadFlag = RegexNodeFlags.HasZerowidthHeadFlag
    member inline this.CanBeNullable =
        this.NodeFlags &&& RegexNodeFlags.CanBeNullableFlag = RegexNodeFlags.CanBeNullableFlag

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CanNotBeNullable() =
         (this.NodeFlags &&& RegexNodeFlags.CanBeNullableFlag) = RegexNodeFlags.None
    member inline this.ContainsLookaround =
        this.NodeFlags &&& RegexNodeFlags.ContainsLookaroundFlag = RegexNodeFlags.ContainsLookaroundFlag


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
        pendingNullables : RefSet<int> *
        info: RegexNodeInfo<'tset>
    | Begin
    | End



    override this.ToString() =

        let maxwidth : int = 50
        let print node =
            if typeof<'tset> = typeof<BDD> then
                Static.staticCharSetSolver.PrettyPrint(unbox (box node))
            else
#if RELEASE
                "φ"
#else
                Debug.debuggerSolver.Value.PrettyPrint(unbox (box node),Static.staticCharSetSolver)
#endif

        let isFull (tset:'tset) =
            if typeof<'tset> = typeof<BDD> then
                Static.staticCharSetSolver.IsFull(unbox (box tset))
            else Debug.debuggerSolver.Value.IsFull(unbox (box tset))
        let isEmpty (tset:'tset) =
            if typeof<'tset> = typeof<BDD> then
                Static.staticCharSetSolver.IsEmpty(unbox (box tset))
            else Debug.debuggerSolver.Value.IsEmpty(unbox (box tset))

        let paren str = $"({str})"

        let tostr(v:'tset) =
            if isFull v then
                "⊤"
            elif isEmpty v then
                "⊥"
            else
                match print v with
                | @"[^\n]" -> "."
                | c when c.Length > maxwidth -> "φ" // dont expand massive sets
                | c -> c

        match this with
        | Singleton v -> tostr v
        | Or(items, _) ->
            let itlen = items.Count

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
            match body with
            | Or(nodes=nodes) ->
                let isCaret =
                    match nodes.Count = 2 with
                    | true when lookBack ->
                        let items2str = nodes |> Seq.map (_.ToString()) |> ResizeArray
                        let v1 = items2str.Contains(@"\A")
                        let v2 = items2str.Contains(@"\n")
                        if v1 && v2 then Some "^" else None
                    | _ -> None
                let isDollar =
                    match nodes.Count = 2 with
                    | true when not lookBack ->
                        let items2str = nodes |> Seq.map (_.ToString()) |> ResizeArray
                        let v1 = items2str.Contains(@"\z")
                        let v2 = items2str.Contains(@"\n")
                        if v1 && v2 then Some "$" else None
                    | _ -> None
                match isCaret, isDollar with
                | Some s, _ | _, Some s -> s
                | _ ->


                let inner = body.ToString()
                let pending =
                    if pending.inner.IsEmpty then ""
                    else $"%A{Seq.toList pending.inner}"
                match lookBack with
                | false-> $"(?={inner})"
                | true -> $"(?<={inner})"
                + pending
            | _ ->

            let inner = body.ToString()
            let pending =
                if pending.inner.IsEmpty then ""
                else $"%A{Seq.toList pending.inner}"
            match lookBack with
            | false-> $"(?={inner})"
            | true -> $"(?<={inner})"
            + pending
        | Concat(h, t, info) -> $"{h.ToString()}{t.ToString()}"
        | Epsilon -> "ε"
        | End -> @"\z"
        | Begin -> @"\A"

    member inline this.TryGetInfo =
        match this with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info) | LookAround( info=info ) ->
            ValueSome info
        | _ -> ValueNone


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetFlags() =
        this.TryGetInfo
        |> ValueOption.map (_.NodeFlags)
        |> ValueOption.defaultWith (fun _ -> this.GetDefaultFlags() )

    member this.GetDefaultFlags() =
        match this with
        | Epsilon ->
            RegexNodeFlags.CanBeNullableFlag |||
            RegexNodeFlags.IsAlwaysNullableFlag |||
            RegexNodeFlags.HasZerowidthHeadFlag
        | Singleton _ -> RegexNodeFlags.None
        | Begin | End ->
            RegexNodeFlags.DependsOnAnchorFlag |||
            RegexNodeFlags.CanBeNullableFlag |||
            RegexNodeFlags.HasZerowidthHeadFlag
        | _ -> failwith "impossible case"


    member this.CanBeNullable = this.GetFlags().CanBeNullable
    member this.CanNotBeNullable = not (this.GetFlags().CanBeNullable)
    member this.ContainsLookaround = this.GetFlags().ContainsLookaround
    member this.HasPrefixOrSuffix =
        let f = this.GetFlags()
        f &&& (RegexNodeFlags.HasPrefixLookbehindFlag ||| RegexNodeFlags.HasSuffixLookaheadFlag) <> RegexNodeFlags.None

    member this.HasPrefix =
        let f = this.GetFlags()
        f &&& (RegexNodeFlags.HasPrefixLookbehindFlag) <> RegexNodeFlags.None

    member this.DependsOnAnchor = this.GetFlags().DependsOnAnchor

    member this.PendingNullables =
        match this with
        | LookAround(regexNode, lookBack, relativeTo, pendingNullables, regexNodeInfo) ->
            if regexNode.CanNotBeNullable then RefSet.empty else
            pendingNullables.inner
            |> Set.map (fun v -> v + relativeTo)
            |> RefSet.Create
        | _ ->
            this.TryGetInfo
            |> ValueOption.map (_.PendingNullables)
            |> ValueOption.defaultWith (fun _ -> RefSet.empty )


    member this.IsAlwaysNullable =
        this.TryGetInfo
        |> ValueOption.map (fun v -> v.IsAlwaysNullable)
        |> ValueOption.defaultWith (fun _ ->
            match this with
            | Singleton _ -> false
            | LookAround _ -> false
            | Epsilon -> true
            | Begin | End -> false
            | _ -> failwith "impossible case"
        )

    member this.HasZerowidthHead =
        this.TryGetInfo
        |> ValueOption.map (fun v -> v.HasZerowidthHead)
        |> ValueOption.defaultWith (fun _ ->
            match this with
            | Singleton _ -> false
            | LookAround _ -> true
            | Epsilon -> true
            | Begin | End -> false
            | _ -> failwith "impossible case"
        )
    member this.SubsumedByMinterm (solver:ISolver<'tset>) =
        this.TryGetInfo
        |> ValueOption.map (fun v -> v.Minterms)
        |> ValueOption.defaultWith (fun _ ->
            match this with
            | Epsilon -> solver.Full
            | Singleton pred -> pred
            | LookAround(node, _, _, _, _) -> node.SubsumedByMinterm solver
            | Begin | End -> solver.Full
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
        { new IEqualityComparer<RegexNode<'t>> with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Equals(x, y) = obj.ReferenceEquals(x, y)

            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
        }
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

    // let zeroList = [0]
    // let zeroList = Set.singleton 0


    let physComparison =
        Comparison<'T>(fun a b -> (LanguagePrimitives.PhysicalHash a).CompareTo(LanguagePrimitives.PhysicalHash b) )




type NodeSet<'tset when 'tset :> IEquatable<'tset> and 'tset: equality> =
    ImmutableHashSet<RegexNode<'tset>>


module Enumerator =
    let inline getSharedHash(e: RegexNode<_>[]) =
        let mutable hash = 0
        for n in e do
            hash <- hash ^^^ LanguagePrimitives.PhysicalHash n
        hash

    let inline getSharedHash2(e: RegexNode<_>Memory) =
        let mutable hash = 0
        let span = e.Span
        for n in span do
            hash <- hash ^^^ LanguagePrimitives.PhysicalHash n
        hash

    let inline getSharedHash3(e: RegexNode<_>Span) =
        let mutable hash = 0
        for n in e do
            hash <- hash ^^^ LanguagePrimitives.PhysicalHash n
        hash


module Memory =
    let inline forall ([<InlineIfLambda>] f) (mem: Memory<'t>) =
        let span = mem.Span
        let mutable e = span.GetEnumerator()
        let mutable forall = true
        while forall && e.MoveNext()  do
            forall <- f e.Current
        forall


//
// type TSet = BitVector
// type TSolver = ISolver<TSet>

// type TSolver = BitVectorSolver

//
// type TSet = BitVector
// type TSolver = BitVectorSolver

type TSet = uint64
type TSolver = UInt64Solver

// type TSet = uint32
// type TSolver = UInt32Solver

// type TSet = uint16
// type TSet = byte

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
    member this.Exists(lambda) =
        let mutable e = pool.AsSpan(0, size).GetEnumerator()
        let mutable found = false
        while not found && e.MoveNext() do
            found <- lambda e.Current
        found
    member this.AsSpan() = pool.AsSpan(0, size)
    member this.AsArray() = pool.AsSpan(0, size).ToArray()

    interface IDisposable with
        member this.Dispose() =
            ArrayPool.Shared.Return(pool)



[<Struct; IsByRefLike; >]
type SharedResizeArrayStruct<'t> =
    val mutable size : int
    val mutable limit : int
    val mutable pool : 't array

    // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Add(item) =
        if this.size = this.limit then this.Grow()
        this.pool[this.size] <- item
        this.size <- this.size + 1

    member this.Grow() =
        let newLimit = this.limit * 2
        let newArray = ArrayPool.Shared.Rent(newLimit)
        Array.Copy(this.pool,newArray,this.size)
        ArrayPool.Shared.Return(this.pool)
        this.pool <- newArray
        this.limit <- this.limit * 2
    member this.Clear() =
        this.size <- 0
    member this.Contains(item) =
        let mutable e = this.pool.AsSpan(0, this.size).GetEnumerator()
        let mutable found = false
        while not found && e.MoveNext() do
            found <- obj.ReferenceEquals(e.Current,item)
        found
    member this.GetEnumerator() =
        let mutable e = this.pool.AsSpan(0, this.size).GetEnumerator()
        e
    member this.Length = this.size

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.AsSpan() = this.pool.AsSpan(0, this.size)
    member this.AsArray() = this.pool.AsSpan(0, this.size).ToArray()

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Dispose() =
        ArrayPool.Shared.Return(this.pool)

    interface IDisposable with
        member this.Dispose() = this.Dispose()
    new(initialSize: int) =
        {
            size = 0
            limit = initialSize
            pool = ArrayPool.Shared.Rent(initialSize)
        }

//
// type RefSet<'t when 't : comparison > =
//     static let cache = Dictionary<Set<'t>, RefSet<'t>>()
//     static member Create(source:Set<'t>) =
//         // ..
//         match cache.TryGetValue(source) with
//         | true, v -> v
//         | _ ->
//             let rs = RefSet(source)
//             rs
//
//
//     member val Set = Set.empty
//
//     new(source:Set<'t>) = RefSet(Set=Set.empty)
//

/// set with canonical reference comparisons
type RefSet<'t when 't : comparison> =
    static let cache = Dictionary<Set<'t>, RefSet<'t>>()

    val mutable inner : Set<'t>
    static member Create(src:seq<'t>) : RefSet<'t> =
         let src_set = Set.ofSeq src
         match cache.TryGetValue(src_set) with
         | true, v -> v
         | _ ->
             let newset = RefSet(src_set)
             cache.Add(src_set,newset)
             newset

    member this.IsEmpty : bool = this.inner.IsEmpty
    static member unionMany (sets:RefSet<'t> seq) : RefSet<'t> =
        sets
        |> Seq.map (fun v -> v.inner)
        |> Set.unionMany
        |> RefSet.Create
    static member map (fn) (arg:RefSet<'t>) : RefSet<'t> =
        arg.inner
        |> Set.map fn
        |> RefSet.Create
    static member union (arg1:RefSet<'t>) (arg2:RefSet<'t>) : RefSet<'t> =
        Set.union arg1.inner arg2.inner |> RefSet.Create
    static member zeroList : RefSet<int> = RefSet.Create(Set.singleton 0)
    static member empty : RefSet<'t> = RefSet.Create(Set.empty)

    private new(src_set:Set<'t>) = {inner=src_set}





// source:Set<'t>

