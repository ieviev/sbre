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
    | ContainsLookaroundFlag = 4uy
    | ContainsEpsilonFlag = 8uy
    | HasCounterFlag = 16uy
    | IsCounterFlag = 32uy

[<AutoOpen>]
module RegexNodeFlagsExtensions =
    type RegexNodeFlags with
        member this.IsAlwaysNullable = byte (this &&& RegexNodeFlags.IsAlwaysNullableFlag) <> 0uy
        member this.CanBeNullable = byte (this &&& RegexNodeFlags.CanBeNullableFlag) <> 0uy
        member this.ContainsLookaround = byte (this &&& RegexNodeFlags.ContainsLookaroundFlag) <> 0uy
        member this.ContainsEpsilon = (this &&& RegexNodeFlags.ContainsEpsilonFlag) = RegexNodeFlags.ContainsEpsilonFlag
        member this.HasCounter = (this &&& RegexNodeFlags.HasCounterFlag) = RegexNodeFlags.HasCounterFlag
        member this.IsCounter = (this &&& RegexNodeFlags.IsCounterFlag) = RegexNodeFlags.IsCounterFlag

//
[<Flags>]
type RegexStateFlags =
    | None = 0
    | InitialFlag = 1
    | AlwaysNullableFlag = 2
    | CanBeNullableFlag = 4
    | CanSkipFlag = 8
    | HasPrefixFlag = 16
    | ContainsLookaroundFlag = 32
    | HasCounterFlag = 64
    | UseDotnetOptimizations = 128
    // todo: fixed length
    // todo: can be subsumed
    // todo: singleton loop

[<AutoOpen>]
module RegexStateFlagsExtensions =
    type Sbre.Types.RegexStateFlags with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.IsInitial =
            // (# "and" this RegexStateFlags.InitialFlag : RegexStateFlags #) <> RegexStateFlags.None
            // this &&& RegexStateFlags.InitialFlag <> RegexStateFlags.None
            // faster???
            this &&& RegexStateFlags.InitialFlag = RegexStateFlags.InitialFlag
        // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        // member this.IsDeadend =
        //     this &&& RegexStateFlags.DeadendFlag = RegexStateFlags.DeadendFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.IsAlwaysNullable = this &&& RegexStateFlags.AlwaysNullableFlag = RegexStateFlags.AlwaysNullableFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CanBeNullable = this &&& RegexStateFlags.CanBeNullableFlag = RegexStateFlags.CanBeNullableFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ContainsLookaround = this &&& RegexStateFlags.ContainsLookaroundFlag = RegexStateFlags.ContainsLookaroundFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CanSkip = this &&& RegexStateFlags.CanSkipFlag = RegexStateFlags.CanSkipFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.HasPrefix = this &&& RegexStateFlags.HasPrefixFlag = RegexStateFlags.HasPrefixFlag
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.HasCounter = this &&& RegexStateFlags.HasCounterFlag = RegexStateFlags.HasCounterFlag



type Transition<'tset when 'tset :> IEquatable<'tset> and 'tset: equality > = {
    mutable Set : 'tset
    Node : RegexNode<'tset>
}

[<Sealed>]
type RegexNodeInfo<'tset when 'tset :> IEquatable<'tset> and 'tset: equality >() =

    member val NodeFlags: RegexNodeFlags = RegexNodeFlags.None with get, set
    member val InitialStartset: InitialStartset<'tset> = InitialStartset.Uninitialized with get, set
    // member val SkipPrefix: Memory<'tset>*Memory<'tset> = Unchecked.defaultof<_> with get, set
    // member val SkipToChars: SearchValues<char> = Unchecked.defaultof<_> with get, set
    member val Transitions: ResizeArray<Transition<'tset>> = ResizeArray() with get, set
    member val Subsumes: Dictionary<RegexNode<'tset>,bool> = Dictionary() with get, set

    // filled in later
    member val IsPostInitialized : bool = false with get, set
    member val Startset: 'tset = Unchecked.defaultof<'tset> with get, set
    member val StateFlags: 'tset = Unchecked.defaultof<'tset> with get, set

    member inline this.IsAlwaysNullable =

        this.NodeFlags &&& RegexNodeFlags.IsAlwaysNullableFlag = RegexNodeFlags.IsAlwaysNullableFlag
        // (this.Flags &&& RegexNodeFlags.IsAlwaysNullable) <> RegexNodeFlags.None
    member inline this.CanBeNullable =
        this.NodeFlags &&& RegexNodeFlags.CanBeNullableFlag = RegexNodeFlags.CanBeNullableFlag

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CanNotBeNullable() =
         (this.NodeFlags &&& RegexNodeFlags.CanBeNullableFlag) = RegexNodeFlags.None
    member inline this.ContainsLookaround =
        this.NodeFlags &&& RegexNodeFlags.ContainsLookaroundFlag = RegexNodeFlags.ContainsLookaroundFlag
    member inline this.DoesNotContainEpsilon =
        (this.NodeFlags &&& RegexNodeFlags.ContainsEpsilonFlag) = RegexNodeFlags.None


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
        negate: bool
    // optimized cases

    // | Star of  (* RE* *) node: RegexNode<'tset> * info: RegexNodeInfo<'tset>




#if DEBUG
    override this.ToString() =
        if Debug.debuggerSolver.IsNone then "NO INFO" else
        let _solver = (box Debug.debuggerSolver.Value) :?> ISolver<'tset>

        let paren str = $"({str})"

        let tostr(v) =
            if v = _solver.Full then
                "⊤"
            elif _solver.IsEmpty(v) then
                "⊥"
            else
                match _solver.PrettyPrint(v, debugcharSetSolver) with
                | @"[^\n]" -> "."
                | c when c.Length > 12 -> "φ" // dont expand massive sets
                | c -> c

        match this with
        | Singleton v -> tostr v
        | Or(items, _) ->
            let setItems: string list =
                if not (obj.ReferenceEquals(items, null)) then
                    items |> Seq.map (fun v ->  v.ToString() ) |> Seq.toList
                else
                    []

            let combinedList = setItems

            combinedList |> String.concat "|" |> paren
        | And(items, _) ->
            let setItems: string list =
                if not (obj.ReferenceEquals(items, null)) then
                    items |> Seq.map string |> Seq.toList
                else
                    []

            setItems |> String.concat "&" |> paren
        | Not(items, info) ->
            let inner = items.ToString()

            $"~({inner})"
        | Loop(body, lower, upper, info) ->
            let inner =  body.ToString()

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
            let inner = body.ToString()

            match lookBack, negate with
            // | true, true when this.isFull body.Head -> "\\A"
            // | false, true when this.isFull body.Head -> "\\z"
            | false, true -> $"(?!{inner})"
            | false, false -> $"(?={inner})"
            | true, true -> $"(?<!{inner})"
            | true, false -> $"(?<={inner})"

        | Concat(h, t, info) ->
            let body = h.ToString() + t.ToString()
            if info.NodeFlags.HasCounter then
                $"⟨{body}⟩"
            else body
        | Epsilon -> "ε"
        // | Star(node, low, up, info) -> $"{node.ToString()}*"
#endif





    member inline this.TryGetInfo =
        match this with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info) ->
            ValueSome info
        | _ -> ValueNone

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetFlags() =
        match this with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info)  ->
            info.NodeFlags
        | Epsilon ->
            RegexNodeFlags.CanBeNullableFlag ||| RegexNodeFlags.IsAlwaysNullableFlag ||| RegexNodeFlags.ContainsEpsilonFlag
        | Singleton foo -> RegexNodeFlags.None
        | LookAround(node, lookBack, negate) ->
            RegexNodeFlags.CanBeNullableFlag ||| RegexNodeFlags.ContainsLookaroundFlag

    member this.CanBeNullable =
        this.GetFlags().HasFlag(RegexNodeFlags.CanBeNullableFlag)
    member this.CanNotBeNullable =
        not (this.GetFlags().HasFlag(RegexNodeFlags.CanBeNullableFlag))
    member this.ContainsLookaround =
        this.GetFlags().HasFlag(RegexNodeFlags.ContainsLookaroundFlag)

    member this.HasCounter =
        this.GetFlags().HasFlag(RegexNodeFlags.HasCounterFlag)

    member this.IsCounter =
        this.GetFlags().HasFlag(RegexNodeFlags.IsCounterFlag)

    member this.IsAlwaysNullable =
        match this with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info) ->
            info.IsAlwaysNullable
        | Singleton _ -> false
        | LookAround _ -> false
        | Epsilon -> false

    member this.DoesNotContainEpsilon =
        match this with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info) ->
            info.DoesNotContainEpsilon
        | Singleton _ -> true
        | LookAround _ -> true
        | Epsilon -> false


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
    static member Of(inverted, (startset:char[])) = { Flags = inverted; Chars = startset }

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



