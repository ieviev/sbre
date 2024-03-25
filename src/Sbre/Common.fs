namespace Sbre

open System
open System.Buffers
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.MemoryMappedFiles
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic


[<RequireQualifiedAccess>]
module internal Static =
    let charsetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()

[<AllowNullLiteral>]
type SbreOptions() =
    /// initial dfa size
    member val InitialDfaCapacity = 2048 with get, set
    /// maximum dfa size
    member val MaxDfaCapacity = 1_000_000 with get, set
    /// very expensive upfront but reduces state space
    member val CanonicalizeStates = false with get, set
    /// attempt to make smaller alternations at the cost of initialization time
    member val CompressPattern = true with get, set
    member val MaxPrefixLength = 20 with get, set
    /// default: false, attempt to optimize lookaround prefixes.
    /// can be expensive with unbounded lookarounds
    member val FindLookaroundPrefix = false with get, set
    // member val FindPotentialStartSizeLimit = 500 with get, set
    member val FindPotentialStartSizeLimit = 200 with get, set
    member val UseUnicode = true with get, set
    member val StreamBufferSize = 65536 with get, set

    static member HighThroughputAscii =
        SbreOptions(
            FindLookaroundPrefix = true,
            FindPotentialStartSizeLimit = 5000,
            InitialDfaCapacity = 512,
            UseUnicode = false
        )

    static member HighThroughputUnicode =
        SbreOptions(
            FindLookaroundPrefix = true,
            FindPotentialStartSizeLimit = 5000,
            InitialDfaCapacity = 512,
            UseUnicode = true
        )

    static member LearningDefaults =
        SbreOptions(
            CompressPattern = false,
            FindLookaroundPrefix = false,
            FindPotentialStartSizeLimit = 20,
            InitialDfaCapacity = 4096
        )

    static member WebappDefaults =
        SbreOptions(
            FindPotentialStartSizeLimit = 1,
            MaxPrefixLength = 1,
            CompressPattern = false,
            FindLookaroundPrefix = false,
            InitialDfaCapacity = 512
        )

type internal UnicodeConditions = System.Text.RegularExpressions.Symbolic.UnicodeCategoryConditions

module internal BDD =
    open System.Text.RuntimeRegexCopy.Symbolic

    let prettyPrintBDD(bdd: BDD) =
        let mutable remainingSet = bdd
        let mutable addedSets = ""
        let css = Static.charsetSolver
        let initial = Static.charsetSolver.PrettyPrint(remainingSet)
        let isInverted = initial.StartsWith("[^")
        let symbolsToEscape = System.String([| '('; ')'; '&'; '~'; '.'; '|'; '^'; '$' |])

        match initial with
        | @"[^\n]" -> "."
        | @"." -> "\."
        | c when symbolsToEscape.Contains(c) -> $@"\{c}"
        | _ when initial.Length <= 20 -> initial
        | _ ->

        if isInverted then
            remainingSet <- css.Not(remainingSet)

        let containsSet(p1: BDD) =
            let cond4 = css.IsEmpty(css.And(css.Not(remainingSet), p1))
            cond4

        let removeSet(removedSet: BDD) =
            remainingSet <- css.And(remainingSet, css.Not(removedSet))

        let wordBdd = UnicodeConditions.WordLetter(css)
        let nonWordBdd = css.Not(wordBdd)
        let spaceBdd = UnicodeConditions.WhiteSpace
        let nonSpaceBdd = css.Not(spaceBdd)
        let digitBdd = UnicodeConditions.GetCategory(UnicodeCategory.DecimalDigitNumber)
        let nonDigitBdd = css.Not(digitBdd)

        if containsSet wordBdd then
            removeSet wordBdd
            addedSets <- addedSets + "\w"

        if containsSet nonWordBdd then
            removeSet nonWordBdd
            addedSets <- addedSets + "\W"

        if containsSet digitBdd then
            removeSet digitBdd
            addedSets <- addedSets + "\d"

        if containsSet nonDigitBdd then
            removeSet nonDigitBdd
            addedSets <- addedSets + "\D"

        if containsSet spaceBdd then
            removeSet spaceBdd
            addedSets <- addedSets + "\s"

        if containsSet nonSpaceBdd then
            removeSet nonSpaceBdd
            addedSets <- addedSets + "\S"

        let orig = Static.charsetSolver.PrettyPrint(remainingSet)
        let inv = if isInverted then "^" else ""
        let orig = orig.Replace("~", @"\~")

        match orig with
        | "[]" ->
            match addedSets with
            | @"\w" when isInverted -> @"\W"
            | @"\s" when isInverted -> @"\S"
            | @"\d" when isInverted -> @"\D"
            | _ when addedSets.Length = 2 -> addedSets
            | _ -> $"[{inv}{addedSets}]"
        | orig when orig.StartsWith('[') -> $"[{inv}{addedSets}{orig[1..]}"
        | _ ->
            if addedSets = "" then
                if isInverted then $"[{inv}{orig}]" else orig
            else
                $"[{inv}{addedSets}{orig}]"



[<Struct>]
type MatchResult = {
    Value: string
    Index: int
    Length: int
}

[<Struct>]
type SingleMatchResult = {
    Success: bool
    Value: string
    Index: int
    Length: int
} with

    static member Empty = {
        Success = false
        Value = ""
        Index = 0
        Length = 0
    }

[<Struct>]
type MatchPosition = {
    Index: int
    Length: int
} with

    /// gets string from char span
    member this.GetText(input: ReadOnlySpan<char>) =
        input.Slice(this.Index, this.Length).ToString()

    /// gets UTF-8 decoded string from byte span
    member this.GetText(input: ReadOnlySpan<byte>) : string =
        let bytes = input.Slice(this.Index, this.Length)
        let str = Text.Encoding.UTF8.GetString(bytes)
        str



module internal Memory =
    let inline forall ([<InlineIfLambda>] f) (mem: Memory<'t>) =
        let span = mem.Span
        let mutable e = span.GetEnumerator()
        let mutable forall = true

        while forall && e.MoveNext() do
            forall <- f e.Current

        forall

    let inline exists ([<InlineIfLambda>] f) (mem: Memory<'t>) =
        let span = mem.Span
        let mutable e = span.GetEnumerator()
        let mutable exists = false

        while not exists && e.MoveNext() do
            exists <- f e.Current

        exists

    let inline getCombinedHash(e: 't Memory) =
        let mutable hash = 0
        let span = e.Span

        for n in span do
            hash <- hash ^^^ LanguagePrimitives.PhysicalHash n

        hash

#nowarn "9"

module internal Ptr =
    open FSharp.NativeInterop

    let inline stackalloc< ^a when ^a: unmanaged>(length: int) : Span< ^a > =
        let p = NativePtr.toVoidPtr (NativePtr.stackalloc< ^a> length)
        Span< ^a>(p, length)



module Common =


    [<AutoOpen>]
    module Extensions =
        type ISolver<'t> with

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
                match box overlapped, box smaller with
                | (:? uint64 as ov), (:? uint64 as sm) -> ov = sm
                | (:? BDD as ov), (:? BDD as sm) -> ov = sm
                | (:? BitVector as ov), (:? BitVector as sm) -> ov = sm
                | _ -> failwith "invalid set"



    [<Struct; IsByRefLike>]
    [<DebuggerDisplay("{pool}")>]
    type SharedResizeArrayStruct<'t> =
        val mutable size: int
        val mutable limit: int
        val mutable pool: 't array

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Add(item) =
            if this.size = this.limit then
                this.Grow()

            this.pool[this.size] <- item
            this.size <- this.size + 1


        member this.Grow() =
            let newLimit = this.limit * 2
            let newArray = ArrayPool.Shared.Rent(newLimit)
            Array.Copy(this.pool, newArray, this.size)
            ArrayPool.Shared.Return(this.pool)
            this.pool <- newArray
            this.limit <- this.limit * 2

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Clear() = this.size <- 0

        member this.Contains(item) =
            let mutable e = this.pool.AsSpan(0, this.size).GetEnumerator()
            let mutable found = false

            while not found && e.MoveNext() do
                found <- obj.ReferenceEquals(e.Current, item)

            found

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.GetEnumerator() =
            let mutable e = this.pool.AsSpan(0, this.size).GetEnumerator()
            e

        member this.Length = this.size

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.AsSpan() = this.pool.AsSpan(0, this.size)
        member this.AllocateArray() : 't[] = this.pool.AsSpan(0, this.size).ToArray()
        member this.RentMemory() : Memory<'t> =
            this.pool.AsMemory(0, this.size)

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Dispose() = ArrayPool.Shared.Return(this.pool)

        interface IDisposable with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Dispose() = this.Dispose()

        new(initialSize: int) =
            {
                size = 0
                limit = initialSize
                pool = ArrayPool.Shared.Rent(initialSize)
            }

    /// same as obj.ReferenceEquals(x, y) but checks for reference type
    let inline refEq x y =
        LanguagePrimitives.PhysicalEquality x y



#if DEBUG

    open System.Text.RuntimeRegexCopy.Symbolic
    let mutable debuggerSolver: ISolver<uint64> option = None
#else
    open System.Text.RuntimeRegexCopy.Symbolic
    let mutable debuggerSolver: ISolver<uint64> option = None
#endif

    [<Struct>]
    type LongMatchPosition = {
        Index: int64
        Length: int64
    } with
        /// gets string from stream
        member this.GetText(input: MemoryMappedViewStream) =
            let moveby = this.Index - input.Position
            let newpos = input.Seek(moveby, SeekOrigin.Current)
            use span = new SharedResizeArrayStruct<byte>(int this.Length)
            let slice = span.AsSpan().Slice(0, int this.Length)
            input.ReadExactly(slice)
            let str = Text.Encoding.UTF8.GetString(slice)
            str


    [<AbstractClass>]
    type GenericRegexMatcher() =
        abstract member IsMatch: input: ReadOnlySpan<char> -> bool
        abstract member Replace: input: ReadOnlySpan<char> * replacement: ReadOnlySpan<char> -> string
        abstract member Matches: input: ReadOnlySpan<char> -> MatchResult seq
        abstract member EnumerateMatches: input: ReadOnlySpan<char> -> Span<MatchPosition>

        abstract member MatchPositions:
            input: ReadOnlySpan<char> -> SharedResizeArrayStruct<MatchPosition>

        abstract member MatchPositions:
            input: ReadOnlySpan<byte> -> SharedResizeArrayStruct<MatchPosition>

        abstract member MatchPositions:
            input: MemoryMappedViewStream -> SharedResizeArrayStruct<LongMatchPosition>



        abstract member Match: input: ReadOnlySpan<char> -> SingleMatchResult

        abstract member Count: input: ReadOnlySpan<char> -> int
        abstract member Count: input: ReadOnlySpan<byte> -> int

