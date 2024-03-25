namespace Sbre

open System
open System.Globalization


module Common =

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
    member val UsePrefixOptimizations = true with get, set
    member val UseUnicode = true with get, set

    static member HighThroughputAscii =
        SbreOptions(
            CanonicalizeStates = false,
            CompressPattern = true,
            FindLookaroundPrefix = true,
            FindPotentialStartSizeLimit = 5000,
            UsePrefixOptimizations = true,
            InitialDfaCapacity = 512,
            MaxPrefixLength = 20,
            UseUnicode = false
        )

    static member HighThroughputUnicode =
        SbreOptions(
            CanonicalizeStates = false,
            CompressPattern = true,
            FindLookaroundPrefix = true,
            FindPotentialStartSizeLimit = 5000,
            UsePrefixOptimizations = true,
            InitialDfaCapacity = 512,
            MaxPrefixLength = 20,
            UseUnicode = true
        )

    static member LearningDefaults =
        SbreOptions(
            CompressPattern = false,
            FindLookaroundPrefix = false,
            FindPotentialStartSizeLimit = 20,
            InitialDfaCapacity = 4096,
            UsePrefixOptimizations = false
        )

    static member WebappDefaults =
        SbreOptions(
            CanonicalizeStates = false,
            FindPotentialStartSizeLimit = 1,
            MaxPrefixLength = 1,
            CompressPattern = false,
            FindLookaroundPrefix = false,
            UsePrefixOptimizations = false,
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



