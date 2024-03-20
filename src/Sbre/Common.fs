namespace Sbre

open System.Globalization



module Common =

#if DEBUG

    open System.Text.RuntimeRegexCopy.Symbolic
    let mutable debuggerSolver: ISolver<uint64> option = None
    #else
    open System.Text.RuntimeRegexCopy.Symbolic
    let mutable debuggerSolver: ISolver<uint64> option = None
#endif

[<RequireQualifiedAccess>]
module Static =
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
    ///
    member val FindPotentialStartSizeLimit = 500 with get, set
    member val UsePrefixOptimizations = true with get, set
    member val UseEcma = false with get, set

    static member HighThroughputDefaults =
        SbreOptions(
            CanonicalizeStates=false,
            CompressPattern=true,
            FindLookaroundPrefix=true,
            FindPotentialStartSizeLimit=5000,
            UsePrefixOptimizations=true,
            InitialDfaCapacity=1024,
            MaxPrefixLength = 20
        )
    static member LearningDefaults =
        SbreOptions(
            CompressPattern=false,
            FindLookaroundPrefix=false,
            FindPotentialStartSizeLimit=20,
            InitialDfaCapacity=4096,
            UsePrefixOptimizations=false
        )

type UnicodeConditions = System.Text.RegularExpressions.Symbolic.UnicodeCategoryConditions

module BDD =
    open System.Text.RuntimeRegexCopy.Symbolic
    let prettyPrintBDD(bdd:BDD) =
        let mutable remainingSet = bdd
        let mutable addedSets = ""
        let css = Static.charsetSolver
        let initial =
            Static.charsetSolver.PrettyPrint(remainingSet)
                .Replace("~",@"\~")
        let isInverted = initial.StartsWith("[^")
        match initial with
        | @"[^\n]" -> "."
        | @"." -> "\."
        | _ when initial.Length <= 20 -> initial
        | _ ->

        if isInverted then
            remainingSet <- css.Not(remainingSet)

        let containsSet (p1:BDD) =
            let cond4 =
                css.IsEmpty(
                    css.And(css.Not(remainingSet),p1)
                )
            cond4

        let removeSet (removedSet:BDD) =
            remainingSet <- css.And(remainingSet,css.Not(removedSet))
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
        let orig = orig.Replace("~",@"\~")
        match orig with
        | "[]" ->
            match addedSets with
            | @"\w" when isInverted -> @"\W"
            | @"\s" when isInverted -> @"\S"
            | @"\d" when isInverted -> @"\D"
            | _ when addedSets.Length = 2 -> addedSets
            | _ -> $"[{inv}{addedSets}]"
        | orig when orig.StartsWith('[')  ->
            $"[{inv}{addedSets}{orig[1..]}"
        | _ ->
            if addedSets = "" then
                if isInverted then $"[{inv}{orig}]" else
                orig
            else $"[{inv}{addedSets}{orig}]"
