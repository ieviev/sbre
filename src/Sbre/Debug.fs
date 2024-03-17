namespace Sbre
module Debug =

#if DEBUG

    open System.Text.RuntimeRegexCopy.Symbolic
    let mutable debuggerSolver: ISolver<uint64> option = None
    #else
    open System.Text.RuntimeRegexCopy.Symbolic
    let mutable debuggerSolver: ISolver<uint64> option = None
#endif


[<AllowNullLiteral>]
type SbreOptions() =
    /// initial dfa size
    member val InitialDfaCapacity = 2048 with get, set
    /// maximum dfa size
    member val MaxDfaCapacity = 1_000_000 with get, set
    /// very expensive upfront but reduces state space
    member val CanonicalizeStates = false with get, set
    /// attempt to make smaller alternations at the cost of initialization time
    member val CompressPattern = false with get, set
    member val MaxPrefixLength = 20 with get, set
    /// default: false, attempt to optimize lookaround prefixes.
    /// can be expensive with unbounded lookarounds
    member val FindLookaroundPrefix = true with get, set
    ///
    member val FindPotentialStartSizeLimit = 500 with get, set
    member val UsePrefixOptimizations = true with get, set


    static member HighThroughputDefaults =
        SbreOptions(
            CompressPattern=false,
            FindLookaroundPrefix=false,
            FindPotentialStartSizeLimit=100
        )
    static member LearningDefaults =
        SbreOptions(
            CompressPattern=false,
            FindLookaroundPrefix=false,
            FindPotentialStartSizeLimit=100
        )


