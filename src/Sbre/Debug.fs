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
    member val InitialDfaCapacity = 1024 with get, set
    /// maximum dfa size
    member val MaxDfaCapacity = 1_000_000 with get, set
    /// very expensive upfront but reduces state space
    member val CanonicalizeStates = false with get, set
    /// attempt to make alternations at the cost of initialization time
    member val MinimizeOr = false with get, set


