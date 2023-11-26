module Debug
#if DEBUG

open System.Text.RuntimeRegexCopy.Symbolic


let mutable debuggerSolver: ISolver<uint64> option = None

#endif

