module Debug


#if DEBUG

open System.Text.RuntimeRegexCopy.Symbolic

let mutable debuggerSolver: ISolver<uint64> option = None
// let mutable debuggerSolver: ISolver<uint32> option = None


#else
open System.Text.RuntimeRegexCopy.Symbolic

let mutable debuggerSolver: ISolver<uint64> option = None

#endif


