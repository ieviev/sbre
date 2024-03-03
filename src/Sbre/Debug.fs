module Debug


#if DEBUG

open System.Text.RuntimeRegexCopy.Symbolic

let mutable debuggerSolver: ISolver<uint64> option = None
// let mutable debuggerSolver: ISolver<uint32> option = None
let mutable debuggerSolver2: ISolver<BDD> option = None
let mutable printSizeLimit: int = 200

#else
open System.Text.RuntimeRegexCopy.Symbolic

let mutable debuggerSolver: ISolver<uint64> option = None

#endif


