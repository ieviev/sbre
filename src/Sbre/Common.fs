[<Microsoft.FSharp.Core.AutoOpen>]
module Sbre.Common

open System.Text.RuntimeRegexCopy.Symbolic

#if DEBUG
let debugcharSetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()
let bddBuilder = SymbolicRegexBuilder<BDD>(debugcharSetSolver, debugcharSetSolver)
let mutable debuggerSolver: ISolver<uint64> option = None


let logger = System.Console.Out
// let logger = new StreamWriter("/tmp/log.log")
// do logger.WriteLine("\n\n")


let logDebug(txt: string) : unit =
    logger.WriteLine(txt)
    logger.Flush()

let inline logDiagnostic(txt: string) : unit =
    ()

#if DIAGNOSTIC
    let frameCount = System.Diagnostics.StackTrace().FrameCount
    let currMethod = System.Diagnostics.StackTrace().GetFrame(0).GetMethod().Name

    if frameCount > 3 then
        let lastCaller1 = System.Diagnostics.StackTrace().GetFrame(1).GetMethod().Name
        let lastCaller2 = System.Diagnostics.StackTrace().GetFrame(2).GetMethod().Name
        logger.WriteLine($"{currMethod}, {frameCount}:{lastCaller2}:{lastCaller1}\n{txt}")
    else
        logger.WriteLine($"{currMethod}, {frameCount}:\n{txt}")

    logger.Flush()
#endif

#endif

let inline refEq x y = obj.ReferenceEquals(x, y)
