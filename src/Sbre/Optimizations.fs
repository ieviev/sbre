module internal rec Sbre.Optimizations

open Sbre.Types
open Sbre.Pat
open System


let inline tryJumpToStartset (c:RegexCache<uint64>)(loc:byref<Location>) (node:RegexNode<uint64>) : int32 =


    let prefix : InitialStartset<_> =
        c.Builder.GetInitializedPrefix(node)

    match prefix with
    | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->
        let arr = arr.Span
        let loopEnd = loopEnd.Span
        let commonStartsetLocation =
            if refEq c.InitialPatternWithoutDotstar node || loopEnd.Length = 0 then
                if not loc.Reversed then
                    c.TryNextStartsetLocationArray(&loc,arr)
                    ValueSome loc.Position
                else c.TryNextStartsetLocationArrayReversed(&loc,arr)
            elif arr.Length = 1 && loopEnd.Length = 1 then
                c.TryNextStartsetLocation(&loc,c.Solver.Or(arr[0],loopEnd[0]))
                ValueSome loc.Position
            // elif arr.Length = 1 then
            //     c.TryNextStartsetLocation(loc,arr[0])
            else
                // c.TryNextStartsetLocation(&loc,c.Solver.Or(arr[0],loopEnd[0]))
                c.TryNextStartsetLocationArrayWithLoopTerminator(&loc,arr,loopEnd)
        match commonStartsetLocation with
        | ValueNone ->
            Location.final loc

        | ValueSome newPos -> newPos
    | _ ->
        // TODO: does occur
        loc.Position



