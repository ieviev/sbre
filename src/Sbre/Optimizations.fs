module internal rec Sbre.Optimizations

open Sbre.Types
open Sbre.Pat
open System


let tryJumpToStartset (c:RegexCache<uint64>)(loc:byref<Location>) (node:RegexNode<uint64>) : int32 =


    let prefix : InitialStartset<_> =
        c.Builder.GetInitializedPrefix(node)

    match prefix with
    | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->
        let arr = arr.Span
        let loopEnd = loopEnd.Span
        let commonStartsetLocation =
            if refEq c.InitialPatternWithoutDotstar node || loopEnd.Length = 0 then
                if not loc.Reversed then
                    let chars = c.MintermStartsetChars(arr[0])
                    c.TryNextStartsetLocationArray(&loc,arr, chars)
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

let tryJumpToStartset2 (c:RegexCache<uint64>)(loc:byref<Location>) (node:RegexNode<uint64>) : unit =

    match c.Builder.GetInitializedPrefix(node) with
    | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->
        let arr = arr.Span
        let loopEnd = loopEnd.Span

        let commonStartsetLocation =
            if loopEnd.Length = 0 then
                let chars = c.MintermStartsetChars(arr[0])
                c.TryNextStartsetLocationArray(&loc,arr, chars)
                ValueSome loc.Position
            // elif arr.Length = 1 then
            //     let chars = c.MintermStartsetChars(arr[0] ||| loopEnd[0])
            //     c.SkipIndexOfAny(&loc,chars)
            //     ValueSome loc.Position
            else c.TryNextStartsetLocationArrayWithLoopTerminator(&loc,arr,loopEnd)
        match commonStartsetLocation with
        | ValueNone ->
            loc.Position <- Location.final loc
        | ValueSome newPos -> loc.Position <- newPos
    | _ -> ()

