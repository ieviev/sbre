module internal rec Sbre.Optimizations

open Sbre.Types
open Sbre.Pat

let rec tryJumpToStartset (c:RegexCache<_>,loc:byref<Location>, nodes:RegexNode<uint64>) : int32 =

    match refEq nodes c.False with
    | true ->
        // attempt prefix search
        match c.GetInitialStartsetPrefix() with
        | InitialStartset.MintermArrayPrefix(arr, _) ->
            let commonStartsetLocation = c.TryNextStartsetLocationArray(&loc,arr)
            match commonStartsetLocation with
            | ValueNone -> Location.final loc
            | ValueSome newPos -> newPos
        | _ -> loc.Position
    | false ->
        let node = nodes
        let prefix : InitialStartset =
            match node.TryGetInfo with
            | ValueSome info ->
                match info.InitialStartset with
                | InitialStartset.Uninitialized ->
                    info.InitialStartset <- c.Builder.GetPrefixCached(node)
                    info.InitialStartset
                | _ ->
                    info.InitialStartset
            | _ -> InitialStartset.Unoptimized

        match prefix with
        | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->
            let commonStartsetLocation =
                if refEq c.InitialPatternWithoutDotstar node || loopEnd.Length = 0 then
                    if not loc.Reversed then c.TryNextStartsetLocationArray(&loc,arr)
                    else c.TryNextStartsetLocationArrayReversed(&loc,arr)
                elif arr.Length = 1 && loopEnd.Length = 1 then
                    c.TryNextStartsetLocation(loc,arr[0] ||| loopEnd[0])
                elif arr.Length = 1 then
                    c.TryNextStartsetLocation(loc,arr[0])
                else
                    c.TryNextStartsetLocationArrayWithLoopTerminator(loc,arr,loopEnd)

            match commonStartsetLocation with
            | ValueNone ->
                if refEq c.InitialPatternWithoutDotstar node then Location.final loc else
                loc.Position
            | ValueSome newPos -> newPos
        | _ ->
            match refEq node c.Builder.uniques._trueStar with
            | true -> Location.final loc
            | _ ->
            let commonStartsetLocation = c.TryNextStartsetLocation(loc,node.Startset)
            match commonStartsetLocation with
            | ValueNone ->
                loc.Position
            | ValueSome newPos -> newPos


