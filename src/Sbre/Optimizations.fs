module internal rec Sbre.Optimizations

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open Sbre.Pat
open Sbre.Info

#nowarn "9"


// [<MethodImpl(MethodImplOptions.AggressiveOptimization)>]
let rec tryJumpToStartset (c:RegexCache<_>,loc:byref<Location>, nodes:inref<ToplevelORCollection>) : int32 =

    match nodes.Count with
    | 1 ->
        let node = nodes.First
        // check if initial pattern

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
            let ss = node.Startset
            match node with
            | Cache.IsTrueStar c -> Location.final loc
            | _ ->
            let commonStartsetLocation = c.TryNextStartsetLocation(loc,ss)
            match commonStartsetLocation with
            | ValueNone ->
                loc.Position
            | ValueSome newPos -> newPos

    | 0 ->
        // attempt prefix search
        match c.GetInitialStartsetPrefix() with
        | InitialStartset.MintermArrayPrefix(arr, _) ->
            let commonStartsetLocation = c.TryNextStartsetLocationArray(&loc,arr)
            match commonStartsetLocation with
            | ValueNone -> Location.final loc
            | ValueSome newPos -> newPos
        | _ -> loc.Position
    | _ ->
        // TBD: more optimizations
        // this branch is rarely reached
        // jump with multiple heads

        let nodeSpan = nodes.Items()



        // first try to eliminate any duplicates
        if refEq nodeSpan[0] nodeSpan[1] then

            nodes.Remove(1)
            tryJumpToStartset(c,&loc,&nodes)
        else
#if OPTIMIZE
            nodes.Items().ToArray()
            // |> Array.map (fun v -> v.ToStringHelper())
            |> Array.map (fun v -> v.ToString())
            |> String.concat "\n"
            |> failwith
#endif

            let mutable ss = c.Solver.Empty
            let startsets =
                for n in nodeSpan do
                    ss <- c.Solver.Or(ss,n.Startset)
            let commonStartsetLocation = c.TryNextStartsetLocation(loc,ss)

            match commonStartsetLocation with
            | ValueNone -> loc.Position
            | ValueSome newPos -> newPos



