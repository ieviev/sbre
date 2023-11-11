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
        let isInitial = refEq c.InitialPatternWithoutDotstar node

        if isInitial then
            let prefix = c.GetInitialStartsetPrefix()
            match prefix with
            | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->
                match c.TryNextStartsetLocationArray(&loc,arr) with
                | ValueNone -> Location.final loc
                | ValueSome newPos -> newPos
            | _ ->
                loc.Position
        else



        let prefix = c.Builder.GetPrefixCached(node)
        match prefix with
        | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->



            let commonStartsetLocation =
                if loc.Reversed && loopEnd.Length = 0 then
                    c.TryNextStartsetLocationArrayReversed(&loc,arr)
                elif isInitial then
                    c.TryNextStartsetLocationArray(&loc,arr)
                elif arr.Length = 1 && loopEnd.Length = 1 then
                // elif arr.Length = 1 then
                    c.TryNextStartsetLocation(loc,arr[0] ||| loopEnd[0])
                else
                    c.TryNextStartsetLocationArrayWithLoopTerminator(loc,arr,loopEnd)

            match commonStartsetLocation with
            | ValueNone ->
                if isInitial then Location.final loc else
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



