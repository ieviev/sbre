module rec Sbre.Optimizations

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
module Ptr =
    open FSharp.NativeInterop
    let inline stackalloc<'a when 'a: unmanaged> (length: int) : Span<'a> =
        let p = NativePtr.toVoidPtr(NativePtr.stackalloc<'a> length)
        Span<'a>(p, length)


[<MethodImpl(MethodImplOptions.AggressiveOptimization)>]
let tryJumpToStartset (c:RegexCache<_>,loc:byref<Location>, nodes:inref<ToplevelORCollection>)  =

    if nodes.Count > 1 then
        // temporary optimization
        let re = obj.ReferenceEquals(nodes.Items[0],nodes.Items[1])
        if re then nodes.MergeIndexes(0,1)

    match nodes.Count with
    | 0 ->
        let ss = c.InitialPatternWithoutDotstar.Startset
        let ss2 = c.InitialSs2()
        let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)
        match commonStartsetLocation with
        | ValueNone -> ()
        | ValueSome pos ->
            loc.Position <- pos
    | 1 ->
        // TODO: reversed startset
        // if loc.Reversed then () else
        match nodes.Items[0] with
        | And(xs,info) as headnode ->
            if not (info.Flags.HasFlag(RegexNodeFlags.CanSkip)) then () else
            let mutable ss = info.Startset

            // 20% worse performance for now
            let commonStartsetLocation = c.TryNextStartsetLocation(loc,ss)

            // if loc.Position = 71 then
            //     ()
            // let mutable ss2 = Startset.inferStartset2(c.Solver)(headnode) //
            // let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)

            // with caching about 125% better performance (NEEDS TESTING)
            // let ss2 = c.Builder.GetSs2Cached(headnode)
            // let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)

            //
            // let pretty1 = c.PrettyPrintMinterm(ss)
            // let pretty2 = c.PrettyPrintMinterm(ss2)
            //
            // if pretty2 = @"[\nT]" then failwith "TODO!"

            match commonStartsetLocation with
            | ValueSome newLoc ->
                loc.Position <- newLoc
            | _ -> ()


        | node when canSkip node ->
#if DEBUG
            // failwith $"TODO!: {node.ToStringHelper()}"
#endif
            let commonStartsetLocation = c.TryNextStartsetLocation(loc,node.Startset)
            match commonStartsetLocation with
            | ValueNone -> ()
            | ValueSome pos ->
                loc.Position <- pos


        | _ ->
            // TBD: more optimizations
            // failwith "TODO!"
            // sample pattern: (and.*&.*dogs.*)
            ()

    | _ ->
        // TBD: more optimizations
        // this branch is rarely reached
        // failwith "TODO!"
        ()
