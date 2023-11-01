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

    // temporary optimization for top-level-or
    let mutable optimized = false
    while not optimized do
        let nc = nodes.Count
        if nc > 1 then
            let re = obj.ReferenceEquals(nodes.Items[nc - 2],nodes.Items[nc - 1])
            if re then nodes.MergeIndexes(nc - 2,nc - 1)
            else optimized <- true
        else
            optimized <- true


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
        match nodes.Items[0] with
        | And(xs,info) as headnode ->
            let mutable ss = info.Startset

            // 20% worse performance for now
            // let commonStartsetLocation = c.TryNextStartsetLocation(loc,ss)

            // if loc.Position = 71 then
            //     ()
            // let mutable ss2 = Startset.inferStartset2(c.Solver)(headnode) //
            // let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)

            // with caching about 125% better performance (NEEDS TESTING)
            let ss2 = c.Builder.GetSs2Cached(headnode)
            let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)

            //
            // let pretty1 = c.PrettyPrintMinterm(ss)
            // let pretty2 = c.PrettyPrintMinterm(ss2)
            //
            // if pretty2 = @"[\nT]" then failwith "TODO!"

            match commonStartsetLocation with
            | ValueNone -> Location.setFinal &loc
            | ValueSome newLoc ->
                loc.Position <- newLoc




        | node ->
#if DEBUG
            // failwith $"TODO!: {node.ToStringHelper()}"
#endif
            let commonStartsetLocation = c.TryNextStartsetLocation(loc,node.Startset)

            // not properly implemented
            // let ss2 = c.Builder.GetSs2Cached(node)
            // let pretty = c.PrettyPrintMinterm(ss2)
            // let isf = c.Solver.IsFull(ss2)
            // let commonStartsetLocation = c.TryNextStartsetLocation2(loc,node.Startset,ss2)

            match commonStartsetLocation with
            | ValueNone -> Location.setFinal &loc
            | ValueSome pos ->
                loc.Position <- pos


    | count ->
        // TBD: more optimizations
        // this branch is rarely reached

        // jump with multiple heads
        let mutable ss = c.Solver.Empty
        let startsets =
            for n in nodes.Items do
                ss <- c.Solver.Or(ss,n.Startset)
        let commonStartsetLocation = c.TryNextStartsetLocation(loc,ss)
        match commonStartsetLocation with
        | ValueNone -> ()
        | ValueSome pos ->
            loc.Position <- pos

        ()
