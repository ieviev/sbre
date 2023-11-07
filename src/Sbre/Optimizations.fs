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
module Ptr =
    open FSharp.NativeInterop
    let inline stackalloc<'a when 'a: unmanaged> (length: int) : Span<'a> =
        let p = NativePtr.toVoidPtr(NativePtr.stackalloc<'a> length)
        Span<'a>(p, length)


// [<MethodImpl(MethodImplOptions.AggressiveOptimization)>]
let rec tryJumpToStartset (c:RegexCache<_>,loc:inref<Location>, nodes:inref<ToplevelORCollection>) : int32 =
    match nodes.Count with
    | 1 ->
        let node = nodes.Items()[0]
        let ss = node.Startset

        // 20% worse performance for now
        // let commonStartsetLocation = c.TryNextStartsetLocation(loc,ss)

        // let mutable ss2 = Startset.inferStartset2(c.Solver)(headnode) //
        // let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)

        // with caching about 125% better performance (NEEDS TESTING)
        // bug in @"lethargy.*air"
        let ss2 = c.Builder.GetSs2Cached(node)
        let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)

        // let pretty1 = c.PrettyPrintMinterm(ss)
        // let pretty2 = c.PrettyPrintMinterm(ss2)
        // let newloc =
        //     Location.create loc.Input commonStartsetLocation.Value

        match commonStartsetLocation with
        | ValueNone -> loc.Position
        | ValueSome newPos -> newPos

    | 0 ->
        let ss = c.InitialPatternWithoutDotstar.Startset
        let commonStartsetLocation = c.TryNextStartsetLocation(loc,ss)
        // let ss2 = c.InitialSs2()
        // let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)
        match commonStartsetLocation with
        | ValueNone ->
            loc.Position
        | ValueSome newPos -> newPos
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
#if DIAGNOSTIC
            failwith "TODO"
#endif


            let mutable ss = c.Solver.Empty
            let startsets =
                for n in nodeSpan do
                    ss <- c.Solver.Or(ss,n.Startset)
            let commonStartsetLocation = c.TryNextStartsetLocation(loc,ss)

            // let mutable ss2 = c.Solver.Empty
            // let startset2s =
            //     for n in nodes.Items do
            //         ss2 <- c.Solver.Or(ss2,c.Builder.GetSs2Cached(n))
            //
            // let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)

            match commonStartsetLocation with
            | ValueNone -> loc.Position
            | ValueSome newPos -> newPos



