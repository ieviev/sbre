module rec Sbre.Optimizations

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Startsets
open Sbre.Types
open Sbre.Pat
open Sbre.Info

#nowarn "9"
module Ptr =
    open FSharp.NativeInterop

    let inline stackalloc<'a when 'a: unmanaged> (length: int) : Span<'a> =
        let p = NativePtr.toVoidPtr(NativePtr.stackalloc<'a> length)
        Span<'a>(p, length)



let rec tryJumpToStartset (c:RegexCache<_>,loc:byref<Location>, nodes:inref<ToplevelORCollection>)  =
    //if true then () else
    let nc = nodes.Count

    if nc > 1 then
        // temporary optimization
        let n1 = nodes.Items[0]
        let n2 = nodes.Items[1]
        let re = obj.ReferenceEquals(n1,n2)
        nodes.MergeIndexes(0,1)



        ()



    match nodes.Count with
    | 0 ->
        if loc.Reversed then
            failwith "TODO!"
        else
            // todo: initial can skip?
            let ss = c.InitialPatternWithoutDotstar.Startset
            // let commonStartsetLocation = c.TryNextStartsetLocation(loc,ss)
            // let ss2 = Startset.inferStartset2 c.Solver c.InitialPatternWithoutDotstar
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
        | And(xs,info) as headnode
            when info.Startset <> Unchecked.defaultof<_> ->
            if not (info.Flags.HasFlag(RegexNodeFlags.CanSkip)) then () else
            let mutable ss = info.Startset
            let mutable ss2 = Startset.inferStartset2(c.Solver)(headnode)
            let commonStartsetLocation = c.TryNextStartsetLocation2(loc,ss,ss2)
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

            // TBD: more startset optimizations
            // match headnode with
            // | TailStartset c outstartset ->
            //     let commonStartsetLocation = c.tryCommonStartset(loc,outstartset)
            //     match commonStartsetLocation with
            //     | ValueSome newLoc ->
            //         loc.Position <- newLoc
            //     | _ -> ()
            // | _ -> ()
        | _ -> ()

    | n ->
        // TBD: more optimizations
        ()

// #nowarn "40"
