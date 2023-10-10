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


// let rec tryJumpToStartset (c:RegexCache<_>,loc:byref<Location>, nodes:inref<HashSet<ToplevelOR>>)  =
// let rec tryJumpToStartset (c:RegexCache<_>,loc:byref<Location>, nodes:inref<ResizeArray<ToplevelOR>>)  =
let rec tryJumpToStartset (c:RegexCache<_>,loc:byref<Location>, nodes:inref<ToplevelORCollection>)  =
    //if true then () else
    match nodes.Count with
    | 0 ->
        // failwith "debug"
        // TODO!
        // Jump.toValidStartPosition(
        //     c,
        //     &loc,
        //     c.InitialStartsetCharsArray[0],
        //     c.initialMintermsArray[1])
        ()
    | 1 ->
        // TODO: reversed startset
        // if loc.Reversed then () else
        match (nodes.Items()[0]) with
        | And(xs,(info)) as headnode
            when info.Startset <> Unchecked.defaultof<_> ->

            if not (info.Flags.HasFlag(RegexNodeFlags.CanSkip)) then () else

            let mutable ss = info.Startset
            // let mutable ss2 = c.Solver.Empty

            // let mutable e = (xs :> seq<_>).GetEnumerator()
            let mutable e = xs.GetEnumerator()
            // while e.MoveNext() && not(c.Solver.IsFull(ss)) do
            //     ss2 <- c.Solver.Or(ss2,Info.inferStartset2(c.Solver)(e.Current))

            let mutable successfulSkip = false

            while not successfulSkip do

                let commonStartsetLocation = c.tryCommonStartset(loc,ss)
                match commonStartsetLocation with
                | ValueSome newLoc ->

#if DIAGNOSTIC
                    if loc.Reversed then logDiagnostic $"{loc.Position} -> {newLoc}, mt: {c.PrettyPrintMinterm(ss)}\n{headnode.ToStringHelper()}"
#endif

                    loc.Position <- newLoc
                    if Location.isPreFinal loc then
                        successfulSkip <- true
                    else


                    let nextLocMinterm =
                        if loc.Reversed then
                            c.MintermForStringIndex(loc.Input,loc.Position - 2)
                        else  c.MintermForStringIndex(loc.Input,Location.nextPosition loc)

                    // let ss2 =
                    //     Info.Startset.inferStartset2 (c.Solver) (headnode)
                    // let ss2pretty =
                    //     $"{c.PrettyPrintMinterm(ss)}  :  {headnode.ToStringHelper()}"

                    match loc.Reversed with
                    | false ->
                        // short skip
                        successfulSkip <- true
                        // longer skip

                        // let mutable nextspan = loc.Input.AsSpan(loc.Position)
                        // match c.IsValidPredicate(ss2, nextLocMinterm) with
                        // | true -> successfulSkip <- true
                        // | false -> loc.Position <- Location.nextPosition loc

                    | true ->
                        let mutable nextspan = loc.Input.AsSpan(0,loc.Position)
                        successfulSkip <- true
                        // TBD: this has interesting semantics
                        // match c.IsValidPredicate(ss2, nextLocMinterm) with
                        // | true -> successfulHop <- true
                        // | false -> loc.Position <- Location.nextPosition loc
                | _ -> successfulSkip <- true //failwith "no more chars to hop"

        // | (Cache.CanSkip c as headnode) ->
        //     // TBD: more startset optimizations
        //     match headnode with
        //     | TailStartset c outstartset ->
        //         let commonStartsetLocation = c.tryCommonStartset(loc,outstartset)
        //         match commonStartsetLocation with
        //         | ValueSome newLoc ->
        //             loc.Position <- newLoc
        //         | _ -> ()
        //     | _ -> ()
        | _ -> ()

    | n ->
        // TBD: more optimizations
        ()

// #nowarn "40"
