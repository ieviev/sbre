module rec Sbre.Optimizations

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Startsets
open Sbre.Types
open Sbre.Patterns
open Sbre.Info

#nowarn "9"
module Ptr =
    open FSharp.NativeInterop

    let inline stackalloc<'a when 'a: unmanaged> (length: int) : Span<'a> =
        let p = NativePtr.toVoidPtr(NativePtr.stackalloc<'a> length)

        Span<'a>(p, length)

let rec getStartset(cache:RegexCache<_>,node: RegexNode< _ > list) =
    let _solver = cache.Solver
    let rec loop (pos:int, node:RegexNode< ^t > list) =
        if pos > 3 then _solver.Full
        else
        let current =
            match node with
            | [] -> _solver.Empty
            | RegexNode.Singleton pred::_ -> pred
            | RegexNode.Or(xs,_)::_ ->
                [yield! xs]
                |> List.fold (fun acc v -> _solver.Or(acc, loop(pos,v))) _solver.Empty
            | RegexNode.Loop(R,low,_,info)::tail ->
                match low with
                | 0 -> _solver.Or(loop(pos,R),loop(pos,tail))
                | _ -> loop(pos,R)
            | RegexNode.And(xs,info)::_ ->
                xs
                |> Solver.mapAnd _solver (fun v -> loop(pos,v) )
            | RegexNode.Not (inner,info)::tail -> _solver.Or(loop(pos,inner),loop(pos,tail))
            // TBD: return full set for lookbacks now
            | RegexNode.LookAround(lookBack=true)::_ -> _solver.Full
            | RegexNode.LookAround(body,_,negate)::tail ->
                if not negate then _solver.And(loop(pos,body),loop(pos,tail))
                else _solver.Full // TBD: potentially invert this?


        match node with
        | [] -> current
        | ZeroWidthNode::_ ->
            if _solver.IsFull(current) then current else
            let currset = _solver.Or(current,loop(pos+1,node))
            currset
        | _ ->
            current
    loop (0,node)



let generateStartsets(cache:RegexCache<_>) =
    let mutable _setpos = 0u
    let mutable currnode = cache.InitialPatternWithoutDotstar
    let mutable _startsetArr = Array.zeroCreate< ^t > 3
    let _solver = cache.Solver

    // try
    while _setpos < 3u do
        let currStartset =
            match currnode with
            | [] -> _solver.Full
            | [final] -> getStartset(cache,[final])
            | Singleton pred::_ -> pred
            | LookAround _::Singleton pred::_ -> pred
            | Not (inner,info)::_ -> info.Startset
            // word border predicate
            | LookAround _::tail -> getStartset(cache,tail)
            | Loop(node=node;low=0)::tail -> _solver.Or(getStartset(cache,node),getStartset(cache,tail))
            | other -> getStartset(cache,other)


        _startsetArr[int _setpos] <- currStartset
        _setpos <- _setpos + 1u

        match currnode with
        | SharedTail tail ->
            currnode <- Cache.mkOrOfSeq(cache,tail)
        | _ -> ()

    cache.initialMintermsArray <- _startsetArr



[<return: Struct>]
let rec (|TailStartset|_|) (cache:RegexCache<_>) (node: RegexNode<_> list) =
    match node with
    | [ Not(_::xs,info) ] -> ValueSome(getStartset(cache,xs))
    | Not(_::xs,info)::tail -> ValueSome(cache.Solver.Or(getStartset(cache,xs),getStartset(cache,tail)))
    | _::xs -> ValueSome(getStartset(cache,xs))
    | _ ->
        // TBD: optimize startset for tail
        ValueNone



let rec tryJumpToStartset (c:RegexCache<_>,loc:byref<Location>, nodes:inref<HashSet<ToplevelOR>>)  =

    match nodes.Count with
    | 0 ->
        Jump.toValidStartPosition(
            c,
            &loc,
            c.InitialStartsetCharsArray[0],
            c.initialMintermsArray[1])
    | 1 ->
        match (head nodes).Node with
        | And(xs,(info))::_
            when info.Startset <> Unchecked.defaultof<_> ->

            if not (info.Flags.HasFlag(RegexNodeFlags.CanSkip)) then () else

            let mutable ss = info.Startset
            let mutable ss2 = c.Solver.Empty

            let mutable e = (xs :> seq<_>).GetEnumerator()
            while e.MoveNext() && not(c.Solver.IsFull(ss)) do
                ss2 <- c.Solver.Or(ss2,Info.inferStartset2(c.Solver)(e.Current))

            let mutable successfulSkip = false

            while not successfulSkip do

                let commonStartsetLocation = c.tryCommonStartset(loc,ss)
                match commonStartsetLocation with
                | ValueSome newLoc ->
#if DIAGNOSTIC
                    logDiagnostic $"jumping {loc.Position}:{ss}- -> {newLoc}"
#endif

                    loc.Position <- newLoc
                    if Location.isPreFinal loc then
                        successfulSkip <- true
                    else


                    let nextLocMinterm =
                        if loc.Reversed then
                            c.MintermForStringIndex(loc.Input,loc.Position - 2)
                        else  c.MintermForStringIndex(loc.Input,Location.nextPosition loc)


                    match loc.Reversed with
                    | false ->
                        let mutable nextspan = loc.Input.AsSpan(loc.Position)
                        match c.IsValidPredicate(ss2, nextLocMinterm) with
                        | true -> successfulSkip <- true
                        | false -> loc.Position <- Location.nextPosition loc
                    | true ->
                        let mutable nextspan = loc.Input.AsSpan(0,loc.Position)
                        successfulSkip <- true
                        // TBD: this has interesting semantics
                        // match c.IsValidPredicate(ss2, nextLocMinterm) with
                        // | true -> successfulHop <- true
                        // | false -> loc.Position <- Location.nextPosition loc
                | _ -> successfulSkip <- true //failwith "no more chars to hop"

        | (Cache.CanSkip c as headnode) ->
            // TBD: more startset optimizations
            match headnode with
            | TailStartset c outstartset ->
                let commonStartsetLocation = c.tryCommonStartset(loc,outstartset)
                match commonStartsetLocation with
                | ValueSome newLoc ->
                    loc.Position <- newLoc
                | _ -> ()
            | _ -> ()
        | _ -> ()

    | n ->
        // TBD: more optimizations
        ()

#nowarn "40"


module ToplevelOr =

    [<return: Struct>]
    // cases where a match ends unconditionally
    let rec (|MustExit|_|) (c: RegexCache< uint64 >) (nodes: HashSet<ToplevelOR>) =
        let mutable found = ValueNone
        let mutable e = nodes.GetEnumerator()
        while e.MoveNext() && found.IsNone do

            match e.Current.Node with
            | Cache.IsAnyStarList c -> found <- ValueSome(e.Current)
            | Cache.IsAnchor c -> found <- ValueSome(e.Current)
            | Cache.IsFalseList c -> found <- ValueSome(e.Current)
            | _ -> ()
        found




