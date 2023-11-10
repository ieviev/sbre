module internal Sbre.Info

open System
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open Pat

#nowarn "9"

module Ptr =
    open FSharp.NativeInterop

    let inline stackalloc< ^a when ^a: unmanaged>(length: int) : Span< ^a > =
        let p = NativePtr.toVoidPtr (NativePtr.stackalloc< ^a> length)
        Span< ^a>(p, length)



type Flag = RegexNodeFlags

module Flag =
    let inline withFlag (arg: RegexNodeFlags) (sourceFlags: RegexNodeFlags) = sourceFlags ||| arg

    let inline withoutFlag (arg: RegexNodeFlags) (sourceFlags: RegexNodeFlags) =
        sourceFlags &&& ~~~arg

    let inline withFlagIf (cond: bool) (arg: RegexNodeFlags) (sourceFlags: RegexNodeFlags) =
        if cond then sourceFlags ||| arg else sourceFlags

    let inline hasFlag (arg: RegexNodeFlags) (sourceFlags: RegexNodeFlags) =
        sourceFlags.HasFlag(arg)

    let inline getContainsFlags(sourceFlags: RegexNodeFlags) =
        (RegexNodeFlags.ContainsEpsilon ||| RegexNodeFlags.ContainsLookaround)
        &&& sourceFlags

    /// bitwise or for multiple flags
    let inline mergeFlags(sourceFlags: RegexNodeFlags seq) =
        (RegexNodeFlags.None, sourceFlags) ||> Seq.fold (fun acc v -> acc ||| v)

// accessors and common functions
let rec canSkip(node: RegexNode<'t>) : bool =
    match node with
    | Singleton _ -> false
    | Or(info = info) -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
    | And(info = info) -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
    | Loop(low = 0; up = Int32.MaxValue) -> true
    | Loop _ -> false
    | Not(inner, info) -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
    | LookAround _ -> false
    | Epsilon -> false
    | Concat(info = info) -> Flag.hasFlag Flag.CanSkip info.Flags


let inline removeFlag (flags: byref<RegexNodeFlags>) (flagsToRemove: RegexNodeFlags) =
    flags <- flags &&& ~~~flagsToRemove

let inline addFlag (flags: byref<RegexNodeFlags>) (flagsToAdd: RegexNodeFlags) =
    flags <- flags ||| flagsToAdd

let inline invertFlag (flags: byref<RegexNodeFlags>) (flagsToInvert: RegexNodeFlags) =
    if flags.HasFlag(flagsToInvert) then
        flags <- flags &&& ~~~flagsToInvert
    else
        flags <- flags ||| flagsToInvert



// Patterns
[<return: Struct>]
let (|CanNotBeNullable|_|)(x: RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.CanBeNullable) with
    | false -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|IsAlwaysNullable|_|)(x: RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) with
    | true -> ValueSome()
    | _ -> ValueNone


[<return: Struct>]
let (|NodeIsAlwaysNullable|_|)(x: RegexNode<'t>) =
    match x with
    | Or(xs, IsAlwaysNullable) -> ValueSome()
    | Singleton foo -> ValueNone
    | Loop(node, low, up, IsAlwaysNullable) -> ValueSome()
    | And(xs, IsAlwaysNullable) -> ValueSome()
    | Not(fSharpList, IsAlwaysNullable) -> ValueSome()
    | LookAround(node, lookBack, negate) -> ValueNone
    | _ -> ValueNone



[<return: Struct>]
let (|CanBeNullable|_|)(x: RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.CanBeNullable) with
    | true -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|ContainsLookaround|_|)(x: RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.ContainsLookaround) with
    | true -> ValueSome()
    | _ -> ValueNone


module rec Startset =

    let inline inferMergeStartset (_solver: ISolver<'t>) (nodes: seq<RegexNode<'t>>) =
        // todo: small optimization here
        nodes |> Solver.mapOr _solver (inferStartset _solver)


    let rec inferConcatStartset (_solver: ISolver<'t>) (head: RegexNode<'t>) (tail: RegexNode<'t>) =

        match head with
        | Loop(node = Singleton pred; low = 0; up = Int32.MaxValue) ->
            // bug here
            let tailStartset = inferStartset _solver tail
            let invertedPred = _solver.Not(pred)
            _solver.Or(invertedPred, tailStartset)

        | Loop(node = node; low = low; up = up) ->

            let inner = inferStartset _solver node
            // failwith "debug2"
            match low with
            | 0 -> _solver.Full // TODO: optimize
            | _ -> inner
        | Singleton pred -> pred
        | Or(xs, info) ->
            use mutable e = xs.GetEnumerator()

            if info.CanNotBeNullable then
                Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e
            else
                let headss = Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e
                _solver.Or(headss, tail.Startset)






        | Not(node, info) ->
            // let tailConcat = Concat(tail, info)
            let tailStartset = inferStartset _solver tail
            let headStartset = inferStartset _solver node
            let merged = _solver.Or(headStartset, tailStartset)
            merged
        | LookAround _ -> _solver.Full
        | Epsilon -> inferStartset _solver tail
        | And(xs, info) ->
            use mutable e = xs.GetEnumerator()

            if info.CanNotBeNullable then
                Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e
            else
                let headss = Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e
                _solver.Or(headss, tail.Startset)


        | Concat(chead, ctail, info) -> inferConcatStartset _solver chead ctail

    let inline inferLoopStartset (_solver: ISolver<'t>) (R, low, up) =
        match (R, low, up) with
        | Concat _, 0, Int32.MaxValue -> _solver.Full
        | _ ->
            let bodyStartset = inferStartset _solver R

            match low, up with
            | 0, Int32.MaxValue -> _solver.Not(bodyStartset)
            | _ -> bodyStartset

    let rec inferStartset (_solver: ISolver<'t>) (node: RegexNode<'t>) =
        match node with
        | Epsilon -> _solver.Full
        | Singleton pred -> pred
        // TODO: how to optimize (a|ab)*
        | Loop(Concat _, low, up, info) -> _solver.Full
        | Loop(loopBody, low, up, info) ->
            let bodyStartset = inferStartset _solver loopBody

            match low, up with
            | 0, Int32.MaxValue -> _solver.Not(bodyStartset)
            | _ -> bodyStartset

        | Or(xs, info) ->
            use mutable e = xs.GetEnumerator()
            Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e

        | Not(inner, info) -> inferStartset _solver inner
        | LookAround(node = body; lookBack = false) -> _solver.Full // TODO: optimize
        | LookAround(lookBack = true) -> _solver.Full
        | Concat(h, t, info) -> inferConcatStartset _solver h t
        | And(xs, info) ->
            use mutable e = xs.GetEnumerator()
            Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e


    let rec inferStartset2 (_solver: ISolver<'t>) (node: RegexNode<'t>) =
        // if true then _solver.Full else // todo: optimize
        // if not node.CanSkip then _solver.Full else // todo: optimize
        match node with
        | Or(xs, info) ->
            use mutable e = xs.GetEnumerator()
            Solver.mergeOrWithEnumerator _solver (inferStartset2 _solver) &e
        | Not(inner, info) -> inferStartset2 _solver inner
        | LookAround(node = body; lookBack = false) -> _solver.Empty // TODO: optimize
        | LookAround(lookBack = true) -> _solver.Empty
        | Concat(Loop(node = Singleton pred; low = 0; up = Int32.MaxValue),
                 Concat(Singleton chead, concatTail2, _),
                 info) ->

            if _solver.IsFull(pred) then
                match concatTail2 with
                | Concat(Loop(node = loopBody; low = 0; up = up) as loop, tail, _) ->

                    _solver.Or(_solver.Not(loop.Startset), tail.Startset)
                | _ -> concatTail2.Startset
            else if concatTail2.CanNotBeNullable then
                // .*air -> .* only has 1 char startset
                _solver.Full

            else if concatTail2.IsAlwaysNullable then // negation startset inference test
                _solver.Or(inferStartset2 _solver concatTail2, concatTail2.Startset)
            else
            // failwith "todo1"
            _solver.Full // .* not optimized

        | Concat(Not(node = inner; info = notInfo), Singleton pred, info) ->
            let ss1 = inferStartset2 _solver inner
            _solver.Or(ss1, pred) // 4x performance win
        | Concat(Not(node = inner; info = notInfo), tailNode, info) ->

            let ss2 = tailNode.Startset
            let ss22 = inferStartset2 _solver tailNode
            let ss1 = inferStartset2 _solver inner
            _solver.Or(_solver.Or(ss1, ss2), ss22) // negation test 8
        // _solver.Full

        | Concat(Singleton hpred, t, info) ->

            match t with
            | Not(_) -> _solver.Full
            | _ ->
                // inferStartset2 _solver
                t.Startset
        // _solver.Full // todo: (.*dogs.*&and.*), (a.*&~(.*b.*)b)
        | Concat(Concat(h1, h2, _) as c1, tailNode, info) when h1.CanNotBeNullable ->
            let ss1 = inferStartset2 _solver h1
            let ss22 = h2.Startset
            let ss2 = tailNode.Startset
            _solver.Or(_solver.Or(ss1, ss2), ss22) // negation test 8
        // ⊤*
        | Concat(head = Loop(node = Singleton pred); tail = tail) when _solver.IsFull(pred) ->
            inferStartset2 _solver tail

        | Concat(h, t, info) ->
            if h.CanNotBeNullable then // regexlib 20-30
                let ss1 = inferStartset2 _solver h
                let ss22 = inferStartset2 _solver t
                let ss2 = t.Startset
                _solver.Or(_solver.Or(ss1, ss2), ss22) // negation test 8
            else
                // failwith "todo2" // TODO: optimize further
                _solver.Full

        | And(xs, info) ->
            use mutable e = xs.GetEnumerator()
            Solver.mergeOrWithEnumerator _solver (inferStartset2 _solver) &e

        // todo: correct?
        // | Loop(low=0; up=Int32.MaxValue) -> _solver.Empty
        // TODO: how to optimize (a|ab)*

        | Loop(_)
        | Epsilon
        | Singleton _ -> _solver.Full


    /// can't be used during-match as it would skip newlines in .*
    let rec inferInitialStartset (_solver: ISolver<uint64>) (startNode: RegexNode<uint64>) =
        let acc = ResizeArray()
        // let acc = Ptr.stackalloc<uint64> 5
        let mutable uninitialized = true
        let mutable cannotOptimizeYet = false
        let mutable curr = startNode
        let mutable loopTerminator = _solver.Empty

        while not (obj.ReferenceEquals(null, curr)) do
            match curr with
            | Loop(node = Singleton pred; info = info) when uninitialized ->
                if not (_solver.IsFull(pred)) then
                    loopTerminator <- _solver.Not(pred)
                    acc.Add(_solver.Not(pred))
                curr <- Unchecked.defaultof<_>
            | Not(node = node; info = info) when uninitialized && info.HasPrefix ->
                match inferInitialStartset _solver node with
                | MintermArrayPrefix(arr, loopEnd) ->
                    while acc.Count < arr.Length do
                        acc.Add(_solver.Empty)

                    for i = 0 to arr.Length - 1 do
                        let ss = arr[i]
                        acc[i] <- _solver.Or(ss, acc[i])
                | _ -> ()

                curr <- Unchecked.defaultof<_>
            | Concat(Loop(node=Singleton pred;low = 0; up = Int32.MaxValue), t, info) ->
                if not uninitialized then curr <- Unchecked.defaultof<_>
                else
                    loopTerminator <- _solver.Or(loopTerminator,_solver.Not(pred))
                    curr <- t

            | Singleton pred ->
                acc.Add(pred)
                curr <- Unchecked.defaultof<_>
                // TODO: \n\n~(⊤*\n\n⊤*)\n\n&
            | LookAround(_)
            | Concat(head=Not(_))
            | Not(_) -> curr <- Unchecked.defaultof<_> // ignore
            | Concat(Singleton pred, t, info) ->
                acc.Add(pred)
                curr <- t
            // top level or optimizations
            | Or(xs, info) ->
                use mutable e = xs.GetEnumerator()

                while e.MoveNext() do
                    match e.Current with
                    | Not(_)
                    | Concat(head = Not(_)) -> () // ignore negations
                    | Loop(node = Singleton pred; low = 0; up = Int32.MaxValue) ->
                        // ignore true-stars, add \n terminator for dot-stars
                        if not (_solver.IsFull(pred)) then
                            loopTerminator <- _solver.Or(loopTerminator,_solver.Not(pred))
                        // ()
                    | LookAround(_) -> () // ignore
                    | Loop(_) -> cannotOptimizeYet <- true
                    | Concat(_) | Or(_) | And(_) ->
                        match inferInitialStartset _solver e.Current with
                        | MintermArrayPrefix(arr,innerLoopTerminator) ->
                            if not (_solver.IsEmpty(innerLoopTerminator)) then
                                loopTerminator <- _solver.Or(loopTerminator,innerLoopTerminator)
                            if acc.Count = 0 then
                                while acc.Count < arr.Length do
                                    acc.Add(_solver.Empty)

                            // shorten to shared prefix
                            if acc.Count > arr.Length then
                                while acc.Count > arr.Length do
                                    acc.RemoveAt(acc.Count - 1)

                            for i = 0 to acc.Count - 1 do
                                let ss = arr[i]
                                acc[i] <- _solver.Or(ss, acc[i])
                        | InitialStartset.Unoptimized ->
                            cannotOptimizeYet <- true


                        let dbg = 1
                        ()
                    // todo: more optimizations
                    | _ -> cannotOptimizeYet <- true
                curr <- Unchecked.defaultof<_>
            | And(xs, info) ->
                use mutable e = xs.GetEnumerator()

                while e.MoveNext() do
                    match e.Current with
                    | Not(_)
                    | Concat(head = Not(_)) -> () // ignore negations if there's something to match
                    | Loop(node = Singleton pred; low = 0; up = Int32.MaxValue) ->
                        // ignore true-stars, add \n terminator for dot-stars
                        if not (_solver.IsFull(pred)) then
                            loopTerminator <- _solver.Or(loopTerminator,_solver.Not(pred))
                        // ()
                    | LookAround(_) -> () // ignore
                    | Loop(_) -> cannotOptimizeYet <- true
                    | Concat(_) | Or(_) | And(_) ->
                        match inferInitialStartset _solver e.Current with
                        | MintermArrayPrefix(arr,innerLoopTerminator) ->
                            if not (_solver.IsEmpty(innerLoopTerminator)) then
                                loopTerminator <- _solver.Or(loopTerminator,innerLoopTerminator)
                            if acc.Count = 0 then
                                while acc.Count < arr.Length do
                                    acc.Add(_solver.Empty)

                            // shorten to shared prefix
                            if acc.Count > arr.Length then
                                while acc.Count > arr.Length do
                                    acc.RemoveAt(acc.Count - 1)

                            for i = 0 to acc.Count - 1 do
                                let ss = arr[i]
                                acc[i] <- _solver.Or(ss, acc[i])
                        | InitialStartset.Unoptimized ->
                            cannotOptimizeYet <- true


                        let dbg = 1
                        ()
                    // todo: more optimizations
                    | _ -> cannotOptimizeYet <- true
                curr <- Unchecked.defaultof<_>
            | x ->
                match x.TryGetInfo with
                | Some info when not (info.Flags.HasFlag(Flag.Prefix)) ->
                    curr <- Unchecked.defaultof<_>
                | _ ->
                    curr <- Unchecked.defaultof<_>
                    // TODO: could be optimized
// #if DEBUG
//                     failwith $"startset: {x.ToStringHelper()}"
// #endif
//                     failwith "asd"


            uninitialized <- false

        if cannotOptimizeYet then InitialStartset.Unoptimized
        elif acc.Count = 0 then
            if _solver.IsEmpty(loopTerminator) then InitialStartset.Unoptimized
            else InitialStartset.MintermArrayPrefix([|loopTerminator|], loopTerminator)
        else
            InitialStartset.MintermArrayPrefix(acc.ToArray(), loopTerminator)




module rec Flags =

    let rec inferLoop(R, lower, upper) =
        match (R, lower, upper) with
        | _, 0, Int32.MaxValue ->
            RegexNodeFlags.CanBeNullable
            |> Flag.withFlag RegexNodeFlags.IsAlwaysNullable
            |> Flag.withFlag RegexNodeFlags.CanSkip
        | _, 0, _ -> RegexNodeFlags.CanBeNullable |> Flag.withFlag RegexNodeFlags.IsAlwaysNullable
        | _ -> inferNode R

    let inferAnd(xs: seq<RegexNode<'t>>) : RegexNodeFlags =
        // todo: can be optimized
        let inner = xs |> Seq.map inferNodeOptimized |> Seq.toArray

        let allCanBeNull = inner |> Array.forall (fun v -> v.HasFlag(Flag.CanBeNullable))

        let allCanSkip = inner |> Array.forall (fun v -> v.HasFlag(Flag.CanSkip))

        let allAlwaysNull = inner |> Array.forall (Flag.hasFlag Flag.IsAlwaysNullable)

        let isPrefix =
            inner |> Array.exists (Flag.hasFlag Flag.Prefix)
            && inner
               |> Array.forall (fun v -> Flag.hasFlag Flag.CanSkip v || Flag.hasFlag Flag.Prefix v)

        let newFlags =
            inner
            |> Array.map Flag.getContainsFlags
            |> Flag.mergeFlags
            |> Flag.withFlagIf allCanBeNull Flag.CanBeNullable
            |> Flag.withFlagIf allAlwaysNull Flag.IsAlwaysNullable
            |> Flag.withFlagIf allCanSkip Flag.CanSkip
            |> Flag.withFlagIf isPrefix Flag.Prefix


        newFlags

    let inferConcat (head: RegexNode<'t>) (tail: RegexNode<'t>) =

        // todo: can be optimized
        let infos = [| inferNodeOptimized head; inferNodeOptimized tail |] |> Seq.toArray

        let allCanBeNull = infos |> Array.forall (fun v -> v.HasFlag(Flag.CanBeNullable))

        let allAlwaysNull = infos |> Array.forall (Flag.hasFlag Flag.IsAlwaysNullable)

        let canSkipHead = infos[0] |> Flag.hasFlag Flag.CanSkip

        let isPrefix =
            match head, tail with
            | Singleton _, (Singleton _ | Concat(head = Singleton _)) -> true
            | _, Concat(info = info) when canSkipHead && info.Flags.HasFlag(Flag.Prefix) -> true
            | _ -> false

        let newFlags =
            infos
            |> Seq.map Flag.getContainsFlags
            |> Flag.mergeFlags
            |> Flag.withFlagIf allCanBeNull Flag.CanBeNullable
            |> Flag.withFlagIf allAlwaysNull Flag.IsAlwaysNullable
            |> Flag.withFlagIf canSkipHead Flag.CanSkip
            |> Flag.withFlagIf isPrefix Flag.Prefix

        newFlags


    let inferNodeOptimized(node: RegexNode<'t>) : RegexNodeFlags =
        match node with
        | Concat(info = info) -> info.Flags
        | Epsilon -> Flag.CanBeNullable ||| Flag.IsAlwaysNullable ||| Flag.ContainsEpsilon
        | Or(nodes, info) -> info.Flags
        | Singleton foo -> Flag.None
        | Loop(node, low, up, info) -> info.Flags
        | And(nodes, info) -> info.Flags
        | Not(node, info) -> info.Flags
        | LookAround(node, lookBack, negate) -> Flag.CanBeNullable ||| Flag.ContainsLookaround


    let inferNot(inner: RegexNode<'t>) =
        let innerInfo = inferNodeOptimized inner
        // not nullable => always nullable
        let isAlwaysNullable =
            if innerInfo.HasFlag(RegexNodeFlags.IsAlwaysNullable) then false
            elif innerInfo.HasFlag(RegexNodeFlags.CanBeNullable) then false
            else true

        let merged =
            Flag.None
            |> Flag.withFlagIf isAlwaysNullable (Flag.IsAlwaysNullable ||| Flag.CanBeNullable)
            |> Flag.withFlagIf
                (Flag.hasFlag (Flag.CanBeNullable ||| Flag.ContainsLookaround) innerInfo)
                Flag.CanBeNullable
            |> Flag.withFlagIf
                (Flag.hasFlag Flag.ContainsLookaround innerInfo)
                Flag.ContainsLookaround
            |> Flag.withFlagIf (Flag.hasFlag Flag.ContainsEpsilon innerInfo) Flag.ContainsEpsilon
            |> Flag.withFlagIf (Flag.hasFlag Flag.CanSkip innerInfo) Flag.CanSkip

        merged

    let rec inferOr(xs: seq<RegexNode<'t>>) : RegexNodeFlags =
        let inner = xs |> Seq.map inferNodeOptimized |> Seq.toArray

        let existsCanBeNull = inner |> Array.exists (fun v -> v.HasFlag(Flag.CanBeNullable))

        let allCanSkip = inner |> Array.forall (fun v -> v.HasFlag(Flag.CanSkip))

        let existsAlwaysNull = inner |> Array.exists (Flag.hasFlag Flag.IsAlwaysNullable)

        let newFlags =
            inner
            |> Seq.map Flag.getContainsFlags
            |> Flag.mergeFlags
            |> Flag.withFlagIf existsCanBeNull Flag.CanBeNullable
            |> Flag.withFlagIf existsAlwaysNull Flag.IsAlwaysNullable
            |> Flag.withFlagIf allCanSkip Flag.CanSkip

        newFlags

    let rec inferNode(node: RegexNode<'t>) =
        match node with
        | Epsilon ->
            RegexNodeFlags.CanBeNullable
            |> Flag.withFlag RegexNodeFlags.IsAlwaysNullable
            |> Flag.withFlag RegexNodeFlags.ContainsEpsilon
        | Singleton _ -> RegexNodeFlags.None
        | Not(inner, info) -> inferNot inner
        // not nullable
        | Or(xs, info) -> inferOr xs
        | LookAround _ -> RegexNodeFlags.ContainsLookaround ||| RegexNodeFlags.CanBeNullable

        | Loop(node, low, up, info) -> inferLoop (node, low, up)

        | And(xs, info) -> inferAnd xs

        | Concat(head, tail, info) -> inferConcat head tail


let defaultInfo(solver: ISolver<'t>) : RegexNodeInfo<'t> = {
    Flags = Flag.None
    Startset = solver.Full
}

let convertLoop
    (
        xs: RegexNode<'t>,
        solver: ISolver<'t>,
        info: RegexNodeInfo<'b>
    )
    : RegexNodeInfo<'t>
    =
    let startset = Startset.inferStartset solver xs
    { Flags = info.Flags; Startset = startset }


[<AutoOpen>]
module Node =
    let inline isAlwaysNullable(node: RegexNode<'t>) =
        match node with
        | Or(info = info) -> info.IsAlwaysNullable
        | Singleton _ -> false
        | Loop(info = info) -> info.IsAlwaysNullable
        | And(info = info) -> info.IsAlwaysNullable
        | Not(info = info) -> info.IsAlwaysNullable
        | LookAround _ -> false
        | Concat(info = info) -> info.IsAlwaysNullable
        | Epsilon -> false

    let inline canBeNullable(node: RegexNode<'t>) =
        match node with
        | Or(info = info) -> info.CanBeNullable
        | Singleton _ -> false
        | Loop(info = info) -> info.CanBeNullable
        | And(info = info) -> info.CanBeNullable
        | Not(info = info) -> info.CanBeNullable
        | LookAround _ -> true
        | Concat(info = info) -> info.CanBeNullable
        | Epsilon -> true
