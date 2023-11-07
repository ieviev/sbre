module internal Sbre.Info

open System
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open Pat




type Flag = RegexNodeFlags

module Flag =
    let inline withFlag (arg: RegexNodeFlags) (sourceFlags: RegexNodeFlags) =
        sourceFlags
        ||| arg

    let inline withoutFlag (arg: RegexNodeFlags) (sourceFlags: RegexNodeFlags) =
        sourceFlags
        &&& ~~~arg

    let inline withFlagIf (cond: bool) (arg: RegexNodeFlags) (sourceFlags: RegexNodeFlags) =
        if cond then
            sourceFlags
            ||| arg
        else
            sourceFlags

    let inline hasFlag (arg: RegexNodeFlags) (sourceFlags: RegexNodeFlags) =
        sourceFlags.HasFlag(arg)

    let inline getContainsFlags (sourceFlags: RegexNodeFlags) =
        (RegexNodeFlags.ContainsEpsilon
         ||| RegexNodeFlags.ContainsLookaround)
        &&& sourceFlags

    /// bitwise or for multiple flags
    let inline mergeFlags (sourceFlags: RegexNodeFlags seq) =
        (RegexNodeFlags.None, sourceFlags)
        ||> Seq.fold (fun acc v -> acc ||| v)

// accessors and common functions
let rec canSkip (node: RegexNode<'t>) : bool =
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
    flags <-
        flags
        &&& ~~~flagsToRemove

let inline addFlag (flags: byref<RegexNodeFlags>) (flagsToAdd: RegexNodeFlags) =
    flags <-
        flags
        ||| flagsToAdd

let inline invertFlag (flags: byref<RegexNodeFlags>) (flagsToInvert: RegexNodeFlags) =
    if flags.HasFlag(flagsToInvert) then
        flags <-
            flags
            &&& ~~~flagsToInvert
    else
        flags <-
            flags
            ||| flagsToInvert



// Patterns
[<return: Struct>]
let (|CanNotBeNullable|_|) (x: RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.CanBeNullable) with
    | false -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|IsAlwaysNullable|_|) (x: RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) with
    | true -> ValueSome()
    | _ -> ValueNone


[<return: Struct>]
let (|NodeIsAlwaysNullable|_|) (x: RegexNode<'t>) =
    match x with
    | Or(xs, IsAlwaysNullable) -> ValueSome()
    | Singleton foo -> ValueNone
    | Loop(node, low, up, IsAlwaysNullable) -> ValueSome()
    | And(xs, IsAlwaysNullable) -> ValueSome()
    | Not(fSharpList, IsAlwaysNullable) -> ValueSome()
    | LookAround(node, lookBack, negate) -> ValueNone
    | _ -> ValueNone



[<return: Struct>]
let (|CanBeNullable|_|) (x: RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.CanBeNullable) with
    | true -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|ContainsLookaround|_|) (x: RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.ContainsLookaround) with
    | true -> ValueSome()
    | _ -> ValueNone


module rec Startset =

    let inline inferMergeStartset (_solver: ISolver<'t>) (nodes: seq<RegexNode<'t>>) =
        // todo: small optimization here
        nodes
        |> Solver.mapOr _solver (inferStartset _solver)


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
                _solver.Or(headss,tail.Startset)






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
                _solver.Or(headss,tail.Startset)


        | Concat(chead, ctail, info) ->
            inferConcatStartset _solver chead ctail

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
        | Concat(Loop(node=Singleton pred;low=0;up=Int32.MaxValue), Concat(Singleton chead,concatTail2,_), info) ->

            if _solver.IsFull(pred) then
                match concatTail2 with
                | Concat(Loop(node=loopBody;low=0;up=up) as loop,tail,_) ->

                    _solver.Or(_solver.Not(loop.Startset), tail.Startset)
                | _ -> concatTail2.Startset
            else
                if concatTail2.CanNotBeNullable then
                    // .*air -> .* only has 1 char startset
                    _solver.Full

                else
                if concatTail2.IsAlwaysNullable then // negation startset inference test
                    _solver.Or(inferStartset2 _solver concatTail2, concatTail2.Startset)
                else
                // failwith "todo1"
                _solver.Full // .* not optimized

        | Concat(Not(node=inner;info=notInfo), Singleton pred, info) ->
            let ss1 = inferStartset2 _solver inner
            _solver.Or(ss1,pred) // 4x performance win
        | Concat(Not(node=inner;info=notInfo), tailNode, info) ->

            let ss2 = tailNode.Startset
            let ss22 = inferStartset2 _solver tailNode
            let ss1 = inferStartset2 _solver inner
            _solver.Or(_solver.Or(ss1,ss2),ss22) // negation test 8
            // _solver.Full

        | Concat(Singleton hpred, t, info) ->

            match t with
            | Not(_) -> _solver.Full
            | _ ->
                // inferStartset2 _solver
                t.Startset
            // _solver.Full // todo: (.*dogs.*&and.*), (a.*&~(.*b.*)b)
        | Concat(Concat(h1,h2,_) as c1, tailNode, info) when h1.CanNotBeNullable ->
            let ss1 = inferStartset2 _solver h1
            let ss22 = h2.Startset
            let ss2 = tailNode.Startset
            _solver.Or(_solver.Or(ss1,ss2),ss22) // negation test 8
        // ⊤*
        | Concat(head=Loop(node=Singleton pred);tail=tail) when _solver.IsFull(pred) ->
            inferStartset2 _solver tail

        | Concat(h, t, info) ->
            if h.CanNotBeNullable then // regexlib 20-30
                let ss1 = inferStartset2 _solver h
                let ss22 = inferStartset2 _solver t
                let ss2 = t.Startset
                _solver.Or(_solver.Or(ss1,ss2),ss22) // negation test 8
            else
                // failwith "todo2" // TODO: optimize further
                _solver.Full

        | And(xs, info) ->
            use mutable e = xs.GetEnumerator()
            Solver.mergeOrWithEnumerator _solver (inferStartset2 _solver) &e
            // Solver.mergeNonFullWithEnumerator _solver (inferStartset2 _solver) &e

        // todo: correct?
        // | Loop(low=0; up=Int32.MaxValue) -> _solver.Empty
        // TODO: how to optimize (a|ab)*

        | Loop(_)
        | Epsilon
        | Singleton _ -> _solver.Full




module rec Flags =

    let rec inferLoop (R, lower, upper) =
        match (R, lower, upper) with
        | _, 0, Int32.MaxValue ->
            RegexNodeFlags.CanBeNullable
            |> Flag.withFlag RegexNodeFlags.IsAlwaysNullable
            |> Flag.withFlag RegexNodeFlags.CanSkip
        | _, 0, _ ->
            RegexNodeFlags.CanBeNullable
            |> Flag.withFlag RegexNodeFlags.IsAlwaysNullable
        | _ -> inferNode R

    let inferAnd (xs: seq<RegexNode<'t>>) : RegexNodeFlags =
        // todo: can be optimized
        let inner =
            xs
            |> Seq.map inferNodeOptimized
            |> Seq.toArray

        let allCanBeNull =
            inner
            |> Array.forall (fun v -> v.HasFlag(Flag.CanBeNullable))

        let allCanSkip =
            inner
            |> Array.forall (fun v -> v.HasFlag(Flag.CanSkip))

        let allAlwaysNull =
            inner
            |> Array.forall (Flag.hasFlag Flag.IsAlwaysNullable)

        let newFlags =
            inner
            |> Array.map Flag.getContainsFlags
            |> Flag.mergeFlags
            |> Flag.withFlagIf allCanBeNull Flag.CanBeNullable
            |> Flag.withFlagIf allAlwaysNull Flag.IsAlwaysNullable
            |> Flag.withFlagIf allCanSkip Flag.CanSkip

        newFlags

    let inferConcat (head: RegexNode<'t>) (tail: RegexNode<'t>) =
        // todo: can be optimized
        let infos =
            [|
                inferNodeOptimized head
                inferNodeOptimized tail
            |]
            // |> Seq.map inferNode
            // |> Seq.map inferNodeOptimized
            |> Seq.toArray

        let allCanBeNull =
            infos
            |> Array.forall (fun v -> v.HasFlag(Flag.CanBeNullable))

        let allAlwaysNull =
            infos
            |> Array.forall (Flag.hasFlag Flag.IsAlwaysNullable)

        let canSkipHead =
            infos[0]
            |> Flag.hasFlag Flag.CanSkip

        let newFlags =
            infos
            |> Seq.map Flag.getContainsFlags
            |> Flag.mergeFlags
            |> Flag.withFlagIf allCanBeNull Flag.CanBeNullable
            |> Flag.withFlagIf allAlwaysNull Flag.IsAlwaysNullable
            |> Flag.withFlagIf canSkipHead Flag.CanSkip

        newFlags


    let inferNodeOptimized (node: RegexNode<'t>) : RegexNodeFlags =
        match node with
        | Concat(info = info) -> info.Flags
        | Epsilon ->
            Flag.CanBeNullable
            ||| Flag.IsAlwaysNullable
            ||| Flag.ContainsEpsilon
        | Or(nodes, info) -> info.Flags
        | Singleton foo -> Flag.None
        | Loop(node, low, up, info) -> info.Flags
        | And(nodes, info) -> info.Flags
        | Not(node, info) -> info.Flags
        | LookAround(node, lookBack, negate) ->
            Flag.CanBeNullable
            ||| Flag.ContainsLookaround


    let inferNot (inner: RegexNode<'t>) =
        let innerInfo = inferNodeOptimized inner
        // not nullable => always nullable
        let isAlwaysNullable =
            if innerInfo.HasFlag(RegexNodeFlags.IsAlwaysNullable) then
                false
            elif innerInfo.HasFlag(RegexNodeFlags.CanBeNullable) then
                false
            else
                true

        let merged =
            Flag.None
            |> Flag.withFlagIf
                isAlwaysNullable
                (Flag.IsAlwaysNullable
                 ||| Flag.CanBeNullable)
            |> Flag.withFlagIf
                (Flag.hasFlag
                    (Flag.CanBeNullable
                     ||| Flag.ContainsLookaround)
                    innerInfo)
                Flag.CanBeNullable
            |> Flag.withFlagIf
                (Flag.hasFlag Flag.ContainsLookaround innerInfo)
                Flag.ContainsLookaround
            |> Flag.withFlagIf (Flag.hasFlag Flag.ContainsEpsilon innerInfo) Flag.ContainsEpsilon
            |> Flag.withFlagIf (Flag.hasFlag Flag.CanSkip innerInfo) Flag.CanSkip

        merged

    let rec inferOr (xs: seq<RegexNode<'t>>) : RegexNodeFlags =
        let inner =
            xs
            |> Seq.map inferNodeOptimized
            |> Seq.toArray

        let existsCanBeNull =
            inner
            |> Array.exists (fun v -> v.HasFlag(Flag.CanBeNullable))

        let allCanSkip =
            inner
            |> Array.forall (fun v -> v.HasFlag(Flag.CanSkip))

        let existsAlwaysNull =
            inner
            |> Array.exists (Flag.hasFlag Flag.IsAlwaysNullable)

        let newFlags =
            inner
            |> Seq.map Flag.getContainsFlags
            |> Flag.mergeFlags
            |> Flag.withFlagIf existsCanBeNull Flag.CanBeNullable
            |> Flag.withFlagIf existsAlwaysNull Flag.IsAlwaysNullable
            |> Flag.withFlagIf allCanSkip Flag.CanSkip

        newFlags

    let rec inferNode (node: RegexNode<'t>) =
        match node with
        | Epsilon ->
            RegexNodeFlags.CanBeNullable
            |> Flag.withFlag RegexNodeFlags.IsAlwaysNullable
            |> Flag.withFlag RegexNodeFlags.ContainsEpsilon
        | Singleton _ -> RegexNodeFlags.None
        | Not(inner, info) -> inferNot inner
        // not nullable
        | Or(xs, info) -> inferOr xs
        | LookAround _ ->
            RegexNodeFlags.ContainsLookaround
            ||| RegexNodeFlags.CanBeNullable

        | Loop(node, low, up, info) -> inferLoop (node, low, up)

        | And(xs, info) -> inferAnd xs

        | Concat(head, tail, info) -> inferConcat head tail

// let ofFlagsAndStartset (flags, ss) = { Flags = flags; Startset = ss; }
// let createInfo (flags, ss, nodeId) = { Flags = flags; Startset = ss; }

let defaultInfo (solver: ISolver<'t>) : RegexNodeInfo<'t> =
    {
        Flags = Flag.None
        Startset = solver.Full
    }

let convertLoop
    (
        xs: RegexNode<'t>,
        solver: ISolver<'t>,
        info: RegexNodeInfo<'b>
    ) : RegexNodeInfo<'t> =
    let startset = Startset.inferStartset solver xs
    { Flags = info.Flags; Startset = startset; }


[<AutoOpen>]
module Node =
    let inline isAlwaysNullable (node:RegexNode<'t>) =
        match node with
        | Or(info = info) -> info.IsAlwaysNullable
        | Singleton _ -> false
        | Loop(info = info) -> info.IsAlwaysNullable
        | And(info = info) -> info.IsAlwaysNullable
        | Not(info = info) -> info.IsAlwaysNullable
        | LookAround _ -> false
        | Concat(info = info) -> info.IsAlwaysNullable
        | Epsilon -> false

    let inline canBeNullable (node:RegexNode<'t>) =
        match node with
        | Or(info = info) -> info.CanBeNullable
        | Singleton _ -> false
        | Loop(info = info) -> info.CanBeNullable
        | And(info = info) -> info.CanBeNullable
        | Not(info = info) -> info.CanBeNullable
        | LookAround _ -> true
        | Concat(info = info) -> info.CanBeNullable
        | Epsilon -> true


