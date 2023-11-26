module internal Sbre.Info

open System
open System.Runtime.InteropServices
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
        // todo: small optimization possible here
        Solver.mapOr _solver (inferStartset _solver) nodes

    let rec inferConcatStartset (_solver: ISolver<'t>) (head: RegexNode<'t>) (tail: RegexNode<'t>) =
        match head with
        | Loop(node = Singleton pred; low = 0; up = Int32.MaxValue) ->
            let tailStartset = inferStartset _solver tail
            let invertedPred = _solver.Not(pred)
            _solver.Or(invertedPred, tailStartset)
        | Loop(node = node; low = low; up = up) ->
            let inner = inferStartset _solver node

            match low with
            | 0 -> _solver.Full // TODO: optimize
            | _ -> inner
        | Singleton pred -> pred
        | Or(xs, info) ->
            use mutable e = xs.GetEnumerator()

            if info.CanNotBeNullable() then
                Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e
            else
                let headss = Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e
                _solver.Or(headss, tail.Startset)
        | Not(node, info) ->
            let tailStartset = inferStartset _solver tail
            let headStartset = inferStartset _solver node
            let merged = _solver.Or(headStartset, tailStartset)
            merged
        | LookAround _ -> _solver.Full
        | Epsilon -> inferStartset _solver tail
        | And(xs, info) ->
            use mutable e = xs.GetEnumerator()

            if info.CanNotBeNullable() then
                Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e
            else
                let headss = Solver.mergeOrWithEnumerator _solver (inferStartset _solver) &e
                _solver.Or(headss, tail.Startset)


        | Concat(chead, ctail, info) -> inferConcatStartset _solver chead ctail

    let inline inferLoopStartset (_solver: ISolver<'t>) struct (R, low, up) =
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


    let mergePrefixes
        (_solver: ISolver<'t>)
        (prefix1: ResizeArray<'t>)
        (prefix2: Memory<'t>)
        =
        // initialize if needed
        if prefix1.Count = 0 then
            while prefix1.Count < prefix2.Length do
                prefix1.Add(_solver.Empty)

        // shorten to shared prefix
        if prefix1.Count > prefix2.Length then
            while prefix1.Count > prefix2.Length do
                prefix1.RemoveAt(prefix1.Count - 1)

        for i = 0 to prefix1.Count - 1 do
            let ss = prefix2.Span[i]
            prefix1[i] <- _solver.Or(ss, prefix1[i])


    let mergePrefixesArr (_solver: ISolver<'t>) (prefix1: Memory<'t>) (prefix2: Memory<'t>) =
        let longer, shorter =
            match prefix1.Length < prefix2.Length with
            | true -> prefix2, prefix1
            | _ -> prefix1, prefix2

        for i = 0 to shorter.Length - 1 do
            shorter.Span[i] <- _solver.Or(longer.Span[i], shorter.Span[i])

        shorter


    /// can't be used during-match as it would skip newlines in .*
    let rec inferInitialStartset (_solver: ISolver<'t>) (startNode: RegexNode<'t>) =
        let mutable acc = ResizeArray()
        let loopterminatorPrefix = ResizeArray()
        let mutable uninitialized = true
        let mutable cannotOptimizeYet = false
        let mutable curr = startNode
        // let mutable loopTerminator = _solver.Empty

        let getPrefix node =
            match inferInitialStartset _solver node with
            | MintermArrayPrefix(arr, loopEnd) -> (arr, loopEnd)
            | _ -> failwithf "todo"

        while not (obj.ReferenceEquals(null, curr)) do
            match curr with
            | Loop(node = Singleton pred; info = info) when uninitialized ->
                if not (_solver.IsFull(pred)) then
                    loopterminatorPrefix.Clear()
                    loopterminatorPrefix.Add(_solver.Not(pred))
                    acc.Add(_solver.Not(pred))

                curr <- Unchecked.defaultof<_>
            | Concat(Not(node = node; info = info), tail, info2) when
                uninitialized && info.HasPrefix
                ->
                // 1.20
                match getPrefix node, getPrefix tail with
                | (arr1, loopEnd1), (arr2, loopEnd2) ->
                    let merged = mergePrefixesArr _solver arr1 arr2
                    mergePrefixes _solver acc merged
                    ()

                curr <- Unchecked.defaultof<_>
            | Not(node = node; info = info) when uninitialized && info.HasPrefix ->
                match inferInitialStartset _solver node with
                | MintermArrayPrefix(arr, loopEnd) ->
                    while acc.Count < arr.Length do
                        acc.Add(_solver.Empty)

                    for i = 0 to arr.Length - 1 do
                        let ss = arr.Span[i]
                        acc[i] <- _solver.Or(ss, acc[i])
                | _ -> ()

                curr <- Unchecked.defaultof<_>
            | Concat(Loop(node = Singleton pred; low = 0; up = Int32.MaxValue), t, info) ->
                if not uninitialized then
                    curr <- Unchecked.defaultof<_>
                else
                    if not (_solver.IsFull(pred)) then
                        if loopterminatorPrefix.Count = 1 then
                            loopterminatorPrefix[0] <-
                                _solver.Or(loopterminatorPrefix[0], _solver.Not(pred))
                        else
                            loopterminatorPrefix.Clear()
                            loopterminatorPrefix.Add(_solver.Not(pred))

                    curr <- t

            | Singleton pred ->
                acc.Add(pred)
                curr <- Unchecked.defaultof<_>
            // TODO: \n\n~(⊤*\n\n⊤*)\n\n&

            // ~(⊤*\n\n⊤*)\n
            // | Concat(head=Not(_) as head; tail=tail) when head.HasPrefix ->
            //     curr <- head

            | LookAround _
            | Not _ -> curr <- Unchecked.defaultof<_> // ignore
            | Concat(Singleton pred, t, info) ->
                acc.Add(pred)
                curr <- t
            // top level or optimizations
            | Or(xs, info) ->
                use mutable e = xs.GetEnumerator()

                while e.MoveNext() do
                    match e.Current with
                    // if there's something else to match
                    // negations apply only after a match has started
                    | Not(node = neg)
                    | Concat(head = Not(node = neg)) -> ()
                    // loopTerminator <- _solver.Or(loopTerminator,_solver.Not(neg.Startset))
                    | Loop(node = Singleton pred; low = 0; up = Int32.MaxValue) ->
                        // ignore true-stars, add \n terminator for dot-stars
                        if not (_solver.IsFull(pred)) then
                            if loopterminatorPrefix.Count = 1 then
                                loopterminatorPrefix[0] <-
                                    _solver.Or(loopterminatorPrefix[0], _solver.Not(pred))
                            else
                                loopterminatorPrefix.Clear()
                                loopterminatorPrefix.Add(_solver.Not(pred))
                    // ()
                    | LookAround _ -> () // ignore
                    | Loop _ -> cannotOptimizeYet <- true
                    | Concat _
                    | Or(_)
                    | And(_) ->
                        match inferInitialStartset _solver e.Current with
                        | MintermArrayPrefix(arr, innerLoopTerminator) ->
                            if innerLoopTerminator.Length > 0 then
                                mergePrefixes _solver loopterminatorPrefix innerLoopTerminator

                            mergePrefixes _solver acc arr
                        | InitialStartset.Unoptimized ->
                            cannotOptimizeYet <- true
                        | InitialStartset.Uninitialized -> failwith "todo"
                    | Singleton pred ->
                        mergePrefixes _solver acc ([|pred|].AsMemory())
                    | Epsilon ->
                        mergePrefixes _solver acc ([||].AsMemory())

                curr <- Unchecked.defaultof<_>
            // and
            | And(xs, info) ->
                use mutable e = xs.GetEnumerator()

                while e.MoveNext() do
                    match e.Current with
                    // if there's something else to match
                    // negations apply only after a match has started
                    | Not(node = neg)
                    | Concat(head = Not(node = neg)) ->
                        match inferInitialStartset _solver e.Current with
                        | MintermArrayPrefix(arr, _) ->
                            // add negation to stop loops
                            mergePrefixes _solver loopterminatorPrefix arr

                        | _ -> ()

                    | Loop(node = node; low = low; up = up) ->
                        // ignore true-stars, add \n terminator for dot-stars
                        match node, low, up with
                        | Singleton pred, 0, Int32.MaxValue ->
                            if not (_solver.IsFull(pred)) then
                                mergePrefixes _solver loopterminatorPrefix ([| _solver.Not(pred) |].AsMemory())
                        | _ -> cannotOptimizeYet <- true
                    // ()
                    | LookAround(_) -> () // ignore
                    | Concat(_)
                    | Or(_)
                    | And(_) ->
                        match inferInitialStartset _solver e.Current with
                        | MintermArrayPrefix(prefix = arr; loopTerminator = innerLoopTerminator) ->
                            if innerLoopTerminator.Length > 0 then
                                mergePrefixes _solver loopterminatorPrefix innerLoopTerminator

                            mergePrefixes _solver acc arr

                        | InitialStartset.Unoptimized -> cannotOptimizeYet <- true
                        | InitialStartset.Uninitialized -> failwith "todo"
                    | _ -> cannotOptimizeYet <- true

                // only negations in AND
                if acc.Count = 0 then
                    e.Reset()
                    let merged = Solver.mergeOrWithEnumerator _solver (fun v -> v.Startset) &e
                    acc.Add(merged)

                curr <- Unchecked.defaultof<_>
            | Concat(head, tail, info) ->
                if uninitialized then
                    acc.Add(curr.Startset)

                match head with
                // \d{2,2}
                | Loop(Singleton pred, low, up, regexNodeInfo) ->
                    mergePrefixes _solver acc ((Array.init low (fun v -> pred)).AsMemory())
                    curr <- Unchecked.defaultof<_>

                | _ ->

                curr <- Unchecked.defaultof<_>
            // failwith $"todo1 {head} {tail}"
            | Epsilon -> curr <- Unchecked.defaultof<_>
            | Loop(node, low, up, info) ->
                // ending ⊤* not optimized-
                if uninitialized then
                    acc.Add(node.Startset)

                curr <- Unchecked.defaultof<_>


            uninitialized <- false

        if cannotOptimizeYet then
#if OPTIMIZE
            [| startNode |]
            // |> Array.map (fun v -> v.ToString())
            |> Array.map (fun v -> v.ToString())
            |> String.concat "\n"
            |> failwithf "cannot optimize initial startset:\n%A"
#endif
            InitialStartset.Unoptimized
        elif acc.Count = 0 then
            if loopterminatorPrefix.Count = 0 then
                InitialStartset.Unoptimized
            else
                let arr = loopterminatorPrefix.ToArray()
                InitialStartset.MintermArrayPrefix(arr.AsMemory(), [||].AsMemory())
        else
            // if acc.Count = loopterminatorPrefix.Count then
            //     let sp1 = Memory(acc.ToArray())
            //     let sp2 = Memory(loopterminatorPrefix.ToArray())
            //     let merged = mergePrefixesArr _solver sp1 sp2
            //     InitialStartset.MintermArrayPrefix(merged, [||].AsMemory())
            // else

            InitialStartset.MintermArrayPrefix(acc.ToArray().AsMemory(), loopterminatorPrefix.ToArray().AsMemory())




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
        xs
        |> Seq.map inferNodeOptimized
        |> Seq.reduce (fun b f ->
            let orflags =
                (b ||| f) &&& (
                    Flag.ContainsEpsilon ||| Flag.ContainsLookaround)
            let andFlags =
                b &&& f &&& (Flag.CanSkip ||| Flag.Prefix ||| Flag.CanBeNullable ||| Flag.IsAlwaysNullable)

            orflags ||| andFlags
        )


    let inferConcat (head: RegexNode<'t>) (tail: RegexNode<'t>) =
        let h1 = inferNodeOptimized head
        let t1 = inferNodeOptimized tail
        let orFlags = h1 ||| t1 &&& (Flag.ContainsEpsilon ||| Flag.ContainsLookaround)
        let andFlags = h1 &&& t1 &&& (Flag.IsAlwaysNullable||| Flag.CanBeNullable)
        let prefixFlag =
            let canSkipHead = h1.HasFlag Flag.CanSkip
            match head, tail with
            | Singleton _, (Singleton _ | Concat(head = Singleton _)) -> Flag.Prefix
            | _ -> (h1 &&& (Flag.Prefix ||| Flag.CanSkip)) ||| (t1 &&& Flag.Prefix)

        prefixFlag ||| andFlags ||| orFlags



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
        let nullableFlags =
            if not (innerInfo.HasFlag(RegexNodeFlags.CanBeNullable)) then
                Flag.IsAlwaysNullable ||| Flag.CanBeNullable
            else Flag.None
        let otherFlags =
            innerInfo &&& (Flag.ContainsEpsilon ||| Flag.ContainsLookaround ||| Flag.CanSkip ||| Flag.Prefix)
        nullableFlags ||| otherFlags


    let rec inferOr(xs: seq<RegexNode<'t>>) : RegexNodeFlags =
        xs
        |> Seq.map inferNodeOptimized
        |> Seq.reduce (fun b f ->
            let orflags =
                (b ||| f) &&& (
                    Flag.CanBeNullable ||| Flag.IsAlwaysNullable ||| Flag.ContainsEpsilon ||| Flag.ContainsLookaround)
            let andFlags =
                b &&& f &&& (Flag.CanSkip ||| Flag.Prefix)

            orflags ||| andFlags
        )


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


let defaultInfo()(solver: ISolver<'t>) : RegexNodeInfo<'t> = RegexNodeInfo<'t>(
    Flags = Flag.None,
    Startset = solver.Full,
    InitialStartset = Uninitialized
)

let convertLoop
    (
        xs: RegexNode<'t>,
        solver: ISolver<'t>,
        info: RegexNodeInfo<'b>
    )
    : RegexNodeInfo<'t>
    =
    RegexNodeInfo<'t>(
        Flags = info.Flags,
        Startset = Startset.inferStartset solver xs,
        InitialStartset = Uninitialized
    )


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

    let inline isAlwaysNullableV(vinfo: RegexNodeInfo<'t> voption, node:RegexNode<'t>) =
        match vinfo with
        | ValueSome info -> info.IsAlwaysNullable
        | ValueNone -> isAlwaysNullable node

    let inline canBeNullable(node: RegexNode<'t>) =
        match node with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info) ->
            info.Flags.HasFlag(Flag.CanBeNullable)
        | Singleton _ -> false
        | LookAround _ -> true
        | Epsilon -> true

    let inline canBeNullableV(vinfo: RegexNodeInfo<'t> voption, node:RegexNode<'t>)=
        match vinfo with
        | ValueSome info -> info.CanBeNullable
        | ValueNone -> canBeNullable node

    let inline canSkip(node: RegexNode<'t>) =
        match node with
        | Or(info = info) -> info.CanSkip()
        | Singleton _ -> false
        | Loop(info = info) -> info.CanSkip()
        | And(info = info) -> info.CanSkip()
        | Not(info = info) -> info.CanSkip()
        | LookAround _ -> false
        | Concat(info = info) -> info.CanSkip()
        | Epsilon -> false

    let inline containsLookaround(node: RegexNode<'t>) =
        match node with
        | Or(info = info) -> info.ContainsLookaround
        | Singleton _ -> false
        | Loop(info = info) -> info.ContainsLookaround
        | And(info = info) -> info.ContainsLookaround
        | Not(info = info) -> info.ContainsLookaround
        | LookAround _ -> true
        | Concat(info = info) -> info.ContainsLookaround
        | Epsilon -> false

    let inline canNotBeNullable(node: RegexNode<'t>) =
        match node with
        | Or(info = info) -> not info.CanBeNullable
        | Singleton _ -> true
        | Loop(info = info) -> not info.CanBeNullable
        | And(info = info) -> not info.CanBeNullable
        | Not(info = info) -> not info.CanBeNullable
        | LookAround _ -> false
        | Concat(info = info) -> not info.CanBeNullable
        | Epsilon -> false
