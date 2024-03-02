module
#if RELEASE
    internal
#endif
    Sbre.Info

open System
open Sbre.Types

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

    /// bitwise or for multiple flags
    let inline mergeFlags(sourceFlags: RegexNodeFlags seq) =
        Seq.fold (fun acc v -> acc ||| v) RegexNodeFlags.None sourceFlags

let inline removeFlag (flags: byref<RegexNodeFlags>) (flagsToRemove: RegexNodeFlags) =
    flags <- flags &&& ~~~flagsToRemove

let inline addFlag (flags: byref<RegexNodeFlags>) (flagsToAdd: RegexNodeFlags) =
    flags <- flags ||| flagsToAdd

let inline invertFlag (flags: byref<RegexNodeFlags>) (flagsToInvert: RegexNodeFlags) =
    if flags.HasFlag(flagsToInvert) then
        flags <- flags &&& ~~~flagsToInvert
    else
        flags <- flags ||| flagsToInvert


[<return: Struct>]
let (|HasPrefixLookaround|_|)(x: RegexNode<'t>) =
    match x with
    | Concat(head=LookAround(lookBack = true) as look; tail=tail) -> ValueSome(look,tail)
    | _ -> ValueNone

[<return: Struct>]
let rec (|HasSuffixLookaround|_|)(x: RegexNode<'t>) =
    let rec loop node =
        match node with
        | Concat(head=LookAround(lookBack = true); tail=_) -> ValueNone
        | Concat(head=_; tail=LookAround(lookBack = true) as look) -> ValueSome look
        | Concat(head=_; tail=HasSuffixLookaround(look)) -> ValueSome look
        | LookAround(lookBack=false) -> ValueSome node
        | _ -> ValueNone
    loop x


[<return: Struct>]
let (|CanBeNullable|_|)(x: RegexNodeInfo<'t>) =
    match x.NodeFlags.HasFlag(RegexNodeFlags.CanBeNullableFlag) with
    | true -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|ContainsLookaround|_|)(x: RegexNodeInfo<'t>) =
    match x.NodeFlags.HasFlag(RegexNodeFlags.ContainsLookaroundFlag) with
    | true -> ValueSome()
    | _ -> ValueNone

module rec Flags =
    let rec inferLoop(R, lower, _) =
        let nullableLoopFlag =
            match lower with
            | 0 ->
                RegexNodeFlags.CanBeNullableFlag
                ||| RegexNodeFlags.IsAlwaysNullableFlag
                ||| RegexNodeFlags.HasZerowidthHeadFlag
            | _ -> RegexNodeFlags.None
        inferNodeOptimized R ||| nullableLoopFlag


    let private getPrefixSuffixFlags node =
        let suf =
            match node with
            | Pat.HasSuffixLookahead -> Flag.HasSuffixLookaheadFlag
            | _ -> Flag.None
        let pref =
            match node with
            | Pat.HasPrefixLookback -> Flag.HasPrefixLookbehindFlag
            | _ -> Flag.None
        pref ||| suf



    let inferAnd(xs: seq<RegexNode<'t>>) : RegexNodeFlags =
        xs
        |> Seq.map inferNodeOptimized
        |> Seq.reduce (fun b f ->
            let orflags = (b ||| f) &&& (Flag.ContainsLookaroundFlag ||| Flag.DependsOnAnchorFlag)
            let andFlags = b &&& f &&& (Flag.CanBeNullableFlag ||| Flag.IsAlwaysNullableFlag ||| Flag.HasZerowidthHeadFlag)
            orflags ||| andFlags
        )
    let rec inferOr(xs: seq<RegexNode<'t>>) : RegexNodeFlags =
        xs
        |> Seq.map inferNodeOptimized
        |> Seq.reduce (fun b f ->
            let orflags = (b ||| f) &&& (
                    Flag.CanBeNullableFlag ||| Flag.IsAlwaysNullableFlag |||
                    Flag.ContainsLookaroundFlag ||| Flag.HasZerowidthHeadFlag |||
                    Flag.DependsOnAnchorFlag ||| Flag.HasSuffixLookaheadFlag ||| Flag.HasPrefixLookbehindFlag)
            orflags
        )
    let inferConcat (head: RegexNode<'t>) (tail: RegexNode<'t>) =
        let h1 = inferNodeOptimized head
        let t1 = inferNodeOptimized tail
        let orFlags = h1 ||| t1 &&& Flag.ContainsLookaroundFlag
        let andFlags = h1 &&& t1 &&& (Flag.IsAlwaysNullableFlag||| Flag.CanBeNullableFlag)
        let dependsOnAnchor = h1.DependsOnAnchor || (h1.CanBeNullable && t1.DependsOnAnchor)
        let dependsOnFlags =
            if dependsOnAnchor then
                Flag.DependsOnAnchorFlag
            else Flag.None
        let lookaroundFlags =
            let suf =
                match tail with
                | Pat.HasSuffixLookahead -> Flag.HasSuffixLookaheadFlag
                | _ -> Flag.None
            let pref =
                match head with
                | Pat.HasPrefixLookback -> Flag.HasPrefixLookbehindFlag
                | _ -> Flag.None
            pref ||| suf

        andFlags ||| orFlags ||| dependsOnFlags ||| lookaroundFlags ||| (h1 &&& Flag.HasZerowidthHeadFlag)

    let inferLookaround (inner: RegexNode<'t>) (lookBack: bool) =
        let innerFlags = inner.GetFlags()
        let nullFlags = innerFlags &&& (RegexNodeFlags.CanBeNullableFlag ||| RegexNodeFlags.IsAlwaysNullableFlag)
        let ancFlag = innerFlags &&& RegexNodeFlags.DependsOnAnchorFlag
        match innerFlags.HasFlag(RegexNodeFlags.CanBeNullableFlag), lookBack with
        // nullable lookahead
        | true, false ->
            nullFlags |||
            RegexNodeFlags.ContainsLookaroundFlag |||
            RegexNodeFlags.HasZerowidthHeadFlag |||
            RegexNodeFlags.HasSuffixLookaheadFlag |||
            ancFlag
        // non nullable lookahead
        | false, false ->
            nullFlags |||
            RegexNodeFlags.ContainsLookaroundFlag |||
            RegexNodeFlags.HasZerowidthHeadFlag |||
            RegexNodeFlags.HasSuffixLookaheadFlag |||
            ancFlag
        // lookback
        | _, true ->
            ancFlag |||
            nullFlags |||
            RegexNodeFlags.ContainsLookaroundFlag |||
            RegexNodeFlags.HasPrefixLookbehindFlag |||
            RegexNodeFlags.HasZerowidthHeadFlag

    let inferNodeOptimized(node: RegexNode<'t>) : RegexNodeFlags =
        match node with
        | Concat(info = info) -> info.NodeFlags
        | Epsilon -> node.GetFlags()
        | Or(_, info) -> info.NodeFlags
        | Singleton _ -> node.GetFlags()
        | Loop(_, _, _, info) -> info.NodeFlags
        | And(_, info) -> info.NodeFlags
        | Not(_, info) -> info.NodeFlags
        | LookAround(info=info) -> info.NodeFlags
        | Begin | End -> node.GetFlags()

    let inferNot(inner: RegexNode<'t>) =
        let innerInfo = inferNodeOptimized inner
        // not nullable => always nullable
        let nullableFlags =
            if not (innerInfo.HasFlag(RegexNodeFlags.CanBeNullableFlag)) then
                Flag.IsAlwaysNullableFlag ||| Flag.CanBeNullableFlag
            else
                Flag.None
        let otherFlags =
            innerInfo &&& (Flag.HasZerowidthHeadFlag ||| Flag.ContainsLookaroundFlag ||| Flag.DependsOnAnchorFlag)
        nullableFlags ||| otherFlags






[<AutoOpen>]
module Node =
    let rec getFixedLength (node: RegexNode<_>) =
        let rec loop (acc:int) node : int option =
            match node with
            | Concat(head, tail, _) ->
                loop acc head
                |> Option.bind (fun headLen ->
                    loop headLen tail
                )
            | Epsilon -> Some (0 + acc)
            | Or(nodes, _) | And(nodes, _) ->
                let sameLen =
                    nodes
                    |> Seq.map (loop 0)
                    |> Seq.distinct
                    |> Seq.toArray
                if sameLen.Length = 1 && sameLen[0].IsSome then
                    Some (acc + sameLen[0].Value)
                else None
            | Singleton _ -> Some (1 + acc)
            | Loop(Singleton _, low, up, _) ->
                if low = up then Some (low + acc) else None
            | Loop _ -> None
            | Not _ -> None
            | LookAround _ -> Some (0 + acc)
            | Begin | End -> Some (0 + acc)
        loop 0 node

    let rec getMinLength (node: RegexNode<_>) =
        let rec loop (acc:int) node : int option =
            match node with
            | Concat(head, tail, _) ->
                loop acc head
                |> Option.bind (fun headLen ->
                    loop headLen tail
                )
            | Epsilon -> Some (0 + acc)
            | Or(nodes, _) | And(nodes, _) ->
                let sameLen =
                    nodes
                    |> Seq.map (loop 0)
                    |> Seq.distinct
                    |> Seq.toArray
                if sameLen.Length = 1 && sameLen[0].IsSome then
                    Some (acc + sameLen[0].Value)
                else None
            | Singleton _ -> Some (1 + acc)
            | Loop(Singleton _, low, _, _) -> Some (low + acc)
            | Loop _ -> None
            | Not _ -> None
            | LookAround _ -> Some (0 + acc)
            | Begin | End -> Some (0 + acc)
        loop 0 node



    let rec containsRecursive (orNodes:NodeSet<'t>) (node: RegexNode<'t>)  =
        if orNodes.Contains(node) then true else
        orNodes
        |> Seq.exists (fun orNode ->
            match orNode with
            | Or(nodes=innerNodes) ->
                containsRecursive innerNodes node
            | _ -> false
        )


    let inline isAlwaysNullable(node: RegexNode<'t>) =
        match node with
        | Or(info = info)
        | Loop(info = info)
        | And(info = info)
        | Not(info = info)
        | Concat(info = info) -> info.IsAlwaysNullable
        | Singleton _ -> false
        | LookAround _ -> false
        | Epsilon -> false
        | Begin | End -> false


    let inline isAlwaysNullableV(vinfo: RegexNodeInfo<'t> voption, node:RegexNode<'t>) =
        match vinfo with
        | ValueSome info -> info.IsAlwaysNullable
        | ValueNone -> isAlwaysNullable node

    let inline canBeNullable(node: RegexNode<'t>) =
        match node with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info) ->
            info.NodeFlags.CanBeNullable
        | Singleton _ -> false
        | LookAround _ -> true
        | Epsilon -> true
        | Begin | End -> true

    let inline canBeNullableV(vinfo: RegexNodeInfo<'t> voption, node:RegexNode<'t>)=
        match vinfo with
        | ValueSome info -> info.CanBeNullable
        | ValueNone -> canBeNullable node

    let inline containsLookaround(node: RegexNode<'t>) =
        match node with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info) ->
            info.ContainsLookaround
        | Singleton _ -> false
        | LookAround _ -> true
        | Epsilon -> false
        | Begin | End -> false

    let inline canNotBeNullable(node: RegexNode<'t>) =
        not (canBeNullable node)
