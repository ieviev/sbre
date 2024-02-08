module internal Sbre.Info

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

    let inline getContainsFlags(sourceFlags: RegexNodeFlags) =
        (RegexNodeFlags.ContainsEpsilonFlag ||| RegexNodeFlags.ContainsLookaroundFlag)
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
    match x.NodeFlags.HasFlag(RegexNodeFlags.CanBeNullableFlag) with
    | false -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|IsAlwaysNullable|_|)(x: RegexNodeInfo<'t>) =
    match x.NodeFlags.HasFlag(RegexNodeFlags.IsAlwaysNullableFlag) with
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
    | LookAround(node, lookBack, negate, _) -> ValueNone
    | _ -> ValueNone

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
    let rec inferLoop(R, lower, upper) =

        // counter flags
        // let innerFlags = inferNodeOptimized R
        // let counterFlags =
        //     match R, upper with
        //     // TODO: more monadic regexes
        //     // decide counter threshold
        //     | (Singleton _, upper) when
        //         upper <> Int32.MaxValue && upper > 1 ->
        //         // Flag.HasCounterFlag ||| Flag.IsCounterFlag
        //         Flag.None
        //     | _ ->
        //         if innerFlags.HasCounter then Flag.HasCounterFlag else
        //         Flag.None


        let nullableLoopFlag =
            match lower with
            | 0 -> RegexNodeFlags.CanBeNullableFlag ||| RegexNodeFlags.IsAlwaysNullableFlag
            | _ -> RegexNodeFlags.None

        inferNodeOptimized R ||| nullableLoopFlag

    let inferAnd(xs: seq<RegexNode<'t>>) : RegexNodeFlags =
        xs
        |> Seq.map inferNodeOptimized
        |> Seq.reduce (fun b f ->
            let orflags =
                (b ||| f) &&& (
                    Flag.ContainsEpsilonFlag ||| Flag.ContainsLookaroundFlag ||| Flag.HasCounterFlag)
            let andFlags =
                b &&& f &&& (Flag.CanBeNullableFlag ||| Flag.IsAlwaysNullableFlag)

            orflags ||| andFlags
        )


    let inferConcat (head: RegexNode<'t>) (tail: RegexNode<'t>) =
        let h1 = inferNodeOptimized head
        let t1 = inferNodeOptimized tail
        let orFlags = h1 ||| t1 &&& (Flag.ContainsEpsilonFlag ||| Flag.ContainsLookaroundFlag)
        let andFlags = h1 &&& t1 &&& (Flag.IsAlwaysNullableFlag||| Flag.CanBeNullableFlag)
        let dependsOnCounter = h1.HasCounter || (h1.CanBeNullable && t1.HasCounter)
        let isCounterFlag =
            match dependsOnCounter, h1.IsCounter with
            | true, true -> Flag.IsCounterFlag
            | _ -> Flag.None
        let counterFlags =
            match dependsOnCounter, t1.CanBeNullable with
            | true, true -> Flag.CanBeNullableFlag ||| Flag.HasCounterFlag
            | true, false -> Flag.HasCounterFlag
            | _ -> Flag.None
        andFlags ||| orFlags ||| counterFlags ||| isCounterFlag



    let inferNodeOptimized(node: RegexNode<'t>) : RegexNodeFlags =
        match node with
        | Concat(info = info) -> info.NodeFlags
        | Epsilon -> Flag.CanBeNullableFlag ||| Flag.IsAlwaysNullableFlag ||| Flag.ContainsEpsilonFlag
        | Or(nodes, info) -> info.NodeFlags
        | Singleton foo -> Flag.None
        | Loop(node, low, up, info) -> info.NodeFlags
        | And(nodes, info) -> info.NodeFlags
        | Not(node, info) -> info.NodeFlags
        | LookAround(node, lookBack, negate, _) -> Flag.CanBeNullableFlag ||| Flag.ContainsLookaroundFlag


    let inferNot(inner: RegexNode<'t>) =
        let innerInfo = inferNodeOptimized inner
        // not nullable => always nullable
        let nullableFlags =
            if not (innerInfo.HasFlag(RegexNodeFlags.CanBeNullableFlag)) then
                Flag.IsAlwaysNullableFlag ||| Flag.CanBeNullableFlag
            else
                if innerInfo.HasCounter && innerInfo.CanBeNullable then
                    Flag.CanBeNullableFlag else

                Flag.None
        let otherFlags =
            innerInfo &&& (Flag.ContainsEpsilonFlag ||| Flag.ContainsLookaroundFlag ||| Flag.HasCounterFlag)
        nullableFlags ||| otherFlags


    let rec inferOr(xs: seq<RegexNode<'t>>) : RegexNodeFlags =
        xs
        |> Seq.map inferNodeOptimized
        |> Seq.reduce (fun b f ->
            let orflags =
                (b ||| f) &&& (
                    Flag.CanBeNullableFlag ||| Flag.IsAlwaysNullableFlag ||| Flag.ContainsEpsilonFlag ||| Flag.ContainsLookaroundFlag ||| Flag.HasCounterFlag)
            orflags
        )


    let rec inferInitialFlags(node: RegexNode<'t>) =
        match node with
        | Epsilon ->
            Flag.CanBeNullableFlag ||| Flag.IsAlwaysNullableFlag ||| Flag.ContainsEpsilonFlag

        | Singleton _ -> RegexNodeFlags.None
        | Not(inner, _) -> inferNot inner
        // not nullable
        | Or(xs, _) -> inferOr xs
        | LookAround _ -> RegexNodeFlags.ContainsLookaroundFlag ||| RegexNodeFlags.CanBeNullableFlag

        | Loop(node, low, up, _) -> inferLoop (node, low, up)

        | And(xs, _) -> inferAnd xs

        | Concat(head, tail, _) -> inferConcat head tail


// let defaultInfo()(solver: ISolver<'t>) : RegexNodeInfo<'t> =
//
//
//     RegexNodeInfo<'t>(
//     NodeFlags = Flag.None,
// )



[<AutoOpen>]
module Node =
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


    let inline isAlwaysNullableV(vinfo: RegexNodeInfo<'t> voption, node:RegexNode<'t>) =
        match vinfo with
        | ValueSome info -> info.IsAlwaysNullable
        | ValueNone -> isAlwaysNullable node

    let inline canBeNullable(node: RegexNode<'t>) =
        match node with
        | Or(info = info) | Loop(info = info) | And(info = info) | Not(info = info) | Concat(info = info) ->
            info.NodeFlags.HasFlag(Flag.CanBeNullableFlag)
        | Singleton _ -> false
        | LookAround _ -> true
        | Epsilon -> true

    let inline canBeNullableV(vinfo: RegexNodeInfo<'t> voption, node:RegexNode<'t>)=
        match vinfo with
        | ValueSome info -> info.CanBeNullable
        | ValueNone -> canBeNullable node

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
