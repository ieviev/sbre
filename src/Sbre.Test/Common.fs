module Sbre.Test.Common

#if DEBUG

open System
open Sbre
open Sbre.Algorithm
open Sbre.Pat
open Sbre.Types
open Xunit

let getRawFlags(reg: Regex) =
    try
        let matcher = reg.ByteMatcher
        matcher.RawPattern.TryGetInfo.Value.NodeFlags
    with e ->
        try
            let matcher = reg.UInt16Matcher
            matcher.RawPattern.TryGetInfo.Value.NodeFlags
        with e ->
            let matcher = reg.TSetMatcher
            matcher.RawPattern.TryGetInfo.Value.NodeFlags


let getDerImpl (reg: Regex) (input: string) =
    let location = (Location.create input 0)
    let state = RegexState()

    try
        let matcher = reg.TSetMatcher
        let cache = matcher.Cache
        let node = matcher.InitialPattern
        let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
        cache.PrettyPrintNode der1
    with e ->
        try
            let matcher = reg.ByteMatcher
            let cache = matcher.Cache
            let node = matcher.InitialPattern
            let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1
        with e ->
            let matcher = reg.UInt16Matcher
            let cache = matcher.Cache
            let node = matcher.InitialPattern
            let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1

let getDer1 (reg: Regex) (input: string) =
    let location = (Location.create input 0)
    let state = RegexState()
    try
        let matcher = reg.TSetMatcher
        let cache = matcher.Cache
        let node = matcher.RawPattern
        let der1 = createDerivative (cache, state,  &location, cache.MintermForLocation(location), node)
        cache.PrettyPrintNode der1
    with e ->
        try
            let matcher = reg.ByteMatcher
            let cache = matcher.Cache
            let node = matcher.RawPattern
            let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1
        with e ->
            let matcher = reg.UInt16Matcher
            let cache = matcher.Cache
            let node = matcher.RawPattern
            let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1

let getDerLoc (reg: Regex) (location: Location) =
    let state = RegexState()
    let matcher = reg.TSetMatcher
    let cache = matcher.Cache
    let node = matcher.RawPattern

    let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
    cache.PrettyPrintNode der1
    // try
    //     let matcher = reg.TSetMatcher
    //     let cache = matcher.Cache
    //     let node = matcher.RawPattern
    //
    //     let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
    //     cache.PrettyPrintNode der1
    // with e ->
    //     try
    //         let matcher = reg.ByteMatcher
    //         let cache = matcher.Cache
    //         let node = matcher.RawPattern
    //         let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
    //         cache.PrettyPrintNode der1
    //     with e ->
    //         let matcher = reg.UInt16Matcher
    //         let cache = matcher.Cache
    //         let node = matcher.RawPattern
    //         let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
    //         cache.PrettyPrintNode der1




let getRawPattern<'t when 't: struct and 't :> IEquatable<'t> and 't: equality>(matcher: Regex) =
    let matcher = matcher.Matcher :?> RegexMatcher<TSet>
    matcher.RawPattern :> obj


let assertPatternIn (expectedResults:string list) (node:RegexNode<TSet>) =
    let nodestr = node.ToString()
    Assert.Contains(nodestr , expectedResults)


let printRegexState (matcher:RegexMatcher<_>) (state:RegexState) (node:RegexNode<TSet>) (loc:string) =
    let nodestr =
        match node with
        | Or(nodes, info) ->
            nodes |> Seq.where (fun v -> not (refEq matcher.InitialPattern v))
            |> Seq.map string
            |> String.concat "; "
        | _ -> node.ToString()
    let counterState =
        state.ActiveCounters
        |> Seq.map (fun v -> v.Value.Offset)
        |> Seq.map (fun v -> $"(c:{v})")
        |> String.concat ";"
    counterState + "; " + nodestr + "; " + loc


#endif