module Sbre.Test.Common

open System
open Sbre
open Sbre.Algorithm
open Sbre.Pat
open Sbre.Types

let getRawFlags(reg: Regex) =
    try
        let matcher = reg.ByteMatcher
        matcher.RawPattern.TryGetInfo.Value.Flags
    with e ->
        try
            let matcher = reg.UInt16Matcher
            matcher.RawPattern.TryGetInfo.Value.Flags
        with e ->
            let matcher = reg.TSetMatcher
            matcher.RawPattern.TryGetInfo.Value.Flags


let getDerImpl (reg: Regex) (input: string) =
    let location = (Location.create input 0)

    try
        let matcher = reg.TSetMatcher
        let cache = matcher.Cache
        let node = matcher.ImplicitPattern
        let der1 = createDerivative (cache, &location, cache.MintermForLocation(location), node)
        cache.PrettyPrintNode der1
    with e ->
        try
            let matcher = reg.ByteMatcher
            let cache = matcher.Cache
            let node = matcher.ImplicitPattern
            let der1 = createDerivative (cache, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1
        with e ->
            let matcher = reg.UInt16Matcher
            let cache = matcher.Cache
            let node = matcher.ImplicitPattern
            let der1 = createDerivative (cache, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1

let getDer1 (reg: Regex) (input: string) =
    let location = (Location.create input 0)

    try
        let matcher = reg.TSetMatcher
        let cache = matcher.Cache
        let node = matcher.RawPattern
        let der1 = createDerivative (cache, &location, cache.MintermForLocation(location), node)
        cache.PrettyPrintNode der1
    with e ->
        try
            let matcher = reg.ByteMatcher
            let cache = matcher.Cache
            let node = matcher.RawPattern
            let der1 = createDerivative (cache, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1
        with e ->
            let matcher = reg.UInt16Matcher
            let cache = matcher.Cache
            let node = matcher.RawPattern
            let der1 = createDerivative (cache, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1

let getDerLoc (reg: Regex) (location: Location) =

    try
        let matcher = reg.TSetMatcher
        let cache = matcher.Cache
        let node = matcher.RawPattern
        let der1 = createDerivative (cache, &location, cache.MintermForLocation(location), node)
        cache.PrettyPrintNode der1
    with e ->
        try
            let matcher = reg.ByteMatcher
            let cache = matcher.Cache
            let node = matcher.RawPattern
            let der1 = createDerivative (cache, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1
        with e ->
            let matcher = reg.UInt16Matcher
            let cache = matcher.Cache
            let node = matcher.RawPattern
            let der1 = createDerivative (cache, &location, cache.MintermForLocation(location), node)
            cache.PrettyPrintNode der1




let getRawPattern<'t when 't: struct and 't :> IEquatable<'t> and 't: equality>(matcher: Regex) =
    let matcher = matcher.Matcher :?> RegexMatcher<TSet>
    matcher.RawPattern :> obj

