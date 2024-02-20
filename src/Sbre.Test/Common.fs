module Sbre.Test.Common

#if DEBUG

open System
open System.Collections
open Sbre
open Sbre.Algorithm
open Sbre.CountingSet
open Sbre.Pat
open Sbre.Types
open Xunit


let der1 (reg: Regex) (input: string) (raw:bool) =
    let location = (Location.create input 0)
    let matcher = reg.TSetMatcher
    let cache = matcher.Cache
    let node = if raw then matcher.RawPattern else matcher.TrueStarredPattern
    let minterm = cache.MintermForLocation(location)
    let der1 = matcher.CreateDerivative ( &location, cache.MintermForLocation(location), node)
    cache.PrettyPrintNode der1

let der1Node (reg: Regex) (input: string) (raw:bool) =
    let location = (Location.create input 0)
    let matcher = reg.TSetMatcher
    let cache = matcher.Cache
    let node = if raw then matcher.RawPattern else matcher.TrueStarredPattern
    let minterm = cache.MintermForLocation(location)
    let der1 = matcher.CreateDerivative  (&location, cache.MintermForLocation(location), node)
    der1

let der1Rev (reg: Regex) (input: string) =
    let location = (Location.createReversedSpan (input.AsSpan()))
    let matcher = reg.TSetMatcher
    let cache = matcher.Cache
    let node = matcher.ReversePattern
    let der1 = matcher.CreateDerivative  (  &location, cache.MintermForLocation(location), node)
    der1

let der1rawlocs (reg: Regex) (location: Location) =
    let matcher = reg.TSetMatcher
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let der1 = matcher.CreateDerivative (&location, cache.MintermForLocation(location), node)
    cache.PrettyPrintNode der1


let inline assertEqual (x1:'t) (x2:'t) = Assert.Equal<'t>(x1, x2)
let inline assertAllEqual (x1:seq<'t>) (x2:seq<'t>) = Assert.Equal<'t>(x1, x2)
let inline assertEqualMatchesRuntime (x1:seq<'t>) (x2:seq<'t>) = Assert.Equal<'t>(x1, x2)
let inline assertTrue (x1:_) (msg:string) = Assert.True(x1, msg)
let inline assertFalse (x1:_) (msg:string) = Assert.False(x1, msg)
let inline assertFlag (nf:RegexNodeFlags) (msg:RegexNodeFlags) = Assert.True(nf.HasFlag(msg), $"{msg}")
let inline assertNotFlag (nf:RegexNodeFlags) (msg:RegexNodeFlags) = Assert.False(nf.HasFlag(msg), $"{msg}")
let inline assertContains (items:'t list) (data:'t) = Assert.Contains(data, items)


let assertPatternIn (expectedResults:string list) (node:RegexNode<TSet>) =
    let nodestr = node.ToString()
    Assert.Contains(nodestr , expectedResults)

let assertAlternation (node:RegexNode<TSet>) (expectedResults:string list)  =
    match node with
    | Or(nodes, info) ->
        let nodestrs = nodes |> Seq.map (_.ToString()) |> set
        for r in expectedResults do
            Assert.Contains(r, nodestrs)
    | _ ->
        for r in expectedResults do
            Assert.Contains(r, node.ToString())


let applyPrefix pattern =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix = Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let applied = Optimizations.applyPrefixSets getder matcher.Cache matcher.ReverseTrueStarredPattern prefix
    applied

let getInitOptimizations pattern =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let optimizations =
        Optimizations.findInitialOptimizations
            getder
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    optimizations

let assertPotentialPrefix pattern expected =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let optimizations =
        Optimizations.findInitialOptimizations
            getder
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    | Optimizations.InitialOptimizations.SetsPotentialStart(prefix) ->
        let prefixString = Optimizations.printPrefixSets matcher.Cache (prefix.ToArray() |> Seq.toList)
        Assert.Equal(expected, prefixString)
    | _ -> failwith $"invalid optimization result: {optimizations}"


let assertSetsPrefix pattern expected =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let optimizations =
        Optimizations.findInitialOptimizations
            getder
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    | Optimizations.InitialOptimizations.SetsPrefix(prefix, transId) ->
        let prefixString = Optimizations.printPrefixSets matcher.Cache (prefix.ToArray() |> Seq.toList)
        Assert.Equal(expected, prefixString)
    | _ -> failwith $"invalid optimization result: {optimizations}"

let assertStringPrefix pattern expected =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let optimizations =
        Optimizations.findInitialOptimizations
            getder
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache matcher.ReversePattern matcher.ReverseTrueStarredPattern
    match optimizations with
    | Optimizations.InitialOptimizations.StringPrefix(prefix, transId) ->
        let prefixString = prefix.ToString()
        Assert.Equal(expected, prefixString)
    | _ -> failwith $"invalid optimization result: {optimizations}"

// let assertCounterStates (regex:Regex) (input:string) (expectedStates:(CounterState * int) list list)  =
//     let matcher = regex.TSetMatcher
//     let cache = matcher.Cache
//     let mutable loc = (Location.create input 0)
//     let mutable currNode =
//         match matcher.RawPattern with
//         | Not(_) -> matcher.RawPattern
//         | _ -> matcher.TrueStarredPattern
//     let mutable remainingStates = expectedStates
//     let mutable endNullable = false
//
//
//     while (not (Location.isFinal loc)) && not (refEq currNode cache.False) && not remainingStates.IsEmpty do
//
//         // let pre = ctrs |> Seq.map (fun v -> v.Queue)
//         let minterm = cache.MintermForLocation(loc)
//
//         let isnull =
//             let endNullable = matcher.IsNullable(&loc, currNode)
//             endNullable
//         endNullable <- isnull
//
//         let locpos = loc.Position
//
//         // bump counters
//         let counters = state.Counters()
//
//         let deriv = matcher.CreateDerivative ( &loc, minterm, currNode)
//         currNode <- deriv
//
//         Seq.zip state.ActiveCounters.Values remainingStates.Head
//         |> Seq.iter (fun (cs,(excs, exof)) ->
//             Assert.True(cs.GetState() = excs, $"pos: {locpos}, expected:{excs}, real:{cs.GetState()}")
//             Assert.True(cs.Offset = exof, $"pos: {locpos}, expected:{exof}, real:{cs.Offset}")
//         )
//
//         if currNode.HasCounter then
//             state.ActiveCounters |> Seq.iter (_.Value.TryReset())
//             CountingSet.stepCounters state minterm
//
//             // CountingSet.bumpCounters state minterm currNode
//
//
//
//         loc.Position <- loc.Position + 1
//         remainingStates <- remainingStates.Tail
//
//
//     let isnull =
//         let endNullable = matcher.IsNullable(  &loc, currNode)
//         endNullable
//     endNullable <- isnull
//
//     {|
//       Node = currNode
//       State = state
//       IsNullable = endNullable
//       |}


let assertRevStates (pattern:string) (input:string) (expectedRegexesList:string list list)  =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let cache = matcher.Cache
    let mutable loc = (Location.createReversedSpan (input.AsSpan()))
    let mutable currNode = matcher.ReverseTrueStarredPattern
    let mutable remainingStates = expectedRegexesList

    while (not (Location.isFinal loc)) && not remainingStates.IsEmpty do
        assertPatternIn remainingStates.Head currNode
        let deriv = matcher.CreateDerivative ( &loc, cache.MintermForLocation(loc), currNode)
        currNode <- deriv
        remainingStates <- remainingStates.Tail
        loc.Position <- loc.Position - 1

    ()



// let assertNullability (regex:Regex) (input:string) (expectedRegexesList:string list list)  =
//     let matcher = regex.TSetMatcher
//     let cache = matcher.Cache
//     let mutable loc = (Location.create input 0)
//     let mutable currNode = matcher.TrueStarredPattern
//     let mutable remainingStates = expectedRegexesList
//
//     while (not (Location.isFinal loc)) && not (refEq currNode cache.False) && not remainingStates.IsEmpty do
//
//         let minterm = cache.MintermForLocation(loc)
//         CountingSet.stepCounters state minterm
//
//         let counters =
//             state.ActiveCounters.Values
//             |> Seq.toList
//
//         // bump counters
//         assertAlternation remainingStates.Head currNode
//         let deriv = matcher.CreateDerivative ( &loc, cache.MintermForLocation(loc), currNode)
//         currNode <- deriv
//         remainingStates <- remainingStates.Tail
//         loc.Position <- loc.Position + 1
//
//     let endNullable = matcher.IsNullable( &loc, currNode)
//     {|
//       Node = currNode
//       State = state
//       IsNullable = endNullable
//       |}



let assertDfaFirstNullable (pattern:string) (input:string) (firstNull)  =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createSpanRev (input.AsSpan()) input.Length false
    let result = matcher.DfaEndPosition(&loc,1)
    failwith "todo"


let assertDfaMatches (pattern:string) (input:string) (expected: (int*int) list)  =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let result = matcher.MatchPositions(input)
    Assert.Equal(expected, result |> Seq.map (fun v -> v.Index,v.Length))



let assertFirstNullablePos (pattern:string) (input:string) (expected) =
    failwith "todo"
    // let regex = Regex(pattern)
    // let matcher = regex.TSetMatcher
    // let mutable loc = Location.create input 0
    // let rstate = RegexState(matcher.Cache.NumOfMinterms())
    // let result = matcher.DfaEndPosition(&loc,1)
    Assert.Equal(expected, 1)


let assertStartset (pattern:string) (initial:bool) (expected:string) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let startState = matcher.GetOrCreateState(matcher.RawPattern)
    matcher.CreateStartset(startState, initial)
    let resultStartset = matcher.Cache.PrettyPrintMinterm(startState.Startset)
    Assert.Equal(resultStartset, expected)



// let assertMatchStart (pattern:string) (input:string) (startLocation:int) (expectedMatchStart:int) =
//     let regex = Regex(pattern)
//     let matcher = regex.TSetMatcher
//     let mutable loc = Location.create input startLocation
//     let result = matcher.DfaStartPosition(&loc,3)
//     Assert.Equal(expectedMatchStart, result)
//
//
// let assertNullableRanges (pattern:string) (input:string) (expected) =
//     let regex = Regex(pattern)
//     let matcher = regex.TSetMatcher
//     let mutable loc = Location.createReversedSpan (input.AsSpan())
//     let result = matcher.DfaSweepNullableRanges(&loc,4)
//     Assert.Equal<struct (int*int)>(expected, result)


let assertNullablePositions (pattern:string) (input:string) (expected) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createReversedSpan (input.AsSpan())
    use mutable acc = new SharedResizeArrayStruct<int>(100)
    let result = matcher.CollectReverseNullablePositions(&acc,&loc)
    Assert.Equal<int>(expected, result.AsArray())

let printAllDerivatives (pattern:string) (input:string) (expected: string list list) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createReversedSpan (input.AsSpan())
    use mutable acc = new SharedResizeArrayStruct<int>(100)
    let result = matcher.PrintAllDerivatives(&acc,&loc)
    failwith $"%A{result}"
    ()


let getDfaMatchEnd (pattern:string) (input:string) (startPos:int)  =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createReversedSpan (input.AsSpan())
    let R_id = matcher.GetOrCreateState(matcher.RawPattern).Id
    let matchStart = startPos
    loc.Position <- matchStart
    loc.Reversed <- false
    let endPos = matcher.DfaEndPosition(&loc,R_id)
    endPos

let getFirstLLmatch (pattern:string) (input:string) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let llmatches = matcher.llmatch_all(input).AsArray()[0]
    (llmatches.Index,llmatches.Index+llmatches.Length)

let getFirstLLmatchText (pattern:string) (input:string) =
    let (ms,me) = getFirstLLmatch pattern input
    input.AsSpan().Slice(ms, me - ms).ToString()



let getAllLLmatches (pattern:string) (input:string) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let rs = ResizeArray()
    for r in matcher.llmatch_all(input) do
        rs.Add(r)
    rs


let assertFirstMatch (pattern:string) (input:string) (expected) =
    let result = getFirstLLmatch pattern input
    Assert.Equal<int*int>(expected, result)

let assertFirstMatchText (pattern:string) (input:string) (expected) =
    let result = getFirstLLmatch pattern input
    Assert.Equal(expected, input.AsSpan().Slice(fst result, snd result - fst result).ToString())

let assertNoMatch (pattern:string) (input:string)  =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createReversedSpan (input.AsSpan())
    use mutable acc = new SharedResizeArrayStruct<int>(100)
    let result = matcher.CollectReverseNullablePositions(&acc,&loc)
    Assert.Equal(0, result.Length)

let assertNoMatchRaw (pattern:string) (input:string)  =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createReversedSpan (input.AsSpan())
    use mutable acc = new SharedResizeArrayStruct<int>(100)
    let result = matcher.CollectReverseNullablePositions(&acc,&loc)
    Assert.Equal(0, result.Length)

let assertIsMatch (pattern:string) (input:string)  =
    let regex = Regex(pattern)
    Assert.True(regex.IsMatch(input))


let assertAllLLmatches (pattern:string) (input:string) (expected) =
    let result =
        getAllLLmatches pattern input
        |> Seq.map (fun v -> v.Index,v.Length)
    try
        Assert.Equal<int*int>(expected, result)
    with e ->
        let real = result |> Seq.map string |> String.concat ";"
        failwith $"expected: %A{expected};\nactual:%A{real}"

let assertAllLLmatchTexts (pattern:string) (input:string) (expected) =
    let result =
        getAllLLmatches pattern input
        |> Seq.map _.GetText(input)
    Assert.Equal<string>(expected, result)

let assertMatchEnd (pattern:string) (input:string) (startPos:int) (expectedEndPos:int)  =
    let endPos = getDfaMatchEnd pattern input startPos
    assertEqual expectedEndPos endPos

let assertMatchEndNoLookback (pattern:string) (input:string) (startPos:int) (expectedEndPos:int)  =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createReversedSpan (input.AsSpan())
    let R_id = matcher.GetOrCreateState(matcher.RawPatternWithoutLookback).Id
    let matchStart = startPos
    loc.Position <- matchStart
    loc.Reversed <- false
    let endPos = matcher.DfaEndPosition(&loc,R_id)
    assertEqual expectedEndPos endPos



let assertNodeOneOf (node:RegexNode<_>) (options:string seq) =
    Assert.Contains(node.ToString() , options)


let matchPosToTuples (items:MatchPosition seq) =
    items |> Seq.map (fun v -> v.Index,v.Length) |> Seq.toArray



let assertRawDerivative (pattern: string) (input: string) (expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let location = (Location.create input 0)
    let result = der1rawlocs matcher location
    Assert.Contains(result,expectedDerivatives)

let assertTSDerivative (pattern: string) (input: string) (expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let result = der1 matcher input false
    Assert.Contains(result,expectedDerivatives)


let assertConverted (pattern: string) (expected: string list) =
    let reg = Regex(pattern)
    let asstr = reg.TSetMatcher.Cache.PrettyPrintNode reg.TSetMatcher.RawPattern
    Assert.Contains<string>(asstr,expected)


#endif
