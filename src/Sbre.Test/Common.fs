module Sbre.Test.Common

#if DEBUG

open System
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
    let node = if raw then matcher.RawPattern else matcher.InitialPattern
    let state = RegexState(cache.NumOfMinterms())
    let minterm = cache.MintermForLocation(location)
    CountingSet.stepCounters state minterm
    let der1 = createDerivative (cache, state,  &location, cache.MintermForLocation(location), node)
    cache.PrettyPrintNode der1

let der1Node (reg: Regex) (input: string) (raw:bool) =
    let location = (Location.create input 0)
    let matcher = reg.TSetMatcher
    let cache = matcher.Cache
    let node = if raw then matcher.RawPattern else matcher.InitialPattern
    let state = RegexState(cache.NumOfMinterms())
    let minterm = cache.MintermForLocation(location)
    CountingSet.stepCounters state minterm
    let der1 = createDerivative (cache, state,  &location, cache.MintermForLocation(location), node)
    der1

let der1rawlocs (reg: Regex) (location: Location) =
    let matcher = reg.TSetMatcher
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let state = RegexState(cache.NumOfMinterms())
    let der1 = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
    cache.PrettyPrintNode der1


let inline assertEqual (x1:'t) (x2:'t) = Assert.Equal<'t>(x1, x2)
let inline assertTrue (x1:_) (msg:string) = Assert.True(x1, msg)
let inline assertFalse (x1:_) (msg:string) = Assert.False(x1, msg)


let assertPatternIn (expectedResults:string list) (node:RegexNode<TSet>) =
    let nodestr = node.ToString()
    Assert.Contains(nodestr , expectedResults)

let assertAlternation (expectedResults:string list) (node:RegexNode<TSet>) =
    match node with
    | Or(nodes, info) ->
        let nodestrs = nodes |> Seq.map (_.ToString()) |> set
        for r in expectedResults do
            Assert.Contains(r, nodestrs)
    | _ ->
        for r in expectedResults do
            Assert.Contains(r, node.ToString())

let assertCounterStates (regex:Regex) (input:string) (expectedStates:(CounterState * int) list list)  =
    let matcher = regex.TSetMatcher
    let state = RegexState(matcher.Cache.NumOfMinterms())
    let cache = matcher.Cache
    let mutable loc = (Location.create input 0)
    let mutable currNode =
        match matcher.RawPattern with
        | Not(_) -> matcher.RawPattern
        | _ -> matcher.InitialPattern
    let mutable remainingStates = expectedStates
    let mutable endNullable = false


    while (not (Location.isFinal loc)) && not (refEq currNode cache.False) && not remainingStates.IsEmpty do

        // let pre = ctrs |> Seq.map (fun v -> v.Queue)
        let minterm = cache.MintermForLocation(loc)

        let isnull =
            let endNullable = RegexNode.isNullable(cache, state, &loc, currNode)
            endNullable
        endNullable <- isnull

        let locpos = loc.Position

        // bump counters
        let counters = state.Counters()

        let deriv = createDerivative (cache, state, &loc, minterm, currNode)
        currNode <- deriv

        Seq.zip state.ActiveCounters.Values remainingStates.Head
        |> Seq.iter (fun (cs,(excs, exof)) ->
            Assert.True(cs.GetState() = excs, $"pos: {locpos}, expected:{excs}, real:{cs.GetState()}")
            Assert.True(cs.Offset = exof, $"pos: {locpos}, expected:{exof}, real:{cs.Offset}")
        )

        if currNode.HasCounter then
            state.ActiveCounters |> Seq.iter (_.Value.TryReset())
            CountingSet.stepCounters state minterm

            // CountingSet.bumpCounters state minterm currNode



        loc.Position <- loc.Position + 1
        remainingStates <- remainingStates.Tail


    let isnull =
        let endNullable = RegexNode.isNullable(cache, state, &loc, currNode)
        endNullable
    endNullable <- isnull

    {|
      Node = currNode
      State = state
      IsNullable = endNullable
      |}


let assertAllStates (regex:Regex) (input:string) (expectedRegexesList:string list list)  =
    let matcher = regex.TSetMatcher
    let state = RegexState(matcher.Cache.NumOfMinterms())
    let cache = matcher.Cache
    let mutable loc = (Location.create input 0)
    let mutable currNode = matcher.InitialPattern
    let mutable remainingStates = expectedRegexesList

    while (not (Location.isFinal loc)) && not (refEq currNode cache.False) && not remainingStates.IsEmpty do

        let minterm = cache.MintermForLocation(loc)
        CountingSet.stepCounters state minterm

        let counters =
            state.ActiveCounters.Values
            |> Seq.toList

        // bump counters
        assertAlternation remainingStates.Head currNode
        let deriv = createDerivative (cache, state, &loc, cache.MintermForLocation(loc), currNode)
        currNode <- deriv
        remainingStates <- remainingStates.Tail
        loc.Position <- loc.Position + 1

    let endNullable = RegexNode.isNullable(cache, state, &loc, currNode)
    {|
      Node = currNode
      State = state
      IsNullable = endNullable
      |}


let assertNullability (regex:Regex) (input:string) (expectedRegexesList:string list list)  =
    let matcher = regex.TSetMatcher
    let state = RegexState(matcher.Cache.NumOfMinterms())
    let cache = matcher.Cache
    let mutable loc = (Location.create input 0)
    let mutable currNode = matcher.InitialPattern
    let mutable remainingStates = expectedRegexesList

    while (not (Location.isFinal loc)) && not (refEq currNode cache.False) && not remainingStates.IsEmpty do

        let minterm = cache.MintermForLocation(loc)
        CountingSet.stepCounters state minterm

        let counters =
            state.ActiveCounters.Values
            |> Seq.toList

        // bump counters
        assertAlternation remainingStates.Head currNode
        let deriv = createDerivative (cache, state, &loc, cache.MintermForLocation(loc), currNode)
        currNode <- deriv
        remainingStates <- remainingStates.Tail
        loc.Position <- loc.Position + 1

    let endNullable = RegexNode.isNullable(cache, state, &loc, currNode)
    {|
      Node = currNode
      State = state
      IsNullable = endNullable
      |}



let assertDfaFirstNullable (pattern:string) (input:string) (firstNull)  =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createSpanRev (input.AsSpan()) input.Length false
    let result = matcher.DfaEndPosition(&loc,1,RegexSearchMode.FirstNullable)
    failwith "todo"


let assertDfaMatches (pattern:string) (input:string) (expected: (int*int) list)  =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let result = matcher.MatchPositions(input)
    Assert.Equal(expected, result |> Seq.map (fun v -> v.Index,v.Length))

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


let derNode(matcher: Regex, state:RegexState, node: RegexNode<TSet>, input: string) =
    let matcher = matcher.Matcher :?> RegexMatcher<TSet>
    let cache = matcher.Cache
    let location = (Location.create input 0)
    let minterm = cache.MintermForLocation(location)
    CountingSet.stepCounters state minterm
    let deriv = createDerivative (cache, state, &location, cache.MintermForLocation(location), node)
    deriv


let getNodeDerivative(matcher: Regex, state:RegexState, node: RegexNode<TSet>, input: string) =
    let matcher = matcher.Matcher :?> RegexMatcher<TSet>
    let cache = matcher.Cache
    let location = (Location.create input 0)
    let minterm = cache.MintermForLocation(location)
    CountingSet.stepCounters state minterm
    let deriv = createDerivative (cache, state, &location, minterm, node)
    deriv


let getFirstDerivative(matcher: Regex, state:RegexState, node: RegexNode<TSet>, input: string) =
    let matcher = matcher.Matcher :?> RegexMatcher<TSet>
    let cache = matcher.Cache
    let loc = (Location.create input 0)
    let minterm = cache.MintermForLocation(loc)
    CountingSet.stepCounters state minterm
    let deriv = createDerivative (cache, state, &loc, minterm, node)
    deriv


let assertFirstNullablePos (pattern:string) (input:string) (expected) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.create input 0
    let result = matcher.DfaEndPosition(&loc,1, RegexSearchMode.FirstNullable)
    Assert.Equal(expected, result)


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
    let result = matcher.CollectReverseNullablePositions(&loc)
    Assert.Equal<int>(expected, result)


let getFirstLLmatch (pattern:string) (input:string) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createReversedSpan (input.AsSpan())
    let result = matcher.CollectReverseNullablePositions(&loc)
    let R_id = matcher.GetOrCreateState(matcher.RawPattern).Id
    let matchStart = result |> Seq.last
    loc.Position <- matchStart
    loc.Reversed <- false
    let endPos = matcher.DfaEndPosition(&loc,R_id,RegexSearchMode.MatchEnd)
    (matchStart,endPos)

let getFirstLLmatchText (pattern:string) (input:string) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable loc = Location.createReversedSpan (input.AsSpan())
    let result = matcher.CollectReverseNullablePositions(&loc)
    let R_id = matcher.GetOrCreateState(matcher.RawPattern).Id
    let matchStart = result |> Seq.last
    loc.Position <- matchStart
    loc.Reversed <- false
    let endPos = matcher.DfaEndPosition(&loc,R_id,RegexSearchMode.MatchEnd)
    input.AsSpan().Slice(matchStart, endPos - matchStart).ToString()



let getAllLLmatches (pattern:string) (input:string) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let results = matcher.llmatch_all(input)
    results


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
    let result = matcher.CollectReverseNullablePositions(&loc)
    Assert.Equal(0, result.Count)



let assertAllLLmatches (pattern:string) (input:string) (expected) =
    let result =
        getAllLLmatches pattern input
        |> Seq.map (fun v -> v.Index,v.Length)
    Assert.Equal<int*int>(expected, result)

let assertAllLLmatchTexts (pattern:string) (input:string) (expected) =
    let result =
        getAllLLmatches pattern input
        |> Seq.map _.GetText(input)
    Assert.Equal<string>(expected, result)

#endif
