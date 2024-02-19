module Sbre.Benchmarks.Optimizations

open System.Buffers
open System.Runtime.CompilerServices
open BenchmarkDotNet.Attributes
open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Optimizations
open System
open Sbre.Pat
open Sbre.Types
let fullInput =
    __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText

let shortInput20k = fullInput[..19999] // 20k chars limit
let shortInput10k = fullInput[..9999] // 10k chars limit


let collectNullablePositionsNoSkip ( matcher: RegexMatcher<TSet>, loc: byref<Location> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let rstate = Sbre.CountingSet.RegexState(matcher.Cache.NumOfMinterms())
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            loc.Position <- loc.Position

        if matcher.StateIsNullable(flags, rstate, &loc, dfaState) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(rstate, flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount


let collectNullablePositionsOriginal ( matcher: RegexMatcher<TSet>, loc: byref<Location> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let rstate = Sbre.CountingSet.RegexState(matcher.Cache.NumOfMinterms())
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            matcher.TrySkipInitialRev(&loc, &dfaState, &currentStateId)

        if matcher.StateIsNullable(flags, rstate, &loc, dfaState) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(rstate, flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount

let commonalityScore (charSet: char array) =
    charSet |> Array.map (fun c ->
        if Char.IsAsciiLetterLower c then 10
        else 0)
    |> Array.sum

let prefixSearchWeightedReversed (loc: byref<Location>) (cache: RegexCache<TSet>) (weightedSets: inref<(int * TSet) list>) =
    let textSpan = loc.Input
    let rarestCharSet = cache.MintermChars(snd weightedSets[0])
    let rarestCharSetIndex = fst weightedSets[0]
    let mutable searching = true

    let mutable prevMatch = loc.Position
    while searching do
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet) with
        | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length <= loc.Position) ->
            let absMatchStart = curMatch - rarestCharSetIndex
            let mutable fullMatch = true
            let mutable i = 1
            while i < weightedSets.Length && absMatchStart + (fst weightedSets[i]) < textSpan.Length && fullMatch do
                let set = cache.MintermChars(snd weightedSets[i])
                if textSpan.Slice(absMatchStart + (fst weightedSets[i]), 1).IndexOfAny(set) = -1 then
                    fullMatch <- false
                else
                    i <- i + 1
            prevMatch <- absMatchStart + rarestCharSetIndex
            if fullMatch && i = weightedSets.Length then
                searching <- false
                loc.Position <- absMatchStart + weightedSets.Length
        | -1 ->
            searching <- false
            loc.Position <- 0
        | outOfBounds -> prevMatch <- outOfBounds
    ()

let prefixSearchWeightedReversed2
    (loc: byref<Location>)
    (cache: RegexCache<TSet>)
    (weightedSets: inref<struct(int * SearchValues<char>) array>) =
    // (a * b) is a reference tuple, struct(a * b) is a struct tuple
    let textSpan = loc.Input
    let struct(rarestCharSetIndex, rarestCharSet) = weightedSets[0]
    let mutable searching = true
    let mutable prevMatch = loc.Position
    while searching do
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet) with
        | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length <= loc.Position) ->
            let absMatchStart = curMatch - rarestCharSetIndex
            let mutable fullMatch = true
            let mutable i = 1
            let struct(weightedSetIndex,weightedSet) = weightedSets[i]
            while fullMatch && i < weightedSets.Length && absMatchStart + (weightedSetIndex) < textSpan.Length do
                let set = weightedSet
                if not (set.Contains(textSpan[absMatchStart + weightedSetIndex])) then
                    fullMatch <- false
                else
                    i <- i + 1
            prevMatch <-
                if rarestCharSetIndex = 0 then absMatchStart - 1 else
                absMatchStart + rarestCharSetIndex
            if fullMatch && i = weightedSets.Length then
                searching <- false
                loc.Position <- absMatchStart + weightedSets.Length
        | -1 ->
            searching <- false
            loc.Position <- 0
        | outOfBounds -> prevMatch <- outOfBounds
    ()


let collectNullablePositionsWeightedSkip ( matcher: RegexMatcher<TSet>, loc: byref<Location>, weightedSets: inref<(int * TSet) list> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let rstate = Sbre.CountingSet.RegexState(matcher.Cache.NumOfMinterms())
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            prefixSearchWeightedReversed &loc matcher.Cache &weightedSets


        if matcher.StateIsNullable(flags, rstate, &loc, dfaState) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(rstate, flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc // Also moves when location is nullable
        else
            looping <- false

    nullableCount

// ---------- slightly modified
let collectNullablePositionsWeightedSkip2 (
    matcher: RegexMatcher<TSet>,
    loc: byref<Location>,
    weightedSets: inref<struct (int * SearchValues<char>) array> // <- F# list lookup is slow
    ) =
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let rstate = Sbre.CountingSet.RegexState(matcher.Cache.NumOfMinterms())
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0


    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            prefixSearchWeightedReversed2 &loc matcher.Cache &weightedSets

        if matcher.StateIsNullable(flags, rstate, &loc, dfaState) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(rstate, flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc // Also moves when location is nullable
        else
            looping <- false

    nullableCount

[<MemoryDiagnoser>]
[<ShortRunJob>]
[<AbstractClass>]
type StringPrefix(pattern:string) =
    let regex = Sbre.Regex(pattern)
    let matcher = regex.TSetMatcher
    // find optimized prefix for regex
    let optimizations =
        Sbre.Optimizations.findInitialOptimizations
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache
            matcher.ReversePattern
            matcher.ReverseTrueStarredPattern
    let prefixString =
        match optimizations with
        | InitialOptimizations.StringPrefix(prefix, transitionNodeId) ->
            prefix
        | _ -> failwith "could not get prefix"

    let charToTSet (chr:char) = matcher.Cache.CharToMinterm(chr)
    let isElemOfSet (tset1:TSet) (tset2:TSet) = Solver.elemOfSet tset1 tset2

    [<Benchmark>]
    member x.StringIndexOf() =
        fullInput.IndexOf(prefixString)

    [<Benchmark>]
    member x.SpanIndexOf() =
        // about 300x faster when vectorized
        fullInput.AsSpan().IndexOf(prefixString)

    [<Benchmark>]
    member x.SpanIndexOf1() =
        let span = fullInput.AsSpan()
        let firstSet = prefixString[0]
        let tail = prefixString.AsSpan(start=1)
        let mutable currpos = 0
        let mutable searching = true
        while searching do
            // vectorize only to first char
            let slice = span.Slice(currpos)
            let delta = slice.IndexOf(firstSet)
            if delta = -1 then
                // doesnt exist
                searching <- false
            currpos <- currpos + delta
            let validStart = slice.StartsWith(tail)
            if validStart then
                // found
                searching <- false
            else currpos <- currpos + 1


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
[<AbstractClass>]
type SetsPrefix(pattern:string) =
    let regex = Sbre.Regex(pattern)
    let matcher = regex.TSetMatcher
    // find optimized prefix for regex
    let optimizations =
        Sbre.Optimizations.findInitialOptimizations
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache
            matcher.ReversePattern
            matcher.ReverseTrueStarredPattern
    let prefixSets =
        match optimizations with
        | InitialOptimizations.PotentialStartPrefix(prefix) ->
            let reverseSpan = prefix.Span
            reverseSpan.Reverse()
            reverseSpan.ToArray()
        | InitialOptimizations.SetsPrefix(prefix, transitionid) ->
            let reverseSpan = prefix.Span
            reverseSpan.Reverse()
            reverseSpan.ToArray()
        | _ -> failwith "could not get prefix"

    let charToTSet (chr:char) = matcher.Cache.Classify(chr)
    let isElemOfSet (tset1:TSet) (tset2:TSet) = Solver.elemOfSet tset1 tset2

    [<Benchmark>]
    member x.FirstSetIndexOfTSet() =
        let inputSpan = fullInput.AsSpan()
        let mutable currpos = 0
        let mutable searching = true
        while searching do
            let currSet = charToTSet inputSpan[currpos]
            if isElemOfSet currSet prefixSets[0] then
                searching <- false
            else
                currpos <- currpos + 1

    [<Benchmark>]
    member x.FirstSetIndexOfChars() =
        let firstSetChars = matcher.Cache.MintermChars(prefixSets[0])
        let inputSpan = fullInput.AsSpan()
        let mutable searching = true
        while searching do
            match inputSpan.IndexOfAny(firstSetChars) with
            | -1 -> failwith "failed search"
            | n ->
                searching <- false

    [<Benchmark>]
    member x.FirstSetIndexOfSearchValues() =
        let firstSetChars = matcher.Cache.MintermSearchValues(prefixSets[0])
        let inputSpan = fullInput.AsSpan()
        let mutable searching = true
        while searching do
            match inputSpan.IndexOfAny(firstSetChars) with
            | -1 -> failwith "failed search"
            | n ->
                searching <- false




[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Prefix1() =
    // inherit StringPrefix("Twain")
    inherit StringPrefix("there")

[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
type Prefix2() =
    // [HF][ui][cn][kn]
    inherit SetsPrefix("Huck|Finn")

[<MemoryDiagnoser(true)>]
// [<ShortRunJob>]
type PrefixCharsetSearch () =

    // let regex = Sbre.Regex("Huck[a-zA-Z]+|Saw[a-zA-Z]+")
    // let regex = Sbre.Regex("[a-zA-Z]+ckle|[a-zA-Z]+awy")
    let regex = Sbre.Regex(".*have.*&.*there.*")


    // let rs = "[a-zA-Z]+ckl|[a-zA-Z]+awy"
    // [<Params("[a-zA-Z]+ckle|[a-zA-Z]+awy", "Huck[a-zA-Z]+|Saw[a-zA-Z]+", ".*have.*&.*there.*")>]
    // member val rs: string = "" with get, set


    let cache = regex.TSetMatcher.Cache
    let matcher = regex.TSetMatcher

    let optimizations =
        Sbre.Optimizations.findInitialOptimizations
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache
            matcher.ReversePattern
            matcher.ReverseTrueStarredPattern

    let reversedPrefixSpan =
        match optimizations with
        | InitialOptimizations.PotentialStartPrefix(prefix) ->
            prefix
        | _ -> failwith "todo"


    let prefixSets =
        match optimizations with
        | InitialOptimizations.PotentialStartPrefix(prefixMem) ->
            Array.toList (prefixMem.ToArray()) |> List.rev
        | _ -> failwith "debug"
    let weightedSets = prefixSets |> List.mapi (fun i set ->
            (i, set, commonalityScore (cache.MintermChars(set).ToArray())))
                       |> List.sortBy (fun (_, _, score) -> score )
                       |> List.map (fun (i, set, _) -> (i, set))

    let charSetIndex = fst weightedSets[0]

    let weightedCharsetsArray2 =
        weightedSets
        |> Seq.map (fun (int,tset) ->
            // TSet may contain up to 65k characters
            // so if is too large it's better to use "Solver.elemOfSet (matcher.Cache.Classify(char)) tset"
            // when such an example comes
            let chars = matcher.Cache.MintermSearchValues(tset)
            // ^ this allocation should really be moved out of the match algorithm
            // --
            // let isInverted = matcher.Cache.IsInverted(tset)
            // ^ this is not used for simplicity because it's not needed
            // but signals MintermChars is inverted
            struct(int, chars)
        )
        |> Seq.toArray


    // [<Benchmark>]
    // member this.NoSkip() =
    //     let textSpan = fullInput.AsSpan()
    //     let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
    //     collectNullablePositionsNoSkip (matcher, &loc)

    //
    [<Benchmark>]
    member this.Original() =
        let textSpan = fullInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsOriginal (matcher, &loc)


    //
    [<Benchmark>]
    member this.Weighted() =
        let textSpan = fullInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsWeightedSkip (matcher, &loc, &weightedSets)

    [<Benchmark>]
    member this.Weighted2() =
        let textSpan = fullInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsWeightedSkip2 (matcher, &loc, &weightedCharsetsArray2)
        |> (function 8562 -> () | n -> failwith $"invalid result {n}") // sanity check for .*have.*&.*there.*


    member this.TestSkip(loc:Location) : int =
        let skipResult = matcher.Cache.TryNextStartsetLocationArrayReversed( &loc, reversedPrefixSpan.Span )
        match skipResult with
        | ValueSome resultEnd ->
            resultEnd
        | ValueNone ->
            Location.final loc


    // [<Benchmark>]
    // member this.WeightedCharsetSearch() =
    //     let regex = Regex(this.rs)
    //     let cache = regex.TSetMatcher.Cache
    //     let prefix = regex.InitialReversePrefix

    //     let prefixSets =
    //         match prefix with
    //         | InitialOptimizations.PotentialStartPrefix(prefixMem) ->
    //             Array.toList (prefixMem.ToArray()) |> List.rev
    //         | _ -> failwith "debug"

    //     let commonalityScore (charSet: char array) =
    //         charSet |> Array.map (fun c ->
    //             if Char.IsAsciiLetterLower c then 10
    //             else 0)
    //         |> Array.sum

    //     let weightedSets = prefixSets |> List.mapi (fun i set ->
    //         (i, set, commonalityScore (cache.MintermChars(set).ToArray())))
    //                        |> List.sortBy (fun (_, _, score) -> score )
    //                        |> List.map (fun (i, set, _) -> (i, set))

    //     let rarestCharSet = cache.MintermChars(snd weightedSets[0]).ToArray().AsMemory()
    //     let charSetIndex = fst weightedSets[0]
    //     let mutable searching = true
    //     let mutable matchPos = 0
    //     let textSpan = fullInput.AsSpan()
    //     // let potMatches = ResizeArray(100)

    //     while searching do
    //         match textSpan.Slice(matchPos).IndexOfAny(rarestCharSet.Span) with
    //         | -1 -> searching <- false
    //         | spanMatchStart when (spanMatchStart + matchPos - charSetIndex >= 0) ->
    //             let absMatchStart = spanMatchStart + matchPos - charSetIndex
    //             let mutable fullMatch = true
    //             let mutable i = 1
    //             while i < weightedSets.Length && absMatchStart + (fst weightedSets[i]) < textSpan.Length && fullMatch do
    //                 let set = cache.MintermChars(snd weightedSets[i])
    //                 if textSpan.Slice(absMatchStart + (fst weightedSets[i]), 1).IndexOfAny(set) = -1 then
    //                     fullMatch <- false
    //                 else
    //                     i <- i + 1
    //             matchPos <- absMatchStart + 1 + charSetIndex
    //             // if fullMatch then potMatches.Add({MatchPosition.Index = absMatchStart; Length = weightedSets.Length })
    //         | _ -> ()
    //     // potMatches


    // [<Benchmark>]
    // member this.NonWeightedCharsetSearch() =
    //     let regex = Regex(this.rs)
    //     let cache = regex.TSetMatcher.Cache
    //     let prefix = regex.InitialReversePrefix

    //     let prefixSets =
    //         match prefix with
    //         | InitialOptimizations.PotentialStartPrefix(prefixMem) ->
    //             Array.toList (prefixMem.ToArray()) |> List.rev
    //         | _ -> failwith "debug"

    //     let firstCharSet = cache.MintermChars(prefixSets[0]).ToArray().AsMemory()
    //     let mutable searching = true

    //     let mutable startPos = 0
    //     let textSpan = fullInput.AsSpan()
    //     // let potMatches = ResizeArray(100)
    //     while searching do
    //         match textSpan.Slice(startPos).IndexOfAny(firstCharSet.Span) with
    //         | -1 -> searching <- false
    //         | spanMatchStart ->
    //             let absMatchStart = spanMatchStart + startPos
    //             let mutable fullMatch = true
    //             let mutable i = 1
    //             while i < prefixSets.Length && absMatchStart + i < textSpan.Length && fullMatch do
    //                 let set = cache.MintermChars(prefixSets[i])
    //                 if textSpan.Slice(absMatchStart + i, 1).IndexOfAny(set) = -1 then
    //                     fullMatch <- false
    //                 else
    //                     i <- i + 1
    //             startPos <- absMatchStart + 1
    //             // if fullMatch then potMatches.Add({MatchPosition.Index = absMatchStart; Length = prefixSets.Length })
    //     // potMatches


    // [<Benchmark>]
    // member this.SbreCount() =
    //     let regex = Regex(this.rs)
    //     regex.Count fullInput
