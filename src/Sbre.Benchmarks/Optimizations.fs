module Sbre.Benchmarks.Optimizations

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86
open BenchmarkDotNet.Attributes
open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.Optimizations
open System
open Sbre.Pat
open Sbre.Types
open System.Text.Json.Nodes
open System.Buffers
// let fullInput = __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText
let fullInput = __SOURCE_DIRECTORY__ + "/data/sherlock.txt" |> System.IO.File.ReadAllText
                |> String.replicate 100

let frequenciesJsonText = __SOURCE_DIRECTORY__ + "/data/charFreqWithControl.json"  |> System.IO.File.ReadAllText

let shortInput20k = fullInput[..19999] // 20k chars limit
let shortInput10k = fullInput[..1000] // 10k chars limit
let testInput = fullInput
                // |> String.replicate 10
                // |> String.replicate 100
// let testInput = "yabcabca"


let collectNullablePositionsNoSkip ( matcher: RegexMatcher<TSet>, loc: byref<Location> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            loc.Position <- loc.Position

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
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
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            matcher.TrySkipInitialRev(&loc, &currentStateId) |> ignore

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount

let loadJsonCharFrequencies (jsonText: string) =
    let json = JsonValue.Parse jsonText
    (json.Item "characters").AsArray() |> Seq.map (fun charFreq ->
        ((charFreq.Item "character").GetValue<char>(), (charFreq.Item "frequency").GetValue<float>())
        ) |> dict

let characterFreq = loadJsonCharFrequencies frequenciesJsonText

let commonalityScore (charSet: char array) =
    charSet |> Array.map (fun c ->
        if Char.IsAsciiLetterLower c then 10
        else 0)
    |> Array.sum

let commonalityScore3 (charSet: char array) =
    charSet |> Array.map (fun c ->
        if characterFreq.ContainsKey(c) then characterFreq.Item c
        else 0)
    |> Array.sum

let prefixSearchWeightedReversed (loc: byref<Location>) (cache: RegexCache<TSet>)
    (weightedSets: inref<(int * MintermSearchValues) list>) =
    let textSpan = loc.Input
    let rarestCharSet = snd weightedSets[0]
    let rarestCharSetIndex = fst weightedSets[0]
    let mutable searching = true

    let mutable prevMatch = loc.Position
    while searching do
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet.SearchValues) with
        // | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length <= textSpan.Length) ->
        | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length <= loc.Position) ->
            let absMatchStart = curMatch - rarestCharSetIndex
            let mutable fullMatch = true
            let mutable i = 1
            while i < weightedSets.Length && absMatchStart + (fst weightedSets[i]) < textSpan.Length && fullMatch do
                let set = snd weightedSets[i]
                if textSpan.Slice(absMatchStart + (fst weightedSets[i]), 1).IndexOfAny(set.SearchValues) = -1 then
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
    (weightedSets: inref<struct(int * MintermSearchValues) array>) =
    // (a * b) is a reference tuple, struct(a * b) is a struct tuple
    let textSpan = loc.Input
    let struct(rarestCharSetIndex, rarestCharSet) = weightedSets[0]
    let mutable searching = true
    let mutable prevMatch = loc.Position
    while searching do
        // todo: .SearchValues
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet.SearchValues) with
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
            // prevMatch <-
            //     if rarestCharSetIndex = 0 then absMatchStart - 1 else
            //     absMatchStart + rarestCharSetIndex
            prevMatch <- absMatchStart + rarestCharSetIndex
            if fullMatch && i = weightedSets.Length then
                searching <- false
                loc.Position <- absMatchStart + weightedSets.Length
        | -1 ->
            searching <- false
            loc.Position <- 0
        | outOfBounds -> prevMatch <- outOfBounds
    ()

let prefixSearchWeightedReversed3
    (loc: byref<Location>)
    (weightedSets: inref<struct(int * MintermSearchValues) array>) =
    let textSpan = loc.Input
    let currentPosition = loc.Position
    let charSetsCount = weightedSets.Length
    let struct(rarestCharSetIndex, rarestCharSet) = weightedSets[0]
    let mutable searching = true
    let mutable prevMatch = currentPosition
    while searching do
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet.SearchValues) with
        // | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + weightedSets.Length < textSpan.Length) ->
        | curMatch when (curMatch - rarestCharSetIndex >= 0 && curMatch - rarestCharSetIndex + charSetsCount <= currentPosition) ->
            let absMatchStart = curMatch - rarestCharSetIndex
            let mutable fullMatch = true
            let mutable i = 1
            while fullMatch && i < charSetsCount do
                let struct (weightedSetIndex,weightedSet) = weightedSets[i]
                if not (weightedSet.Contains(textSpan[absMatchStart + weightedSetIndex])) then
                    fullMatch <- false
                else
                    i <- i + 1
            // ?
            // prevMatch <-
            //     if rarestCharSetIndex = 0 then absMatchStart - 1 else
            //     absMatchStart + rarestCharSetIndex
            prevMatch <- absMatchStart + rarestCharSetIndex
            if fullMatch && i = charSetsCount then
                searching <- false
                loc.Position <- absMatchStart + charSetsCount
        | -1 ->
            searching <- false
            loc.Position <- 0
        | outOfBounds -> prevMatch <- outOfBounds
    ()


let collectNullablePositionsWeightedSkip ( matcher: RegexMatcher<TSet>, loc: byref<Location>, weightedSets: inref<(int * MintermSearchValues) list> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            prefixSearchWeightedReversed &loc matcher.Cache &weightedSets

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount

// ---------- slightly modified

let collectNullablePositionsWeightedSkip2 ( matcher: RegexMatcher<TSet>, loc: byref<Location>, weightedSets: inref<struct (int * MintermSearchValues) array> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            prefixSearchWeightedReversed2 &loc &weightedSets

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount

let collectNullablePositionsWeightedSkip3 ( matcher: RegexMatcher<TSet>, loc: byref<Location>, weightedSets: inref<struct (int * MintermSearchValues) array> ) =
    assert (loc.Position > -1)
    assert (loc.Reversed = true)
    let mutable looping = true
    let mutable currentStateId = matcher.GetOrCreateState(matcher.ReverseTrueStarredPattern).Id
    let _stateArray = matcher.DfaStateArray
    let mutable dfaState = _stateArray[currentStateId]
    let mutable nullableCount = 0

    while looping do
        dfaState <- _stateArray[currentStateId]
        let flags = dfaState.Flags
        if flags.IsInitial then
            prefixSearchWeightedReversed3 &loc &weightedSets

        if matcher.StateIsNullable(flags, &loc, currentStateId) then
            nullableCount <- nullableCount + 1

        if loc.Position > 0 then
            matcher.TakeTransition(flags, &currentStateId, &loc)
            loc.Position <- Location.nextPosition loc
        else
            looping <- false

    nullableCount




[<MemoryDiagnoser(true)>]
[<ShortRunJob>]
type PrefixCharsetSearch () =

    // let regex = Sbre.Regex("Huck[a-zA-Z]+|Saw[a-zA-Z]+")
    // let regex = Sbre.Regex("[a-zA-Z]+ckle|[a-zA-Z]+awy")
    // let regex = Sbre.Regex(".*have.*&.*there.*")

    let regex = Sbre.Regex("Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty")
    // Sets:          [IJlo];[or];[ ceh.0];[LMkn];[ eo];[ HWrs];[Aaiot];[adlrt];[almrs];[deot];[enrsy]
    // Weighted sets 1: [or];[ eo];[IJlo];[LMkn];[ HWrs];[ ceh];[deot];[Aaiot];[adlrt];[almrs];[enrsy]
    // Weighted sets 0: [IJlo];[or];[LMkn];[ eo];[ HWrs];[ ceh];[Aaiot];[deot];[adlrt];[almrs];[enrsy]

    // let regex = Sbre.Regex("abca|xxx")

    // let rs = "[a-zA-Z]+ckl|[a-zA-Z]+awy"
    // [<Params("[a-zA-Z]+ckle|[a-zA-Z]+awy", "Huck[a-zA-Z]+|Saw[a-zA-Z]+", ".*have.*&.*there.*")>]
    // member val rs: string = "" with get, set


    let cache = regex.TSetMatcher.Cache
    let matcher = regex.TSetMatcher
    let optimizations = matcher.InitialOptimizations

    let reversedPrefixSpan =
        match optimizations with
        | InitialOptimizations.SetsPotentialStart(prefix) -> prefix
        | InitialOptimizations.SearchValuesPotentialStart(_,prefix) -> prefix
        | InitialOptimizations.SetsPrefix(prefix, stateId) -> prefix
        | _ -> failwith "todo"

    let prefixSets =
        match optimizations with
        | InitialOptimizations.SetsPotentialStart(prefixMem) ->
            Array.toList (prefixMem.ToArray()) |> List.rev
        | InitialOptimizations.SearchValuesPotentialStart(_,prefixMem) ->
            Array.toList (prefixMem.ToArray()) |> List.rev
        | InitialOptimizations.SetsPrefix(prefix, transitionNodeId) ->
            Array.toList (prefix.ToArray()) |> List.rev
        | _ -> failwith "debug"

    let test = cache.MintermSearchValues(prefixSets[0])
    let weightedSets = prefixSets |> List.mapi (fun i set ->
            (i, set, commonalityScore (cache.MintermChars(set).Value.Span.ToArray())))
                       |> List.sortBy (fun (_, _, score) -> score )
                       |> List.map (fun (i, set, _) -> (i, set))
    let weightedSets3 = prefixSets |> List.mapi (fun i set ->
            (i, set, commonalityScore3 (cache.MintermChars(set).Value.Span.ToArray())))
                       |> List.sortBy (fun (_, _, score) -> score )
                       |> List.map (fun (i, set, _) -> (i, set))


    let weightedCharsetsArray1 =
        weightedSets
        |> Seq.map (fun (int,tset) ->
            let chars = matcher.Cache.MintermSearchValues(tset)
            (int, chars)
        )
        |> Seq.toList
        // |> Seq.toArray


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


    let weightedCharsetsArray3 =
        weightedSets3
        |> Seq.map (fun (int,tset) ->
            let chars = matcher.Cache.MintermSearchValues(tset)
            struct(int, chars)
        )
        |> Seq.toArray


    // [<Benchmark>]
    // member this.NoSkip() =
    //     let textSpan = fullInput.AsSpan()
    //     let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
    //     collectNullablePositionsNoSkip (matcher, &loc)

    [<Benchmark>]
    member this.Original() =
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsOriginal (matcher, &loc)


    // [<Benchmark>]
    member this.Weighted() =
        // let a = Optimizations.printPrefixSets cache (prefixSets)
        // let b = Optimizations.printPrefixSets cache (weightedSets |> List.map snd )
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsWeightedSkip (matcher, &loc, &weightedCharsetsArray1)

    [<Benchmark>]
    member this.Weighted2() =
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsWeightedSkip2 (matcher, &loc, &weightedCharsetsArray2)
        // |> (function 8562 -> () | n -> failwith $"invalid result {n}") // sanity check for .*have.*&.*there.*

    [<Benchmark>]
    member this.Weighted3() =
        let textSpan = testInput.AsSpan()
        let mutable loc = Location.createReversedSpan textSpan // end position, location reversed
        collectNullablePositionsWeightedSkip3 (matcher, &loc, &weightedCharsetsArray3)



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

[<MemoryDiagnoser>]
[<ShortRunJob>]
// [<AbstractClass>]
type StringPrefix(pattern:string) =
    let regex = Sbre.Regex(pattern)
    let matcher = regex.TSetMatcher
    // find optimized prefix for regex
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )

    let charToTSet (chr:char) = matcher.Cache.CharToMinterm(chr)
    let isElemOfSet (tset1:TSet) (tset2:TSet) = Solver.elemOfSet tset1 tset2

    let svals = [|'n'|].AsMemory()

    // [<Benchmark>]
    // member x.SpanIndexOf() =
    //     // let tc = fullInput.AsSpan().Count("Twain")
    //     let tc = fullInput.AsSpan().Count(")")
    //     ()
        // if tc <> 811 then failwith $"invalid count {tc}"
        // if tc <> 2673 then failwith $"invalid count {tc}"

    [<Benchmark>]
    member x.SpanIndexOf1() =
        let span = fullInput.AsSpan()
        let mutable currpos = fullInput.Length
        let mutable looping = true
        let mutable tc = 0
        let tlen = "Twain".Length
        while looping do
            // vectorize only to first char
            let slice = span.Slice(0,currpos)
            let newPos = slice.IndexOfAny(svals.Span)
            if newPos = -1 || newPos < tlen then looping <- false else
            currpos <- newPos
            let mstart = currpos - tlen + 1
            let validStart = slice.Slice(mstart).StartsWith("Twain")
            if validStart then
                tc <- tc + 1
                currpos <- mstart
            else currpos <- currpos - 1
        if tc <> 811 then failwith $"invalid count: {tc}"


    // member x.VecLastIndex(vecSpans:ReadOnlySpan<Vector256<uint16>>) =
    //     let enumerator = vecSpans.Slice(0, )
    //     // for (var i = 0; i < vInts.Length; i++)
    //     // {
    //     //     var result = Vector256.Equals(vInts[i], compareValue);
    //     //     if (result == Vector256<int>.Zero) continue;
    //     //
    //     //     for (var k = 0; k < vectorLength; k++)
    //     //         if (result.GetElement(k) != 0)
    //     //             return i * vectorLength + k;
    //     // }

    [<Benchmark>]
    member x.SpanIndexOf2() =
        let origspan = fullInput.AsSpan()
        let mutable tc = 0
        let alignAmount = origspan.Length % 16
        let alignSpan = origspan.Slice(alignAmount)
        let inputVectors = MemoryMarshal.Cast<char, Vector256<uint16>>(alignSpan)
        let searchVector = Vector256.Create<uint16>(uint16 'n')
        let onevec = Vector256<uint16>.AllBitsSet
        let idx = inputVectors.Length - 1
        let tlen = "Twain".Length
        let outArray = Array.zeroCreate<uint16> 16
        let outSpan = outArray.AsSpan()

        for i = idx downto 0 do
            let result = Vector256.Equals(inputVectors[i], searchVector)
            if not (Vector256.EqualsAny(result, onevec)) then () else
            Vector256.CopyTo(result,outSpan)
            for j = 0 to 15 do
                if outSpan[j] <> 0us then
                    if j > 0 && inputVectors[i][j-1] <> uint16 'i' then () else
                    let absoluteIndex = (i * 16) + j
                    let mstart = absoluteIndex - tlen + 1
                    let validStart = alignSpan.Slice(mstart).StartsWith("Twain")
                    if validStart then
                        tc <- tc + 1
        if tc <> 811 then failwith $"invalid count: {tc}"




[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
[<ShortRunJob>]
[<AbstractClass>]
type SetsPrefix(pattern:string) =
    let regex = Sbre.Regex(pattern)
    let matcher = regex.TSetMatcher
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    // find optimized prefix for regex
    let optimizations =
        Sbre.Optimizations.findInitialOptimizations
            getder
            (fun node -> matcher.GetOrCreateState(node).Id)
            (fun node -> matcher.GetOrCreateState(node).Flags)
            matcher.Cache
            matcher.ReversePattern
            matcher.ReverseTrueStarredPattern
    let prefixSets =
        match optimizations with
        | InitialOptimizations.SetsPotentialStart(prefix) ->
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
        let firstSetChars = matcher.Cache.MintermChars(prefixSets[0]).Value.Span
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