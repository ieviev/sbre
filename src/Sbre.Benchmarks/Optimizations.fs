module Sbre.Benchmarks.Optimizations

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


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
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

[<MemoryDiagnoser(false)>]
[<ShortRunJob>]
type PrefixCharsetSearch () =
    // let rs = "[a-zA-Z]+ckl|[a-zA-Z]+awy"
    [<Params("[a-zA-Z]+ckle|[a-zA-Z]+awy", "Huck[a-zA-Z]+|Saw[a-zA-Z]+", ".*have.*&.*there.*")>]
    member val rs: string = "" with get, set
    
    
    [<Benchmark>]
    member this.WeightedCharsetSearch() =
        let regex = Regex(this.rs)
        let cache = regex.TSetMatcher.Cache
        let prefix = regex.InitialReversePrefix
        
        let prefixSets =
            match prefix with
            | InitialOptimizations.PotentialStartPrefix(prefixMem) -> 
                Array.toList (prefixMem.ToArray()) |> List.rev
            | _ -> failwith "debug"
            
        let commonalityScore (charSet: char array) =
            charSet |> Array.map (fun c ->
                if Char.IsAsciiLetterLower c then 10
                else 0)
            |> Array.sum
            
        let weightedSets = prefixSets |> List.mapi (fun i set ->
            (i, set, commonalityScore (cache.MintermChars(set).ToArray())))
                           |> List.sortBy (fun (_, _, score) -> score )
                           |> List.map (fun (i, set, _) -> (i, set))
        
        let rarestCharSet = cache.MintermChars(snd weightedSets[0]).ToArray().AsMemory()
        let charSetIndex = fst weightedSets[0]
        let mutable searching = true
        let mutable matchPos = 0
        let textSpan = fullInput.AsSpan()
        // let potMatches = ResizeArray(100)
        
        while searching do
            match textSpan.Slice(matchPos).IndexOfAny(rarestCharSet.Span) with
            | -1 -> searching <- false
            | spanMatchStart when (spanMatchStart + matchPos - charSetIndex >= 0) ->
                let absMatchStart = spanMatchStart + matchPos - charSetIndex
                let mutable fullMatch = true
                let mutable i = 1
                while i < weightedSets.Length && absMatchStart + (fst weightedSets[i]) < textSpan.Length && fullMatch do
                    let set = cache.MintermChars(snd weightedSets[i])
                    if textSpan.Slice(absMatchStart + (fst weightedSets[i]), 1).IndexOfAny(set) = -1 then
                        fullMatch <- false
                    else
                        i <- i + 1
                matchPos <- absMatchStart + 1 + charSetIndex
                // if fullMatch then potMatches.Add({MatchPosition.Index = absMatchStart; Length = weightedSets.Length })
            | _ -> ()
        // potMatches

    
    [<Benchmark>]
    member this.NonWeightedCharsetSearch() =
        let regex = Regex(this.rs)
        let cache = regex.TSetMatcher.Cache
        let prefix = regex.InitialReversePrefix
        
        let prefixSets =
            match prefix with
            | InitialOptimizations.PotentialStartPrefix(prefixMem) -> 
                Array.toList (prefixMem.ToArray()) |> List.rev
            | _ -> failwith "debug"

        let firstCharSet = cache.MintermChars(prefixSets[0]).ToArray().AsMemory()
        let mutable searching = true
        
        let mutable startPos = 0
        let textSpan = fullInput.AsSpan()
        // let potMatches = ResizeArray(100)
        while searching do
            match textSpan.Slice(startPos).IndexOfAny(firstCharSet.Span) with
            | -1 -> searching <- false
            | spanMatchStart ->
                let absMatchStart = spanMatchStart + startPos
                let mutable fullMatch = true
                let mutable i = 1
                while i < prefixSets.Length && absMatchStart + i < textSpan.Length && fullMatch do
                    let set = cache.MintermChars(prefixSets[i])
                    if textSpan.Slice(absMatchStart + i, 1).IndexOfAny(set) = -1 then
                        fullMatch <- false
                    else
                        i <- i + 1
                startPos <- absMatchStart + 1
                // if fullMatch then potMatches.Add({MatchPosition.Index = absMatchStart; Length = prefixSets.Length })
        // potMatches

    
    [<Benchmark>]
    member this.SbreCount() =
        let regex = Regex(this.rs)
        regex.Count fullInput
