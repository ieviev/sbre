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
let fullInput =
    __SOURCE_DIRECTORY__ + "/data/input-text.txt" |> System.IO.File.ReadAllText

let shortInput20k = fullInput[..19999] // 20k chars limit
let shortInput10k = fullInput[..9999] // 10k chars limit


[<BenchmarkDotNet.Attributes.MemoryDiagnoser>]
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
