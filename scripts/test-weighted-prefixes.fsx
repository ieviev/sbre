#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

// open System.Collections.Generic
open System
open System.Threading
open Microsoft.VisualBasic.CompilerServices
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types
open Sbre.Pat
open Sbre.Optimizations


let fullInput =
    __SOURCE_DIRECTORY__ + "/input-text.txt" |> System.IO.File.ReadAllText



/// Print binary value of the class and all characters in n character classes
let printAllCharClasses (cache: RegexCache<TSet>) n =
    for i in [0..n] do
        let bin = (String.replicate i "0") + "1" + (String.replicate (n - i) "0")
        printf $"Binary: %s{bin} "
        printfn $"Chars: %A{cache.MintermChars(Convert.ToUInt64(bin, 2)).ToArray()}"
    
/// Uint64 to binary
let rec ToBin (value: uint64) (len: int) =
   (if len > 1 then ToBin (value >>> 1) (len - 1) else "") + ("01"[int (value &&& 1UL)]).ToString()

/// Convert TSets to binary
let prefixesToBin (prefixes: TSet List) = List.map (fun x -> ToBin (uint64 x) 64) prefixes

/// Check if and how match positions overlap.
///
/// Returns:
/// * -1 if pos1 is before pos2 and does not overlap
/// * 1 if pos1 is after pos2 and does not overlap
/// * 0 if pos1 and pos2 overlap with at least 1 character
let howDoPositionsOverlap (pos1: MatchPosition) (pos2: MatchPosition) =
    if pos1.Index + pos1.Length <= pos2.Index then -1
    else if pos2.Index + pos2.Length <= pos1.Index then 1
    else 0



/// Metrics to use:
/// * Frequency of capital letters vs lowercase letters (in English texts, but the ratio should be similar
///     in other languages)
/// * Maybe frequency of individual letters (in English texts)
/// * Non-letter symbols
///
/// Capital letters make up about 3% of English text.
/// https://english.stackexchange.com/questions/43563/what-percentage-of-characters-in-normal-english-literature-is-written-in-capital
/// 
/// TODO: Maybe figure out how to take the derivative of a regex to skip false matches
/// regex2.TSetMatcher.CreateDerivative
let commonalityScore (charSet: char array) =
    charSet |> Array.map (fun c ->
        if Char.IsAsciiLetterLower c then 10
        else 0)
    |> Array.sum



let prefixSearchWeighted regexStr (text: String) =
    let regex = Regex(regexStr)
    let cache = regex.TSetMatcher.Cache
    let prefix = regex.InitialReversePrefix
    let prefixSets =
        match prefix with
        | InitialOptimizations.PotentialStartPrefix(prefixMem) -> 
            Array.toList (prefixMem.ToArray()) |> List.rev
        | _ -> failwith "debug"

    
    printfn "%A" (prefixSets)
    printfn ""
    printfn "%A" (prefixesToBin prefixSets)
    printfn ""
    printfn "%A" (printAllCharClasses cache 8)
    printfn ""
    printfn "%A" (Optimizations.printPrefixSets cache (prefixSets))
    printfn ""
    // "[A-Za-z];[kw];[ac];[Su]"

    let weightedSets = prefixSets |> List.mapi (fun i set ->
            (i, set, commonalityScore (cache.MintermChars(set).ToArray())))
                       |> List.sortBy (fun (_, _, score) -> score )
                       |> List.map (fun (i, set, _) -> (i, set))
    
    printfn "%A" weightedSets
    
    let rarestCharSet = cache.MintermChars(snd weightedSets[0]).ToArray().AsMemory()
    let charSetIndex = fst weightedSets[0]
    let mutable searching = true

    let mutable matchPos = 0
    let textSpan = text.AsSpan()
    let potMatches = ResizeArray(100)
    while searching do
        match textSpan.Slice(matchPos).IndexOfAny(rarestCharSet.Span) with
        | -1 -> searching <- false
        | spanMatchStart when (spanMatchStart + matchPos - charSetIndex >= 0) ->
            let absMatchStart = spanMatchStart + matchPos - charSetIndex
            // printfn $"Potential match start at %d{absMatchStart}"
            let mutable fullMatch = true
            let mutable i = 1
            while i < weightedSets.Length && absMatchStart + (fst weightedSets[i]) < textSpan.Length && fullMatch do
                let set = cache.MintermChars(snd weightedSets[i])
                // printfn $"Prefix {i}: %A{set.ToArray()}"
                if textSpan.Slice(absMatchStart + (fst weightedSets[i]), 1).IndexOfAny(set) = -1 then
                    fullMatch <- false
                else
                    i <- i + 1
            matchPos <- absMatchStart + 1 + charSetIndex
            if fullMatch then potMatches.Add({MatchPosition.Index = absMatchStart; Length = weightedSets.Length })
        | _ -> ()
    potMatches
    

let prefixSearchSimple regexStr (text: String) =
    let regex = Regex(regexStr)
    let cache = regex.TSetMatcher.Cache
    let prefix = regex.InitialReversePrefix
    let prefixSets =
        match prefix with
        | InitialOptimizations.PotentialStartPrefix(prefixMem) -> 
            Array.toList (prefixMem.ToArray()) |> List.rev
        | _ -> failwith "debug"

    
    // prefixSets
    // prefixesToBin prefixSets
    // printAllCharClasses cache 8

    // let prefixPretty = Optimizations.printPrefixSets cache (prefixSets)
    // "[A-Za-z];[kw];[ac];[Su]"

    // Search in default order
    let firstCharSet = cache.MintermChars(prefixSets[0]).ToArray().AsMemory()
    let mutable searching = true
    
    let mutable startPos = 0
    let textSpan = text.AsSpan()
    let potMatches = ResizeArray(100)
    while searching do
        match textSpan.Slice(startPos).IndexOfAny(firstCharSet.Span) with
        | -1 -> searching <- false
        | spanMatchStart ->
            let absMatchStart = spanMatchStart + startPos
            // printfn $"Potential match start at %d{absMatchStart}"
            let mutable fullMatch = true
            let mutable i = 1
            while i < prefixSets.Length && absMatchStart + i < textSpan.Length && fullMatch do
                let set = cache.MintermChars(prefixSets[i])
                // printfn $"Prefix {i}: %A{set.ToArray()}"
                if textSpan.Slice(absMatchStart + i, 1).IndexOfAny(set) = -1 then
                    fullMatch <- false
                else
                    i <- i + 1
            startPos <- absMatchStart + 1
            if fullMatch then potMatches.Add({MatchPosition.Index = absMatchStart; Length = prefixSets.Length })
    potMatches
    
    
// let rs = "Huck[a-zA-Z]+|Saw[a-zA-Z]+"
// let rs = "[a-zA-Z]+ckl|[a-zA-Z]+awy"
let rs = ".*have.*&.*there.*"
let sText = fullInput[4000000..4050000]
// let sText = fullInput[4000000..9000000]
let potMatchesS = prefixSearchSimple rs fullInput
let potMatchesW = prefixSearchWeighted rs sText
let realMatches = Regex(rs).MatchPositions(fullInput)

potMatchesS.Count
potMatchesW.Count
Regex(rs).Count(fullInput)


let matchComparerPrintLine (text: String) (startPos: int) (endPos: int) (format: int) =
    let padLimit = 100
    let mutable startOffset = 0
    let mutable endOffset = 0
    while startPos - startOffset > 0 && startOffset < padLimit && text[startPos - startOffset - 1] <> '\n' do
        // printfn $"Start offset {startOffset + 1}"
        startOffset <- startOffset + 1
    while endPos + endOffset < text.Length - 1 && endOffset < padLimit && text[endPos + endOffset + 1] <> '\n' do
        // printfn $"End offset {endOffset + 1}"
        endOffset <- endOffset + 1
    match format with
    | 0 ->
        printfn $"Overlap:   %s{text[ (startPos - startOffset) .. (endPos + endOffset) ]}"
    | -1 -> 
        printfn $"Potential: %s{text[ (startPos - startOffset) .. (endPos + endOffset) ]}"
    | 1 -> 
        printfn $"Missed:    %s{text[ (startPos - startOffset) .. (endPos + endOffset) ]}"
    | _ -> ()

let matchComparer
    (potentialMatches: System.Collections.Generic.IEnumerable<MatchPosition>)
    (actualMatches: System.Collections.Generic.IEnumerable<MatchPosition>)
    (text: String) =
    let mutable potEr = potentialMatches.GetEnumerator()
    let mutable actEr = actualMatches.GetEnumerator()
    let mutable potMore = potEr.MoveNext()
    let mutable actMore = actEr.MoveNext()
    let mutable pIndex = potEr.Current.Index
    let mutable aIndex = actEr.Current.Index
    while potMore || actMore do
        printfn $"P {pIndex} A {aIndex}"
        let potMatch = potEr.Current
        let actMatch = actEr.Current
        if (potMore && not actMore) || (pIndex + potMatch.Length <= aIndex) then
            matchComparerPrintLine text pIndex (pIndex + potMatch.Length - 1) -1
            potMore <- potEr.MoveNext()
        else if (actMore && not potMore) || (aIndex + actMatch.Length <= pIndex) then
            matchComparerPrintLine text aIndex (aIndex + actMatch.Length - 1) 1
            actMore <- actEr.MoveNext()
        else
            matchComparerPrintLine text (Math.Min(pIndex, aIndex))
                (Math.Max(pIndex + potMatch.Length, aIndex + actMatch.Length) - 1) 0
            potMore <- potEr.MoveNext()
            actMore <- actEr.MoveNext()
        pIndex <- potEr.Current.Index
        aIndex <- actEr.Current.Index



matchComparer potMatchesS realMatches sText

let count c = Seq.filter ((=) c) >> Seq.length

count '\n' fullInput

// priority of searches
// 1. [Su] // uppercase characters are rare
// 2. [kw] //
// 3. [ac] // vowels are usually more frequent
// 4. [A-Za-z] (compare either as TSet or System.Buffers.SearchValues)









