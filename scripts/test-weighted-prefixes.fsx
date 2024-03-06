#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

// open System.Collections.Generic
open System
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open System.Threading
open Microsoft.FSharp.Core
open Microsoft.VisualBasic.CompilerServices
open Sbre
open FSharp.Data
open System.Globalization
open Sbre.Types
open Sbre.Pat
open Sbre.Optimizations



let li = [1; 2; 3]
li[0..1]


let regex = Regex(@"\w+nn\W")
let cache = regex.TSetMatcher.Cache
let prefix = regex.InitialReversePrefix
let prefixSets = // [6UL; 4UL; 4UL; 1UL]
    match prefix with
    | InitialOptimizations.PotentialStartPrefix(prefixMem) -> 
        Array.toList (prefixMem.ToArray()) |> List.rev
    | InitialOptimizations.SetsPrefix(prefixMem, _) ->
        Array.toList (prefixMem.ToArray()) |> List.rev
    | _ -> failwith "?"

cache.PrettyPrintMinterm 6UL                  // [0-9A-Z_a-z\u00AA...] -> '\w'
cache.MintermSearchValues(6UL).Contains('n')  // true
cache.MintermSearchValues(6UL).Contains('a')  // false ??
(cache.MintermChars 6UL).ToString()           // 'n'   ??

cache.PrettyPrintMinterm 4UL                  // 'n'
(cache.MintermChars 4UL).ToString()           // 'n'

cache.PrettyPrintMinterm 1UL                  // \W
cache.MintermSearchValues(1UL).Contains('a')  // false
cache.MintermSearchValues(1UL).Contains(' ')  // false ??
cache.MintermSearchValues(1UL).Contains('n')  // true  ??
(cache.MintermChars 1UL).ToString()           // 'n'   ??



cache.PrettyPrintMinterm 1UL // '\W'
cache.PrettyPrintMinterm 2UL // '\w' - 'n'
cache.PrettyPrintMinterm 4UL // 'n'
cache.IsInverted 1UL // true
cache.IsInverted 2UL // false
cache.IsInverted 4UL // false

cache.Solver.isElemOfSet (cache.Classify(' '), 1UL)
cache.Solver.isElemOfSet (cache.Classify('n'), 2UL)

cache.IsInverted 6UL // false


cache.PrettyPrintMinterm 72UL
(cache.MintermChars 18UL).ToString()
(cache.MintermChars 36UL).ToString()
(cache.MintermChars 72UL).ToString()

cache.Classify('n')


let allChars = 6UL
let onlyN = 4UL
let allNonChars = 1UL
cache.PrettyPrintMinterm qwe
cache.IsInverted qwe
(cache.MintermChars qwe).ToString()
cache.CharToMinterm ' '













let fullInput = __SOURCE_DIRECTORY__ + "/input-text.txt" |> System.IO.File.ReadAllText
let smallText = "ABCabcabcd"


let frequenciesJsonText = __SOURCE_DIRECTORY__ + "/characterWeights.json"  |> System.IO.File.ReadAllText


let count c = Seq.filter ((=) c) >> Seq.length


// Count how many times all characters occur in the text
let findLetterFrequency(text: string) =
    text |> Seq.distinct
    // |> Seq.filter (fun c -> not (Char.IsControl(c)))
    |> Seq.map (fun (c: char) -> (c, count c text))
    |> Seq.sortByDescending snd
    |> Array.ofSeq

let freq = findLetterFrequency fullInput

let writeCharFrequenciesToJsonFile (frequencyByType: (char * int) array) filename =
    let charFreqJsonArray =
        frequencyByType |> Array.map (fun (character, count) ->
        $"{{\"character\": \"{JsonEncodedText.Encode(Char.ToString(character)).Value}\", \"frequency\": %.6f{float 100 * (float count) / (float fullInput.Length)}}}"
        ) |> String.concat ","
    let fullJson = "{\"characters\": [" + charFreqJsonArray + "]}"
    // Console.WriteLine(fullJson)
    let res = (JsonValue.Parse fullJson).ToJsonString(JsonSerializerOptions(WriteIndented = true))
    System.IO.File.WriteAllText(filename, res)
    ()
    
let a = writeCharFrequenciesToJsonFile freq (__SOURCE_DIRECTORY__ + "/charFreqWithControl.json")











// Count how many times different types of characters occur
let countFrequencyTypes (charCounts: (char * int) array) =
    let mutable uppercase = 0
    let mutable lowercase = 0
    let mutable number = 0
    let mutable newline = 0
    let mutable space = 0
    let mutable other = 0
    for charCount in charCounts do
        let character, count = charCount
        match character with
            | c when Char.IsUpper(c) -> uppercase <- uppercase + count
            | c when Char.IsLower(c) -> lowercase <- lowercase + count
            | c when Char.IsNumber(c) -> number <- number + count
            | '\n' -> newline <- newline + count
            | ' ' -> space <- space + count
            | _ -> other <- other + count
    [| "uppercase", uppercase; "lowercase", lowercase; "number", number; "newline", newline
       "space", space; "other", other|]

let freqByType = countFrequencyTypes freq

// Print what percentage of a text character types make up
let printFreqTypeProportions frequencyByType =
    frequencyByType |> Array.map (fun (charType, count) ->
            printfn $"{charType}: {100f * float32 count / float32 fullInput.Length}"
        )

printFreqTypeProportions freqByType

// Print what percentage of a text characters make up
let printCharFrequencyProportions charFrequnecies =
    charFrequnecies |> Array.sortByDescending snd |> Array.map (fun (character, count) ->
            printfn $"{character}: {100f * float32 count / float32 fullInput.Length}"
        )

printCharFrequencyProportions freq

// Load character frequencies from JSON file
let loadJsonCharFrequencies (jsonText: string) =
    let json = JsonValue.Parse jsonText
    (json.Item "characters").AsArray() |> Seq.map (fun charFreq ->
        ((charFreq.Item "character").GetValue<char>(), (charFreq.Item "frequency").GetValue<float>())
        ) |> dict

loadJsonCharFrequencies frequenciesJsonText










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
    
    printfn "%A" prefixSets

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
    


// let rs = "Huck[a-zA-Z]+|Saw[a-zA-Z]+"
// let rs = "[a-zA-Z]+ckl|[a-zA-Z]+awy"
let rs = ".*have.*&.*there.*"
let sText = fullInput[4000000..4050000]
// let sText = fullInput[4000000..9000000]
// let potMatchesS = prefixSearchSimple rs fullInput
let potMatchesW = prefixSearchWeighted rs fullInput
// let realMatches = Regex(rs).MatchPositions(fullInput)
let realMatches = Regex(rs).MatchPositions(fullInput)

// potMatchesS.Count
potMatchesW.Count
Regex(rs).Count(sText)
Regex(rs).Count(fullInput)


printfn "%A" (potMatchesW.ToArray())


let testText = "Lorem ipsum dolor"
let testRs = "[a-z]+sum|[a-z]+lor"
let testPot = prefixSearchWeighted testRs testText
let testAct = Regex(testRs).MatchPositions(testText)
testPot.Count
printfn "%A" (testPot.ToArray())









let count c = Seq.filter ((=) c) >> Seq.length


let matchComparer
    (potentialMatches: System.Collections.Generic.IEnumerable<MatchPosition>)
    (actualMatches: System.Collections.Generic.IEnumerable<MatchPosition>)
    (text: String) =
    let mutable potEnumerator = potentialMatches.GetEnumerator()
    let mutable actEnumerator = actualMatches.GetEnumerator()
    let mutable hasPotMore = try potEnumerator.MoveNext() with | _ -> false
    let mutable hasActMore = try actEnumerator.MoveNext() with | _ -> false
    let mutable potMatchStart = try potEnumerator.Current.Index with | _ -> 0
    let mutable actMatchStart = try actEnumerator.Current.Index with | _ -> 0
    let mutable potMatchEnd = try potMatchStart + potEnumerator.Current.Length with | _ -> 0
    let mutable actMatchEnd = try actMatchStart + actEnumerator.Current.Length with | _ -> 0
    let mutable newLineCount = 1
    // let mutable newLineCount = count '\n' fullInput[0 .. 4000000] + 1
    let mutable newLineIndex = 0
    while hasPotMore || hasActMore do
        // Find highlight regions
        // pot: + -   act: ^ ~   both: =   empty: ' '
        let includesPot = hasPotMore && (not hasActMore || actMatchEnd >= potMatchStart)
        let includesAct = hasActMore && (not hasPotMore || potMatchEnd >= actMatchStart)
        
        // printfn $"Includes potential match: %b{includesPot} Includes actual match: %b{includesAct}"
        // printfn $"Potential match indexes: %d{potMatchStart}  %d{potMatchEnd}"
        // printfn $"Actual    match indexes: %d{actMatchStart}  %d{actMatchEnd}"
        
        let mutable changePoints = []
        if includesPot then changePoints <- [(potMatchStart, '+'); (potMatchEnd, '-')]
        if includesAct then changePoints <- changePoints @ [(actMatchStart, '^'); (actMatchEnd, '~')]
        
        let mutable state = ' '
        let mutable stateChanges = changePoints |> List.sortBy fst |> List.map (fun (i, c) ->
            match state with
            | ' ' -> state <- c
            | '=' -> state <- if c = '-' then '^' else '+'
            | '+' -> state <- if c = '-' then ' ' else '='
            | '^' -> state <- if c = '~' then ' ' else '='
            | _ -> ()
            (i, state))
        
        // Find start and end positions
        let matchStartPos = if includesPot && includesAct then Math.Min(potMatchStart, actMatchStart) elif includesPot then potMatchStart else actMatchStart
        let matchEndPos = if includesPot && includesAct then Math.Min(potMatchEnd, actMatchEnd)  elif includesPot then potMatchEnd else actMatchEnd
        // Find entire match line indexes or 100 chars around the match
        let padLimit = 100
        let mutable startPad = 0
        let mutable endPad = 0
        while matchStartPos - startPad > 0 && startPad < padLimit && text[matchStartPos - startPad - 1] <> '\n' do
            startPad <- startPad + 1
        while matchEndPos + endPad < text.Length - 1 && endPad < padLimit && text[matchEndPos + endPad + 1] <> '\n' do
            endPad <- endPad + 1
        // Find line number
        newLineCount <- newLineCount + count '\n' text[newLineIndex .. matchStartPos - 1]
        // Ensure last highlight region gets shown
        stateChanges <- stateChanges @ [(matchEndPos + endPad + 1, ' ')]
        // Create highlight string
        state <- ' '
        let mutable prevIndex = matchStartPos - startPad
        let highlights = stateChanges |> List.map (fun (i, c) ->
            let res = String.replicate (i - prevIndex) (state.ToString())
            state <- c
            prevIndex <- i
            res)
        
        // Print match line and highlight string
        let matchText = text[ (matchStartPos - startPad) .. (matchEndPos + endPad) ]
        printfn ""  // Without this FSI freezes for some reason
        if includesAct && not includesPot then printfn "Missed match!!!"
        Console.Write("Line " + newLineCount.ToString() + ": ")
        Console.WriteLine(matchText)
        printfn "%s%s" (String.replicate (7 + newLineCount.ToString().Length) " ") (String.concat "" highlights)
        
        // Move to next match(es)
        newLineIndex <- matchStartPos
        if includesPot then hasPotMore <- try potEnumerator.MoveNext() with | _ -> false
        if includesAct then hasActMore <- try actEnumerator.MoveNext() with | _ -> false
        potMatchStart <- try potEnumerator.Current.Index with | _ -> 0
        actMatchStart <- try actEnumerator.Current.Index with | _ -> 0
        potMatchEnd <- try potMatchStart + potEnumerator.Current.Length with | _ -> 0
        actMatchEnd <- try actMatchStart + actEnumerator.Current.Length with | _ -> 0
    ()


let shlock = __SOURCE_DIRECTORY__ + "/sherlock.txt" |> System.IO.File.ReadAllText

let regex = Regex(@"(?i)\w+nn\W")
regex.Matcher.Count(shlock.AsSpan())
regex.Matcher.Matches(shlock.AsSpan())


let shlockMatches = regex.Matcher.MatchPositions(shlock.AsSpan())

matchComparer Seq.empty shlockMatches shlock



// matchComparer potMatchesW realMatches fullInput


// matchComparer testPot testAct testText

// count '\n' fullInput


// priority of searches
// 1. [Su] // uppercase characters are rare
// 2. [kw] //
// 3. [ac] // vowels are usually more frequent
// 4. [A-Za-z] (compare either as TSet or System.Buffers.SearchValues)






let prefixSearchWeightedReversed regexStr (text: String) =
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
    
    // printf "Prefix sets: "
    printfn "%A" prefixSets

    let weightedSets = prefixSets |> List.mapi (fun i set ->
            (i, set, commonalityScore (cache.MintermChars(set).ToArray())))
                       |> List.sortBy (fun (_, _, score) -> score )
                       |> List.map (fun (i, set, _) -> (i, set))
    
    // printf "Weighted prefix sets: "
    printfn "%A" weightedSets
    
    let rarestCharSet = cache.MintermChars(snd weightedSets[0]).ToArray().AsMemory()
    let charSetIndex = fst weightedSets[0]
    let mutable searching = true

    let mutable prevMatch = text.Length
    let textSpan = text.AsSpan()
    let potMatches = ResizeArray(100)
    while searching do
        // printfn "Previous match: %d" prevMatch
        // printfn "Looking at \"%s\"" (textSpan.Slice(0, prevMatch).ToString())
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet.Span) with
        | curMatch when (curMatch - charSetIndex >= 0) ->
            let absMatchStart = curMatch - charSetIndex
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
            prevMatch <- absMatchStart + charSetIndex
            if fullMatch && i = weightedSets.Length then
                // printfn "Match at %d" prevMatch
                potMatches.Add({MatchPosition.Index = absMatchStart; Length = weightedSets.Length })
        | -1 | _  -> searching <- false
    potMatches
    


let rsr = "Huck[a-zA-Z]+|Saw[a-zA-Z]+"
// let rs = "[a-zA-Z]+ckl|[a-zA-Z]+awy"
// let rs = ".*have.*&.*there.*"
let sTextr = fullInput[4000000..4050000]
// let sTextr = "Lorem ipsum Sawyer dolor"
// let sText = fullInput[4000000..9000000]
// let potMatchesS = prefixSearchSimple rs fullInput
let potMatchesWR = prefixSearchWeightedReversed rsr sTextr
// let realMatches = Regex(rs).MatchPositions(fullInput)

// potMatchesS.Count
potMatchesWR.Count
Regex(rsr).Count(sTextr)
Regex(rsr).Count(fullInput)


printfn "%A" (potMatchesWR.ToArray())







let prefixSearchWeightedReversed regexStr (text: String) =
    let regex = Regex(regexStr)
    let cache = regex.TSetMatcher.Cache
    let prefix = regex.InitialReversePrefix
    let prefixSets =
        match prefix with
        | InitialOptimizations.PotentialStartPrefix(prefixMem) -> 
            Array.toList (prefixMem.ToArray()) |> List.rev
        | _ -> failwith "debug"

    let weightedSets = prefixSets |> List.mapi (fun i set ->
            (i, set, commonalityScore (cache.MintermChars(set).ToArray())))
                       |> List.sortBy (fun (_, _, score) -> score )
                       |> List.map (fun (i, set, _) -> (i, set))
    
    let rarestCharSet = cache.MintermChars(snd weightedSets[0]).ToArray().AsMemory()
    let charSetIndex = fst weightedSets[0]
    let mutable searching = true

    let mutable prevMatch = text.Length
    let textSpan = text.AsSpan()
    let potMatches = ResizeArray(100)
    while searching do
        match textSpan.Slice(0, prevMatch).LastIndexOfAny(rarestCharSet.Span) with
        | curMatch when (curMatch - charSetIndex >= 0) ->
            let absMatchStart = curMatch - charSetIndex
            let mutable fullMatch = true
            let mutable i = 1
            while i < weightedSets.Length && absMatchStart + (fst weightedSets[i]) < textSpan.Length && fullMatch do
                let set = cache.MintermChars(snd weightedSets[i])
                if textSpan.Slice(absMatchStart + (fst weightedSets[i]), 1).IndexOfAny(set) = -1 then
                    fullMatch <- false
                else
                    i <- i + 1
            prevMatch <- absMatchStart + charSetIndex
            if fullMatch && i = weightedSets.Length then
                potMatches.Add({MatchPosition.Index = absMatchStart; Length = weightedSets.Length })
        | -1 | _  -> searching <- false
    potMatches