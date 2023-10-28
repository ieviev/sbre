module Sbre.Startsets

open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Pat
open Sbre.Types
open System
open Info

type Jump() =

    static member toValidStartPosition(
        cache:RegexCache<'t>,
        loc:byref<Location>,
        startsetChars: PredStartset,
        expectedNextMinterm:'t) =

        let mutable successfulSkip = false
        while not successfulSkip do
            let slice = loc.Input.AsSpan().Slice(loc.Position)
            match startsetChars.Flags with
            // TBD: unimplemented cases
            | StartsetFlags.IsFull -> successfulSkip <- true
            | StartsetFlags.IsEmpty -> successfulSkip <- true
            | StartsetFlags.Inverted ->
                let sharedIndex = slice.IndexOfAnyExcept(startsetChars.Chars)
                if sharedIndex = -1 then
                    successfulSkip <- true
                else
                    loc.Position <- loc.Position + sharedIndex
                    if slice.Length < sharedIndex + 1 then
                        successfulSkip <- true
                    else successfulSkip <- true
            | _ ->
            let sharedIndex = slice.IndexOfAny(startsetChars.Chars)
            if sharedIndex = -1 then
                successfulSkip <- true
            else
                loc.Position <- loc.Position + sharedIndex

                if Location.isPreFinal loc then
                    successfulSkip <- true
                else

                if slice.Length < sharedIndex + 1 then
                    successfulSkip <- true
                else
                let nextmt =
                    if loc.Reversed then
                        cache.CharToMinterm(&slice[sharedIndex - 2])
                    else
                        cache.CharToMinterm(&slice[sharedIndex + 1])
#if DEBUG
                // let d_pred = cache.PrettyPrintMinterm(cache.initialMintermsArray[1])
                // let d_loc = cache.PrettyPrintMinterm(nextmt)
#endif

                if cache.Solver.isElemOfSet(expectedNextMinterm,nextmt) then
                    successfulSkip <- true
                else
                    loc.Position <- loc.Position + 1
                    ()
        ()




let createStartsetCharsOfMinterm(s:ISolver<'t>,css:CharSetSolver, minterm: 't) =
        match minterm with
        | _ when s.IsFull minterm ->
            (PredStartset.Of(StartsetFlags.IsFull, [||]))
        | _ when s.IsEmpty minterm  ->
            (PredStartset.Of(StartsetFlags.IsEmpty, [||]))
        | _ ->
            let firstbdd = s.ConvertToBDD(minterm, css)
            let rcc = RegexCharClass()
            let mutable cc = 0u // char-count
            let mutable i = 0   //loop index
            let mutable setTooBig = false
            let charArray = ResizeArray<char>()
            let mutable e = BDDRangeConverter.ToRanges(firstbdd) //.GetEnumerator()
            while i < (e.Length) && not setTooBig do

                let struct (rs,re) = e[i]
                if (cc + (re - rs)) > 10000u then setTooBig <- true
                else
                rcc.AddRange(char rs, char re)
                for j = int rs to int re do
                    charArray.Add(char j)
                    cc <- cc + 1u
                i <- i + 1
            if setTooBig then
#if DIAGNOSTIC
                logDiagnostic($"startset too big at {cc}, inverting set")
#endif
                i <- 0
                setTooBig <- false
                cc <- 0u
                charArray.Clear()
                let invertedBdd = s.ConvertToBDD(s.Not(minterm), css)
                let invertedRanges = BDDRangeConverter.ToRanges(invertedBdd)
                e <- BDDRangeConverter.ToRanges(invertedBdd)
                while i < (e.Length) && not setTooBig do
                    let struct (rs,re) = e[i]
                    rcc.AddRange(char rs, char re)
                    for j = int rs to int re do
                        charArray.Add(char j)
                        cc <- cc + 1u
                    i <- i + 1
                let setChars = charArray.ToArray()
                PredStartset.Of(StartsetFlags.Inverted,setChars)

            else
                let setChars = charArray.ToArray()
                PredStartset.Of(StartsetFlags.None,setChars)

