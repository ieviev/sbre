[<Xunit.Collection("Sequential")>]
module Sbre.Test._03_NullabilityTests


#if DEBUG

open System
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre
open Sbre.CountingSet
open Sbre.Info
open Sbre.Pat
open Sbre.Types
open Xunit
open Common

module Helpers =
    let charSetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()
    let bddBuilder = SymbolicRegexBuilder<BDD>(charSetSolver, charSetSolver)


[<Fact>]
let ``nullable 01``() =
    let regex = Regex(".{2}c")
    let matcher = regex.TSetMatcher
    let state = RegexState(matcher.Cache.NumOfMinterms())
    let loc = Location.create "" 0
    let node = matcher.RawPattern
    let nullable = matcher.IsNullable(state,&loc,node)
    assertFalse nullable


#if TODO

[<Fact>]
let ``startset generation 1``() =
    let matcher = Regex(@"⊤*Huck⊤*").TSetMatcher
    let c = matcher.Cache

    // let ss1 = c.InitialPatternWithoutDotstar.TryGetInfo.Value.Startset Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("H", ss1pretty)


[<Fact>]
let ``startset generation 2``() =

    let matcher = Regex(@"⊤*(Huck|Finn)⊤*").TSetMatcher
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("[FH]", ss1pretty)



[<Fact>]
let ``startset generation 3``() =

    let matcher = Regex(@"\n\n~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*&⊤*Paris⊤*").TSetMatcher
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[\nEKP]", ss1pretty)



[<Fact>]
let ``startset generation 4``() =
    let matcher = Regex(@"(b|)*").TSetMatcher
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[]", ss1pretty)


[<Fact>]
let ``startset generation 5``() =
    let matcher = Regex(@"(a|ab)*").TSetMatcher
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@".", ss1pretty)



[<Fact>]
let ``startset generation 6``() =

    let matcher = Regex(@"⊤*Finn⊤*&⊤*Huck⊤*").TSetMatcher
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal("[FH]", ss1pretty)


[<Fact>]
let ``startset generation 7``() =

    let matcher = Regex(@"(.+\n)+\n").TSetMatcher
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (matcher.ImplicitPattern)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[^\n]", ss1pretty)

[<Fact>]
let ``startset generation 8``() =

    let matcher = Regex(@"([a-ci]*|⊤*i[a-ci]*)c").TSetMatcher
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (matcher.RawPattern)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[^ab]", ss1pretty)

[<Fact>]
let ``startset generation 9``() =

    let matcher = Regex(@"⊤*[a-z]*a[a-z]*⊤*").TSetMatcher
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (matcher.RawPattern)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[^b-z]", ss1pretty)

[<Fact>]
let ``startset generation 10``() =

    let matcher = Regex(@"⊤*[a-z]*a[a-z]*⊤*").TSetMatcher
    let c = matcher.Cache
    let ss1 = Info.Startset.inferStartset (c.Solver) (matcher.ReversePattern)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[^b-z]", ss1pretty)



// [<Fact>]
// let ``first startset generation 1``() =
//
//     let matcher = Regex(@"⊤*have⊤*&⊤*there⊤*&⊤*other⊤*&.*").UInt64Matcher
//     let c = matcher.Cache
//     let ss1 = Info.Startset.inferStartset (c.Solver) (matcher.RawPattern)
//     let ss1pretty = c.PrettyPrintMinterm(ss1)
//     // let ss2pretty = c.PrettyPrintMinterm(ss2)
//     // -- [\nhot]
//     Assert.Equal(@"[\nhot]", ss1pretty)
//     // Assert.Equal(@"[aht]", ss2pretty)




[<Fact>]
let ``reverse startset generation 1``() =
    let matcher = Regex(@"\n\n~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*&⊤*Paris⊤*").TSetMatcher
    let c = matcher.Cache
    let patstr = c.PrettyPrintNode matcher.ReversePattern
    let ss1 = Info.Startset.inferStartset (c.Solver) (matcher.ReversePattern)
    let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.Equal(@"[\nghs]", ss1pretty)





// [<Fact>]
// let ``startset2 generation 2``() =
//
//     let matcher = Matcher(@"(.*a&~(.*b.*))")
//     let c = matcher.Cache
//     let ss1 = Info.Startset.inferStartset2 (c.Solver) (c.InitialPatternWithoutDotstar)
//     let ss1pretty = c.PrettyPrintMinterm(ss1)
//     Assert.Equal("[FH]", ss1pretty)
//
//



//TODO:
// [<Fact>]
// let ``startset2 generation 2`` () =
//
//     let matcher = Matcher(@"⊤*(Finn|Huck)⊤*")
//     let c = matcher.Cache
//     let ss1 = Info.Startset.inferStartset2 (c.Solver) (c.InitialPatternWithoutDotstar)
//     let ss1pretty = c.PrettyPrintMinterm(ss1)
//     Assert.Equal("[in]", ss1pretty)


[<Fact>]
let ``startsetChars of bdd 1``() =
    let matcher = Regex(@"⊤*Finn⊤*&⊤*Huck⊤*").TSetMatcher
    let bdds = matcher.Cache.MintermBdds()
    let startsetChars =
        bdds[1..] |> Array.map (fun v -> StartsetHelpers.bddToStartsetChars (v))

    let charArrays =
        startsetChars |> Array.map (fun v -> v.Chars |> String) |> String.concat ","
    // let ss1 = Info.Startset.inferStartset2 (c.Solver) (c.InitialPatternWithoutDotstar)
    // let ss1pretty = c.PrettyPrintMinterm(ss1)
    Assert.StrictEqual(charArrays, "F,H,c,i,k,n,u" )


[<Fact>]
let ``startsetChars of bdd 2 - merged span``() =
    let matcher = Regex(@"⊤*Finn⊤*&⊤*Huck⊤*").TSetMatcher
    let c = matcher.Cache
    let bdds = c.MintermBdds()

    let startsetsArray = StartsetHelpers.startsetsFromMintermArray(bdds)
    let uintminterms = c.Minterms()

    let ss1 = Info.Startset.inferStartset (c.Solver) (c.InitialPatternWithoutDotstar)
    let ss1pretty = c.PrettyPrintMinterm(ss1)

    let mergedIndexOf =
        StartsetHelpers.getMergedIndexOfSpan(c.Solver,startsetsArray, c.Minterms(),  ss1)

    // let mergedStr =
    //     mergedIndexOf.

    // Assert.Equal("FH",mergedStr)
    ()



[<Fact>]
let ``initialstartset prefix 01``() =
    let matcher = Regex(@"⊤*have⊤*")
    let flags = Common.getRawFlags matcher
    Assert.Equal(Flag.PrefixFlag ||| Flag.CanSkipFlag, flags)



// [<Fact>]
// let ``initialstartset prefix 02``() =
//     let matcher = Regex(@"~(⊤*\n\n⊤*)\n&⊤*have⊤*").UInt64Matcher
//     let info = matcher.RawPattern.TryGetInfo.Value
//     let acc = ResizeArray()
//     let initialStart =
//         Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.RawPattern
//
//     let r = 1
//     ()
//     // Assert.Equal(Flag.Prefix ||| Flag.CanSkip, info.Flags)


[<Fact>]
let ``initialstartset prefix 03``() =
    let matcher = Regex("THE.*LIFE").TSetMatcher

    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.RawPattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        Assert.Equal(arr.Length, 3)
        arr.Length
    | _ -> failwith "invalid result"


[<Fact>]
let ``initialstartset prefix 04``() =
    let matcher = Regex("⊤*have⊤*&⊤*there⊤*&.*").TSetMatcher
    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.RawPattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        Assert.Equal(arr.Length, 4)
        Assert.Equal("[ht]",matcher.Cache.PrettyPrintMinterm(arr.Span[0]))
        Assert.Equal("[ah]",matcher.Cache.PrettyPrintMinterm(arr.Span[1]))
        Assert.Equal("[ev]",matcher.Cache.PrettyPrintMinterm(arr.Span[2]))
        Assert.Equal("[er]",matcher.Cache.PrettyPrintMinterm(arr.Span[3]))
    | _ -> failwith "invalid result"

[<Fact>]
let ``initialstartset prefix 05``() =
    let matcher = Regex("⊤*have⊤*&⊤*there⊤*&.*").TSetMatcher
    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.ReversePattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        Assert.Equal(arr.Length, 4)
        Assert.Equal("e",matcher.Cache.PrettyPrintMinterm(arr.Span[0]))
        Assert.Equal("[rv]",matcher.Cache.PrettyPrintMinterm(arr.Span[1]))
        Assert.Equal("[ae]",matcher.Cache.PrettyPrintMinterm(arr.Span[2]))
        Assert.Equal("h",matcher.Cache.PrettyPrintMinterm(arr.Span[3]))
    | _ -> failwith "invalid result"

[<Fact>]
let ``initialstartset prefix 06``() =
    let matcher = Regex(@"lethargy.*air").TSetMatcher
    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.ReversePattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        Assert.Equal(arr.Length, 3)
        let arr = arr.Span
        Assert.Equal("r",matcher.Cache.PrettyPrintMinterm(arr[0]))
        Assert.Equal("i",matcher.Cache.PrettyPrintMinterm(arr[1]))
        Assert.Equal("a",matcher.Cache.PrettyPrintMinterm(arr[2]))
    | _ -> failwith "invalid result"


[<Fact>]
let ``initialstartset prefix 07``() =
    let matcher = Regex(@".*have.*there.*|.*there.*have.*").TSetMatcher
    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.RawPattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        Assert.Equal(arr.Length, 4)
        let arr = arr.Span
        Assert.Equal("[ht]",matcher.Cache.PrettyPrintMinterm(arr[0]))
        Assert.Equal("[ah]",matcher.Cache.PrettyPrintMinterm(arr[1]))
        Assert.Equal("[ev]",matcher.Cache.PrettyPrintMinterm(arr[2]))
    | _ -> failwith "invalid result"



[<Fact>]
let ``initialstartset prefix 08``() =
    let matcher = Regex(@"((.* t[a-z]*e .*|[a-z]*e .*)&.* a[a-z]*d .*)").TSetMatcher
    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.RawPattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        Assert.Equal(arr.Length, 2)
        let arr = arr.Span
        Assert.Equal("[ e]",matcher.Cache.PrettyPrintMinterm(arr[0]))
        Assert.Equal("[ at]",matcher.Cache.PrettyPrintMinterm(arr[1]))
    | _ -> failwith "invalid result"



[<Fact>]
let ``initialstartset prefix 09``() =
    let matcher = Regex(@"(~(⊤*honor⊤*)&~(⊤*\n\n⊤*)\n)").TSetMatcher
    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.RawPattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        // Assert.Equal(arr.Length, 2)
        let arr = arr.Span
        Assert.Equal(@"[\nh]",matcher.Cache.PrettyPrintMinterm(arr[0]))
        // Assert.Equal("[ at]",matcher.Cache.PrettyPrintMinterm(arr[1]))
    | _ -> failwith "invalid result"

[<Fact>]
let ``initialstartset prefix 10``() =
    let matcher = Regex(@"~(⊤*\n\n⊤*)\n").TSetMatcher
    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.RawPattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        Assert.Equal(arr.Length, 1)
        let arr = arr.Span
        Assert.Equal(@"\n",matcher.Cache.PrettyPrintMinterm(arr[0]))
    | _ -> failwith "invalid result"

[<Fact>]
let ``initialstartset prefix 11``() =
    let matcher = Regex(@"~(⊤*\n\n⊤*)").TSetMatcher
    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.RawPattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        Assert.Equal(arr.Length, 2)
        let arr = arr.Span
        Assert.Equal(@"\n",matcher.Cache.PrettyPrintMinterm(arr[0]))
        Assert.Equal(@"\n",matcher.Cache.PrettyPrintMinterm(arr[1]))
    | _ -> failwith "invalid result"

[<Fact>]
let ``initialstartset prefix 12``() =
    let matcher = Regex(@"~(⊤*\n\n⊤*)&⊤*Huck⊤*").TSetMatcher
    let initialStart =
        Info.Startset.inferInitialStartset matcher.Cache.Solver matcher.RawPattern
    match initialStart with
    | InitialStartset.MintermArrayPrefix(prefix=arr) ->
        Assert.Equal(arr.Length, 4)
        let arr = arr.Span
        Assert.Equal(@"H",matcher.Cache.PrettyPrintMinterm(arr[0]))
        Assert.Equal(@"u",matcher.Cache.PrettyPrintMinterm(arr[1]))
        Assert.Equal(@"c",matcher.Cache.PrettyPrintMinterm(arr[2]))
        Assert.Equal(@"k",matcher.Cache.PrettyPrintMinterm(arr[3]))
    | _ -> failwith "invalid result"


[<Fact>]
let ``skip position test 1``() =
    let matcher = Regex(@"~(⊤*\n\n⊤*)\n&⊤*Twain⊤*").TSetMatcher
    let mutable loc = Location.create "aa Twa Twain asd" 0

    let arr = matcher.Cache.GetInitialStartsetPrefix()
    let result =
        let arr = arr.Span
        let chars = matcher.Cache.MintermStartsetChars(arr[0])
        matcher.Cache.TryNextStartsetLocationArray(&loc, arr,chars)
    Assert.Equal(loc.Position, 7) // aa Twa |Tw





[<Fact>]
let ``skip position test 2``() =
    let matcher = Regex(@"lethargy.*").TSetMatcher
    let text = ("lethargy, and and the air tainted with\nc")
    let mutable loc =
        Location.create text (text.Length - 4)
    loc.Reversed <- true

    let prefix = matcher.Cache.Builder.GetInitializedPrefix(matcher.ReversePattern)
    let result =
        match prefix with
        | InitialStartset.MintermArrayPrefix(prefix=arr; loopTerminator=term) ->
            let arr = arr.Span
            let term = term.Span
            matcher.Cache.TryNextStartsetLocationArrayWithLoopTerminator(&loc, arr,term)
        | _ -> failwith "todo"
    Assert.Equal(ValueSome 8, result)



#endif

#endif