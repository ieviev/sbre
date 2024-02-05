[<Xunit.Collection("Sequential")>]
module Sbre.Test._02_NodeTests

open System.Globalization
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre
open Sbre.Info
open Sbre.Types
open Xunit
open Common
open Sbre.Pat

#if DEBUG

module Helpers =
    let charSetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()

    let bddBuilder = SymbolicRegexBuilder<BDD>(charSetSolver, charSetSolver)

    let converter = RegexNodeConverter(bddBuilder, null)
    let bddBuilder2 = RegexBuilder(converter, charSetSolver, charSetSolver)

let printImplicit(reg: Regex) =
    try
        let matcher = reg.TSetMatcher
        let nodes = matcher.TrueStarredPattern
        matcher.Cache.PrettyPrintNode nodes
    with e ->
        try
            let matcher = reg.UInt16Matcher
            let nodes = matcher.TrueStarredPattern
            matcher.Cache.PrettyPrintNode nodes
        with e ->
            let matcher = reg.TSetMatcher
            let nodes = matcher.TrueStarredPattern
            matcher.Cache.PrettyPrintNode nodes

let printNode(reg: RegexMatcher<_>, node: RegexNode<_>) =
    try
        let matcher = reg
        let nodes = node
        matcher.Cache.PrettyPrintNode nodes
    with e ->
        failwith "failed to print node"


[<Fact>]
let ``collectSets has same behavior``() =
    // default F# sets and HashSets have different behavior!!!

    // dot starred mariomka uri test
    let pattern = """.*?[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?"""

    let tree =
        RegexParser.Parse(
            pattern,
            RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking,
            CultureInfo.InvariantCulture
        )

    let runtimeSets =
        System.Text.RuntimeRegexCopy.Symbolic
            .RegexNodeConverter(Helpers.bddBuilder, null)
            .ConvertToSymbolicRegexNode(tree.Root)
            .GetSets(Helpers.bddBuilder)



    let sbreSets =
        RegexNodeConverter.convertToSymbolicRegexNode (
            Helpers.charSetSolver,
            Helpers.bddBuilder,
            Helpers.bddBuilder2,
            tree.Root
        )
        |> Minterms.collectSets //|> HashSet

    let areEqual = runtimeSets.SetEquals(sbreSets)

    areEqual |> Assert.True


[<Fact>]
let ``pretty printer test 1``() =

    let matcher = Regex("a")
    Assert.Equal("⊤*a", printImplicit matcher)
    ()


[<Fact>]
let ``identity true star``() =

    let matcher = Regex(@"[\s\S]*").TSetMatcher
    let cache = matcher.Cache
    let nodes = matcher.RawPattern

    let identity =
        match nodes with
        | _ when obj.ReferenceEquals(cache.Builder.uniques._trueStar, nodes) -> true
        | _ -> false

    Assert.True(identity)


[<Fact>]
let ``identity true star reversed``() =

    let matcher = Regex(@"[\s\S]*").TSetMatcher
    let cache = matcher.Cache
    let nodes = matcher.ReversePattern

    let identity =
        match nodes with
        | _ when obj.ReferenceEquals(cache.Builder.uniques._trueStar, nodes) -> true
        | _ -> false

    Assert.True(identity)


// [<Fact>]
// let ``set equality``() =
//
//     let matcher = Matcher(@"(⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")
//
//     let matcher2 = Matcher(@"(⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")
//
//     let result = matcher.RawPattern = matcher2.RawPattern
//     let a1 = 1
//     Assert.True(result, "not equal sets: (⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")


// [<Fact>]
// let ``set equality reversed``() =
//
//     let matcher = Matcher(@"(⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")
//
//     let matcher2 = Matcher(@"(⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")
//
//     let result = matcher.ReversePattern = matcher2.ReversePattern
//
//     Assert.True(result, "not equal reverse sets: (⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")



let equalSeq (xs1: seq<'t>) (xs2: seq<'t>) : unit = Assert.Equal<'t>(xs1, xs2)


// [<Fact>]
// let ``conversion of or 1``() =
//
//
//     let regexTree =
//         ExtendedRegexParser.Parse(
//             "(310|0[1-9]2|452)",
//             RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking,
//             CultureInfo.InvariantCulture
//         )
//
//     let symbolicBddnode: RegexNode<BDD> =
//         RegexNodeConverter.convertToSymbolicRegexNode (
//             debugcharSetSolver,
//             bddBuilder,
//             Helpers.bddBuilder2,
//             regexTree.Root
//         )
//
//     match symbolicBddnode with
//     | Types.Or(nodes, _) ->
//         let nodes = nodes |> Seq.map (fun x -> matcher.Cache.PrettyPrintNode  x) |> set
//
//         let expected = [ "0[1-9]2"; "310"; "452" ] |> set
//         let a = 1
//         equalSeq expected nodes
//
//     | _ -> Assert.True(false, "not an or node")
//

let assertConverted (pattern: string) (expected: string) =
    let regexTree =
        ExtendedRegexParser.Parse(
            pattern,
            RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking,
            CultureInfo.InvariantCulture
        )

    let symbolicBddnode: RegexNode<BDD> =
        RegexNodeConverter.convertToSymbolicRegexNode (
            debugcharSetSolver,
            bddBuilder,
            Helpers.bddBuilder2,
            regexTree.Root
        )

    let reg = Regex(pattern)

    let asstr =
        try
            reg.TSetMatcher.Cache.PrettyPrintNode reg.TSetMatcher.RawPattern
        with e ->
            try
                reg.UInt16Matcher.Cache.PrettyPrintNode reg.UInt16Matcher.RawPattern
            with e ->
                reg.UInt16Matcher.Cache.PrettyPrintNode reg.TSetMatcher.RawPattern

    Assert.Equal(expected, asstr)

[<Fact>]
let ``conversion lookaround ``() = assertConverted ".(?<=A.*)" @".(?<=A.*)"
// assertConverted ".(?<=A.*)" @"[^\n](?<=A[^\n]*)"

[<Fact>]
let ``conversion lookaround 2 ``() = assertConverted ".(?=A.*)" @".(?=A.*)"

// assertConverted ".(?=A.*)" @"[^\n](?=A[^\n]*)"


// cannot test this reliably
// [<Fact>]
// let ``conversion large set ``() =
//     assertConverted
//         @"^((0?[13578]a)|(0?[13456789]a))$"
//         @"(?<!⊤)(0?[13578]a|0?[13-9]a)((?!⊤)|(?=\n))"


[<Fact>]
let ``conversion label``() = assertConverted "(?<Time>^\d)" @"(?<!⊤)\d"
// assertConverted ".(?=A.*)" @"[^\n](?=A[^\n]*)"


[<Fact>]
let ``conversion neg lookahead ``() = assertConverted "1(?! Sep)" "1(?! Sep)"

[<Fact>]
let ``conversion conc ``() = assertConverted "Twain" "Twain"



[<Fact>]
let ``flags 1``() =
    let matcher = Regex("(\d⊤*|⊤*\d{2,2}⊤*)").TSetMatcher
    let flags = Flags.inferInitialFlags matcher.RawPattern

    if flags.HasCounter then
        assertEqual (Flag.CanBeNullableFlag ||| Flag.HasCounterFlag) flags


[<Fact>]
let ``flags 2``() =
    let matcher = Regex(@"(.*b|)").TSetMatcher

    match matcher.RawPattern with
    | Types.Or(nodes, info) ->
        // let flags = Flags.inferNode matcher.RawPattern
        // let info = Cache.mkInfoOfOr (matcher.Cache, nodes)
        let flags = Flags.inferInitialFlags matcher.RawPattern

        Assert.Equal(
            Flag.CanBeNullableFlag ||| Flag.IsAlwaysNullableFlag ||| Flag.ContainsEpsilonFlag,
            flags
        )
    | _ -> Assert.True(false, "wrong node type")


[<Fact>]
let ``flags 3``() =
    let matcher = Regex(@"\d{2}⊤*").TSetMatcher
    let flags = matcher.RawPattern.GetFlags()

    if flags.HasCounter then
        assertTrue (flags.HasFlag(Flag.HasCounterFlag)) "hascounter"
        assertTrue (flags.HasFlag(Flag.IsCounterFlag)) "iscounter"


[<Fact>]
let ``flags 4``() =
    let matcher = Regex(@"⊤*\d{2}⊤*").TSetMatcher
    let flags = matcher.RawPattern.GetFlags()

    if flags.HasCounter then
        assertTrue (flags.HasFlag(Flag.HasCounterFlag)) "hascounter"
        assertTrue (flags.HasFlag(Flag.CanBeNullableFlag)) "canbenullable"
        assertFalse (flags.HasFlag(Flag.IsCounterFlag)) "iscounter"


[<Fact>]
let ``flags 5``() =
    let matcher = Regex(@"~(⊤*\d{2}⊤*)").TSetMatcher
    let flags = matcher.RawPattern.GetFlags()

    if flags.HasCounter then
        Assert.True(flags.HasFlag(Flag.HasCounterFlag))
        Assert.True(flags.HasFlag(Flag.CanBeNullableFlag))




[<Fact>]
let ``identity derivative 2``() =
    let m = Regex(@"((⊤*t|)neW⊤*&⊤*erohsa⊤*&⊤*lirpA⊤*&⊤*yadsruhT⊤*)")
    let deriv = der1Node m "test" true
    let req = refEq m.TSetMatcher.RawPattern deriv
    Assert.True(req)


[<Fact>]
let ``identity and 1``() =
    let m = Regex(@"((nglish⊤*|⊤*English⊤*)&~(⊤*\n\n⊤*)\n&⊤*King⊤*&⊤*Paris⊤*)")

    let deriv = der1Node m "English" true

    let req = refEq (m.TSetMatcher.RawPattern) deriv
    Assert.True(req)


[<Fact>]
let ``identity singleton 1``() =
    let m = Regex(@".*b|a")

    // let deriv = der1Node m "aaab" true

    let l1 =
        match m.TSetMatcher.RawPattern with
        | Or(nodes, _) ->
            let conc =
                nodes
                |> Seq.find (
                    function
                    | Concat(_) -> true
                    | _ -> false
                )

            let loop =
                match conc with
                | Concat(regexNode, tail, regexNodeInfo) -> regexNode
                | _ -> failwith "debug"

            loop
        | _ -> failwith "debug"

    let l2 =
        match m.TSetMatcher.ReversePattern with
        | Or(nodes, _) ->
            let conc =
                nodes
                |> Seq.find (
                    function
                    | Concat(_) -> true
                    | _ -> false
                )

            let loop =
                match conc with
                | Concat(regexNode, tail, regexNodeInfo) -> tail
                | _ -> failwith "debug"

            loop
        | _ -> failwith "debug"

    let req = refEq l1 l2
    Assert.True(req)



#if TODO


// [<Fact>]
// let ``same minterms are computed`` () =
//
//     let pattern =
//         """.*?[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?"""
//
//     let tree =
//         RegexParser.Parse(
//             pattern,
//             RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking,
//             CultureInfo.InvariantCulture
//         )
//
// #if DEBUG
//     let runtimeMinterms =
//         System
//             .Text
//             .RuntimeRegexCopy
//             .Symbolic
//             .RegexNodeConverter(Debug.bddBuilder, null)
//             .ConvertToSymbolicRegexNode(tree.Root)
//             .ComputeMinterms(Debug.bddBuilder)
//         |> Array.map Debug.charSetSolver.PrettyPrint
//
//     let sbreMinterms =
//         RegexNodeConverter.convertToSymbolicRegexNode(Helpers.charSetSolver,Helpers.bddBuilder,tree.Root)
//         |> Minterms.compute Debug.bddBuilder
//         |> Array.map Debug.charSetSolver.PrettyPrint
//
//     StructuralComparisons.StructuralEqualityComparer.Equals(runtimeMinterms, sbreMinterms)
//     |> Assert.True
// #else
//     Assert.True true
// #endif

#endif



[<Fact>]
let ``startset 01``() = assertStartset "(ab)" true "a"

[<Fact>]
let ``startset 02``() = assertStartset "(ab)*" true "a"

[<Fact>]
let ``startset 03``() = assertStartset "(ab)*" false "."

[<Fact>]
let ``startset 04``() = assertStartset "~(.*11.*|1.*)" false "."
















#endif
