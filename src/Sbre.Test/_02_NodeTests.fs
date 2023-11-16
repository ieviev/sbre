[<Xunit.Collection("Sequential")>]
module Sbre.Test._02_NodeTests

open System.Globalization
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre
open Sbre.Info
open Sbre.Types
open Xunit


#if DEBUG

module Helpers =
    let charSetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()

    let bddBuilder = SymbolicRegexBuilder<BDD>(charSetSolver, charSetSolver)

    let converter = RegexNodeConverter(bddBuilder, null)
    let bddBuilder2 = RegexBuilder(converter, charSetSolver, charSetSolver)


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
        |> Minterms.collectSets Helpers.bddBuilder //|> HashSet

    let areEqual = runtimeSets.SetEquals(sbreSets)

    areEqual |> Assert.True


[<Fact>]
let ``pretty printer test 1``() =
    let matcher = Regex("a")
    let nodes = matcher.ImplicitPattern
    let pretty = nodes.ToStringHelper()
    Assert.Equal("⊤*a", pretty)
    ()


[<Fact>]
let ``identity true star``() =

    let matcher = Regex(@"[\s\S]*")
    let cache = matcher.Cache
    let nodes = matcher.RawPattern

    let identity =
        match nodes with
        | _ when obj.ReferenceEquals(cache.Builder.uniques._trueStar,nodes) -> true
        | _ -> false

    Assert.True(identity)


[<Fact>]
let ``identity true star reversed``() =

    let matcher = Regex(@"[\s\S]*")
    let cache = matcher.Cache
    let nodes = matcher.ReversePattern

    let identity =
        match nodes with
        | _ when obj.ReferenceEquals(cache.Builder.uniques._trueStar,nodes) -> true
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


[<Fact>]
let ``set equality 2``() =
    let matcher = Regex(@"(⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")

    let matcher2 = Regex(@"(⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")

    let c = matcher.Cache

    Assert.True(true)
// let result =
//     c.CheckEquality(matcher.RawPattern, matcher2.RawPattern)
//
// Assert.True(result, "not equal sets: (⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")


[<Fact>]
let ``nullable not eps``() =
    let matcher = Regex(@"(⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")

    let matcher2 = Regex(@"(⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")

    let c = matcher.Cache
    Assert.True(true)
// let result =
//     c.CheckEquality(matcher.RawPattern, matcher2.RawPattern)
//
// Assert.True(result, "not equal sets: (⊤*Arabs&⊤*French⊤*&⊤*Chinese⊤*)")




let equalSeq (xs1: seq<'t>) (xs2: seq<'t>) : unit = Assert.Equal<'t>(xs1, xs2)


[<Fact>]
let ``conversion of or 1``() =


    let regexTree =
        ExtendedRegexParser.Parse(
            "(310|0[1-9]2|452)",
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

    match symbolicBddnode with
    | Types.Or(nodes, _) ->
        let nodes = nodes |> Seq.map (fun x -> x.ToStringHelper()) |> set

        let expected = [ "0[1-9]2"; "310"; "452" ] |> set
        let a = 1
        equalSeq expected nodes

    | _ -> Assert.True(false, "not an or node")


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

    let minterms = symbolicBddnode |> Minterms.compute bddBuilder

    let solver = UInt64Solver(minterms, Helpers.charSetSolver)

    debuggerSolver <- Some solver

    let asstr = symbolicBddnode.ToStringHelper()
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




// [<Fact>]
// let ``conversion 2`` () =
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
//         RegexNodeConverter.convertToSymbolicRegexNode (debugcharSetSolver, bddBuilder, regexTree.Root)
//
//     match symbolicBddnode with
//     | Types.Or(nodes, _) ->
//         let nodes =
//             nodes
//             |> Seq.map (fun x -> x.ToStringHelper())
//             |> set
//
//         let expected =
//             ["0[1-9]2"; "310"; "452"]
//             |> set
//         let a = 1
//         equalSeq expected nodes
//
//     | _ -> Assert.True(false, "not an or node")



[<Fact>]
let ``conversion of or 2``() =

    let matcher = Regex("(310|0[1-9]2|452)")

    match matcher.RawPattern with
    | Types.Or(nodes, _) ->
        let nodes = nodes |> Seq.map (fun x -> x.ToStringHelper()) |> set

        let expected = [ "310"; "0[1-9]2"; "452" ] |> set
        let a = 1
        equalSeq expected nodes

    | _ -> Assert.True(false, "not an or node")


[<Fact>]
let ``flags 1``() =
    let matcher = Regex("(\d⊤*|⊤*\d{2,2}⊤*)")

    match matcher.RawPattern with
    | Types.Or(nodes, info) ->
        let flags = Flags.inferNode matcher.RawPattern
        // let info = Cache.mkInfoOfOr (matcher.Cache, nodes)
        Assert.Equal(Flag.None, flags)
    | _ -> Assert.True(false, "wrong node type")


[<Fact>]
let ``flags 2``() =
    let matcher = Regex(@"⊤*English⊤*&⊤*King⊤*&⊤*Paris⊤*&~(⊤*\n\n⊤*)\n")

    match matcher.RawPattern with
    | Types.And(nodes, info) ->
        // let flags = Flags.inferNode matcher.RawPattern
        let flags = Info.Flags.inferAnd (nodes)
        Assert.Equal(Flag.CanSkip ||| Flag.Prefix, flags)
    | _ -> Assert.True(false, "wrong node type")


[<Fact>]
let ``flags 3``() =
    let matcher = Regex(@"(.*b|)")

    match matcher.RawPattern with
    | Types.Or(nodes, info) ->
        // let flags = Flags.inferNode matcher.RawPattern
        // let info = Cache.mkInfoOfOr (matcher.Cache, nodes)
        let flags = Flags.inferNode matcher.RawPattern
        Assert.Equal(Flag.CanBeNullable ||| Flag.IsAlwaysNullable ||| Flag.ContainsEpsilon, flags)
    | _ -> Assert.True(false, "wrong node type")



[<Fact>]
let ``flags 4``() =
    let matcher = Regex(@"~(⊤*Ara⊤*)")

    match matcher.RawPattern with
    | Types.Not(nodes, info) ->
        Assert.Equal(Flag.CanBeNullable ||| Flag.IsAlwaysNullable ||| Flag.CanSkip ||| Flag.Prefix, info.Flags)
    | _ -> Assert.True(false, "wrong node type")




[<Fact>]
let ``flags prefix 1``() =
    let matcher = Regex(@"have⊤*")
    let info = matcher.RawPattern.TryGetInfo.Value
    Assert.Equal(Flag.Prefix, info.Flags)


[<Fact>]
let ``flags prefix 2``() =
    let matcher = Regex(@"⊤*have⊤*")
    let info = matcher.RawPattern.TryGetInfo.Value
    Assert.Equal(Flag.Prefix ||| Flag.CanSkip, info.Flags)


[<Fact>]
let ``flags prefix 3``() =
    let matcher = Regex(@"~(⊤*\n\n⊤*)\n&⊤*have⊤*")
    let info = matcher.RawPattern.TryGetInfo.Value
    Assert.Equal(Flag.Prefix ||| Flag.CanSkip, info.Flags)



[<Fact>]
let ``reverse unwrap``() =
    let matcher = Regex("⊤*have⊤*")
    match matcher.ReversePattern with
    | Concat(Loop(_),t,_) -> Assert.True(true)
    | _ -> failwith "wrong result"





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
