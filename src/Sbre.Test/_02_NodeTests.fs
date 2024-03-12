[<Xunit.Collection("Sequential")>]
module Sbre.Test._02_NodeTests

open System.Globalization
open System.IO
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
    let bddBuilder2 = RegexBuilder(converter, charSetSolver, charSetSolver, Sbre.SbreOptions())



let printNode(reg: RegexMatcher<_>, node: RegexNode<_>) =
    try
        let matcher = reg
        let nodes = node
        matcher.PrettyPrintNode nodes
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
            Helpers.bddBuilder2,
            tree.Root
        )
        |> Minterms.collectSets //|> HashSet

    let areEqual = runtimeSets.SetEquals(sbreSets)

    areEqual |> Assert.True


let assertSolverContains12 pattern =
    let pat = pattern
    let reg = Regex(pat)
    match reg.TSetMatcher.RawPattern with
    | Concat(head=Singleton head;tail=Singleton tail) ->
        assertTrue (Solver.containsS reg.TSetMatcher.Cache.Solver head tail)
    | _ -> failwith "_"

let assertNotSolverContains12 pattern =
    let pat = pattern
    let reg = Regex(pat)
    match reg.TSetMatcher.RawPattern with
    | Concat(head=Singleton head;tail=Singleton tail) ->
        assertFalse (Solver.containsS reg.TSetMatcher.Cache.Solver head tail)
    | _ -> failwith "_"




[<Fact>]
let ``_ solver 1``() = assertSolverContains12 "[a-z]a"

[<Fact>]
let ``_ solver 2``() = assertNotSolverContains12 "a[a-z]"


[<Fact>]
let ``a feature 1.1``() = assertConverted """\b\d{5}(?:-\d{4})?\b""" [
    @"((?<=φ)|\A)φ{5,5}(-φ{4,4})?((?=φ)|\z)"
    @"(\A|(?<=φ))φ{5,5}(-φ{4,4})?(\z|(?=φ))"
    @"(\A|(?<=φ))φ{5,5}(-φ{4,4})?((?=φ)|\z)"

    @"(?<=(φ|\A))φ{5,5}(-φ{4,4})?(?=(φ|\z))"
    @"(?<=(\A|φ))φ{5,5}(-φ{4,4})?(?=(φ|\z))"
    @"(?<=(φ|\A))φ{5,5}(-φ{4,4})?(?=(\z|φ))"
    @"(?<=(\A|φ))φ{5,5}(-φ{4,4})?(?=(\z|φ))"
]




[<Fact>]
let ``a conversion 2.1``() = assertConverted "(⊤*B⊤*&⊤*A⊤*)" [ "(⊤*A⊤*&⊤*B⊤*)"; "(⊤*B⊤*&⊤*A⊤*)" ]


[<Fact>]
let ``a conversion 2.2``() = assertConverted "(⊤*A⊤*&⊤*B⊤*)&⊤*B⊤*" [
    @"(⊤*B⊤*&⊤*A⊤*)"
    "(⊤*A⊤*&⊤*B⊤*)"
]




[<Fact>]
let ``a conversion 2.6``() = assertConverted """([a-zA-Z]+)Huck|([a-zA-Z]+)Saw""" [
    """([A-Za-z])+(Huck|Saw)"""
    """([A-Za-z])+(Saw|Huck)"""
]



// [<Fact>]
// let ``b conversion 2.2``() = assertConverted """((⊤*,}.*|.*)&(⊤*X|))|(⊤*,}.*&(⊤*X|))""" [
//     """((⊤*X|ε)&(.*|⊤*,}.*))"""
//     """((ε|⊤*X)&(.*|⊤*,}.*))"""
//     """((⊤*,}.*|.*)&(ε|⊤*X))"""
// ]



// """.*(?=aaa)"""

// (.*|(.*11.*|1.*)


// [<Fact>]
// let ``subsumption and loop ``() =
//     testPartDerivative (@"(.*&.*s)", "aaa", @".*s")
//
//
// [<Fact>]
// let ``subsumption and larger ``() =
//     testPartDerivatives (@"(.* and .*|and .*)&.*", "aaa", [@"(.* and .*|nd .*)";"(nd .*|.* and .*)"])
//
//

// [<Fact>]
// let ``a conversion 1.3``() = assertConverted @".(?<=a)" [ "a" ]
//
//



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


[<Fact>]
let ``conversion lookaround 2 ``() = assertConverted ".(?=A.*)" [@".(?=A.*)"]

[<Fact>]
let ``conversion label``() = assertConverted "(?<Time>^\d)" [
    @"^\d"
    @"^φ"
    @"(?<=(\n|\A))φ"
]




[<Fact>]
let ``conversion conc ``() = assertConverted "Twain" ["Twain"]


[<Fact>]
let ``flags 01``() =
    let matcher = Regex(@"^\d$").TSetMatcher
    let f = matcher.ReverseTrueStarredPattern.GetFlags()
    assertFlag f Flag.DependsOnAnchorFlag

[<Fact>]
let ``flags 02``() =
    let matcher = Regex("""(?<=\W)\w+nn(?=\W)""").TSetMatcher
    let f = matcher.ReverseTrueStarredPattern.GetFlags()
    assertNotFlag f Flag.DependsOnAnchorFlag


[<Fact>]
let ``flags 03``() =
    let matcher = Regex("""(?<=.?)""").TSetMatcher
    let f = matcher.ReverseTrueStarredPattern.GetFlags()
    assertFlag f Flag.CanBeNullableFlag
    assertFlag f Flag.IsAlwaysNullableFlag



[<Fact>]
let ``flags 05``() =
    let matcher = Regex(@"~(⊤*\d{2}⊤*)").TSetMatcher
    let flags = matcher.RawPattern.GetFlags()
    ()
    // if flags.HasCounter then
    //     Assert.True(flags.HasFlag(Flag.HasCounterFlag))
    //     Assert.True(flags.HasFlag(Flag.CanBeNullableFlag))


[<Fact>]
let ``flags 06``() =
    let matcher = Regex(@"~(\z)").TSetMatcher
    let flags = matcher.RawPattern.GetFlags()
    Assert.False(flags.HasFlag(Flag.IsAlwaysNullableFlag))

[<Fact>]
let ``flags 07``() =
    let matcher = Regex(@"a(?=b)").TSetMatcher
    let f = matcher.RawPattern.GetFlags()
    assertFlag f Flag.HasSuffixLookaheadFlag

[<Fact>]
let ``flags 08``() =
    let matcher = Regex(@"(a|b)(?=b)").TSetMatcher
    let f = matcher.RawPattern.GetFlags()
    assertFlag f Flag.HasSuffixLookaheadFlag

[<Fact>]
let ``flags 09``() =
    let matcher = Regex(@"(?<=b)(a|b)").TSetMatcher
    let f = matcher.RawPattern.GetFlags()
    assertFlag f Flag.HasPrefixLookbehindFlag


[<Fact>]
let ``flags 10``() =
    let matcher = Regex(""".*$""").TSetMatcher
    let f = matcher.RawPattern.GetFlags()
    assertFlag f Flag.HasSuffixLookaheadFlag


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
let ``identity rev 1``() =
    let m = Regex(@"⊤*Huck⊤*")

    let deriv = der1Rev m "g"

    let req = refEq (m.TSetMatcher.ReversePattern) deriv
    Assert.True(req)


// [<Fact>]
// let ``identity word border``() =
//     let m = Regex(@"\b")
//
//     let req = refEq (m.TSetMatcher.ReversePattern) m.TSetMatcher.RawPattern
//     Assert.True(req)




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



let assertNodeWithoutPrefix (patt:string) (expected:string list) =
    let m = Sbre.Regex(patt).TSetMatcher
    let n = m.RawPattern
    let n2 = Optimizations.mkNodeWithoutLookbackPrefix m.Cache.Builder n
    assertContains expected (n2.ToString())

let assertCanBuild (patt:string) (expected:string list) =
    let m = Sbre.Regex(patt, SbreOptions(CanonicalizeStates=true, MinimizeOr=true))
    // let n = m.RawPattern
    ()

[<Fact>]
let ``withoutprefix 01``() =
    assertNodeWithoutPrefix "(?<=author).*&.*and.*" [ "(.*and.*&.*)" ;".*and.*"; "(.*&.*and.*)"]

[<Fact>]
let ``withoutprefix 02``() =
    assertNodeWithoutPrefix @"\b11" [
        "11"
    ]


[<Fact>]
let ``withoutprefix 03``() =
    assertNodeWithoutPrefix """(?<=aaa).*""" [".*"]

// let reg2 =
//     @"(?<=6|8\(.*).*&(?<=6|8\(|4|8|0\().*&~(.*\)\:.*)&\w.*&.*\w&.*(?=.*\)\:)&.*(?=\)\:|\)\:)"


// [<Fact>]
// let ``withoutprefix 04``() =
//     assertNodeWithoutPrefix reg2 [".*"]


[<Fact>]
let ``very large pat 01``() =
    assertCanBuild
        (File.ReadAllText "/home/ian/f/ieviev/sbre/src/Sbre.Test/data/pattern-date.txt") [".*"]




#endif
