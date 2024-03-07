[<Xunit.Collection("Sequential")>]
module Sbre.Test._03_SubsumptionTests


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
    let loc = Location.create "" 0
    let node = matcher.RawPattern
    let nullable = matcher.IsNullable(&loc,node)
    assertFalse nullable


[<Fact>]
let ``a conversion 1.1``() = assertConverted "1(?! Sep)" [ @"1(?=~( Sep⊤*)\z)" ]

[<Fact>]
let ``a conversion 1.2``() = assertConverted @".(?<=a)" [ "a" ]

[<Fact>]
let ``a conversion 1.3``() = assertConverted @"(a*|.*)" [ ".*" ]

[<Fact>]
let ``a conversion 1.4``() = assertConverted "(.*&.*s)" [ ".*s" ]

[<Fact>]
let ``a conversion 1.5``() = assertConverted "[a-z]&.*a" [ "a" ]

[<Fact>]
let ``a conversion 1.6``() = assertConverted ".*b&a" [ "⊥" ]

[<Fact>]
let ``a conversion 1.7``() = assertConverted "a&b.*" [ "⊥" ]


// [<Fact>]
// let ``a conversion 2.4``() = assertConverted "(~((|.*Finn))&.*)" [
//     @"~((.*Finn)?)"
// ]

// [<Fact>]
// let ``deriv negation end ``() =
//     assertRawDerivative @"(.*&~((n|.*Finn)))" "nn" [
//         @"~((.*Finn)?)"
//         "(~((ε|.*Finn))&.*)"
//         "(~((.*Finn|ε))&.*)"
//         "(.*&~((ε|.*Finn)))"
//         "(.*&~((.*Finn|ε)))"
//
//         @"~((ε|.*Finn))"
//         @"~((.*Finn|ε))"
//         @"(.*&~((.*Finn)?))"
//     ]

[<Fact>]
let ``derivative neg lookaround 2``() =
    assertRawDerivative "((?<=.*).*&~(.*A.*))" "A" [
        @"⊥"
        // "(~(.*)&(.*|(?<=.*).*))"
        // @"(~(.*)&((?<=.*).*|.*))"
        // @"((.*|(?<=.*).*)&~(.*))"
        // @"(((?<=.*).*|.*)&~(.*))"
        @"(~((.*A.*|.*))&.*)"
        @"(~((.*|.*A.*))&.*)"
        @"(.*&~((.*|.*A.*)))"
        @"(.*&~((.*A.*|.*)))"
        // --
        @"(.*&~((.*A)?.*))"
        @"(~((.*A)?.*)&.*)"
    ]



[<Fact>]
let ``subsumption and larger ``() =
    assertRawDerivative @"(.* and .*|and .*)&.*" "aaa" [
        @"(.* and .*|nd .*)"
        "(nd .*|.* and .*)"
        @"(.* a)?nd .*"

        @"((ε|.* a)nd .*&.*)" // subsumed
        @"(.*&(.* a|ε)nd .*)"
        @"(.*&(ε|.* a)nd .*)"
        @"((.* a|ε)nd .*&.*)"
    ]


[<Fact>]
let ``subsumption and loop ``() =
    assertRawDerivative @"(.*&.*s)" "aaa" [@".*s"]


[<Fact>]
let ``subsumption or loop ``() =
    assertRawDerivative @"(a*|.*)" "aaa" [@".*"]



[<Fact>]
let ``a conversion 2.5``() =
    assertConverted
        """Huck[a-zA-Z]+|Saw[a-zA-Z]+""" [
    @"(Saw([A-Za-z])+|Huck([A-Za-z])+)"
    @"(Huck([A-Za-z])+|Saw([A-Za-z])+)"
    // this optimization is probably detrimental
    """(Huck|Saw)([A-Za-z])+"""
    """(Saw|Huck)([A-Za-z])+"""
]


[<Fact>]
let ``b conversion 1 ``() =
    assertRawDerivative @".*t.*hat.*" "ttt" [
        @".*(t.*)?hat.*"
        @".*(hat.*|t.*hat.*)"
        @".*(t.*hat.*|hat.*)"
        @".*hat.*" // <- this is nontrivial to infer
    ]


[<Fact>]
let ``b conversion 2.1 ``() =
    assertTSDerivative @"^a*b" "a" [
        @"(⊤*^)?a*b"
        @"(a*b|⊤*^a*b)"
        @"(⊤*^a*b|a*b)"

        @"a*b|⊤*(?<=(\n|\A))a*b)"
        @"(a*b|⊤*(?<=(\A|\n))a*b)"
    ]


[<Fact>]
let ``a conversion 1.8``() = assertConverted "(\d){2,2}⊤*&\d⊤*" [ "(\d){2,2}⊤*" ]

[<Fact>]
let ``a canonical 1.1``() =
    assertConverted "a(|b)|[abc]b?" [
        "[a-c]b?"
        @"[a-c](b|ε)"
        @"[a-c](ε|b)"
    ]

[<Fact>]
let ``a conversion 2.3``() = assertConverted "(.*|(.*11.*|1.*))" [ ".*" ]


[<Fact>]
let ``rewrite suffix 1``() = assertConverted """.*(?=.*def)&.*def""" [ ".*def(?=.*def)" ]


[<Fact>]
let ``rewrite prefix 1``() = assertConverted """(?<=abc).*&.*def""" [ "(?<=abc).*def" ]


[<Fact>]
let ``merge 1``() = assertConverted """a|s""" [ "[as]" ]


[<Fact>]
let ``merge 2``() = assertConverted """at|st""" [ "[as]t" ]



#endif

