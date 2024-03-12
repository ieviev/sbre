[<Xunit.Collection("Sequential")>]
module Sbre.Test._11_LookaroundTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Types
open Xunit
open Common

#if DEBUG

[<Fact>]
let ``a pos simple 1.1``() = assertNullablePositions "(?=b)" "b" [ 0 ]

[<Fact>]
let ``a pos simple 1.2``() = assertNullablePositions "a(?=b)" "_ab_ab_" [ 4; 1 ]

[<Fact>]
let ``a pos simple 1.3``() = assertNullablePositions "a(?=b_)" "_ab_ab_" [ 4; 1 ]

[<Fact>]
let ``a pos simple 1.4``() = assertNullablePositions ".*(?=b_)" "_ab_ab_" (List.rev [ 0 .. 5 ])

[<Fact>]
let ``a pos simple 1.6``() = assertNullablePositions "(ab)+(?=_)" "_ab_ab_" [4;1]

[<Fact>]
let ``a pos simple 1.7``() = assertNullablePositions "bb(?=aa)" "__bbaa__" [ 2 ]


[<Fact>]
let ``a pos simple 2.1a``() = assertMatchEnd "(?=a)" "a" 0 0

[<Fact>]
let ``a pos simple 2.1b``() = assertMatchEnd "a(?=a)" "aa" 0 1

[<Fact>]
let ``a pos simple 2.1c``() = assertMatchEnd "bb(?=a)" "__bbaaa__" 2 4

[<Fact>]
let ``a pos simple 2.2``() = assertMatchEnd "bb(?=aa)" "__bbaa__" 2 4

[<Fact>]
let ``a pos simple 2.3``() = assertMatchEnd "bb(?=aaa)" "__bbaaa__" 2 4

[<Fact>]
let ``a pos simple 2.4``() = assertMatchEnd "bb(?=aaaa)" "__bbaaaa__" 2 4

[<Fact>]
let ``a pos simple 2.5``() = assertMatchEnd "bb(?=aaaaa)" "__bbaaaaa__" 2 4


[<Fact>]
let ``a pos simple 3.1``() = assertAllLLmatchTexts "a(?=b)" "_ab_ab_" [ "a"; "a" ]

[<Fact>]
let ``a pos simple 3.2``() = assertAllLLmatchTexts "bb(?=aa)" "__bbaa__" [ "bb" ]



/// aaa(?<=bbb)
/// (?<=aaa)aaa(?=ccc)
/// aaa(?=c)fdfd
/// aaa(\z|c⊤*&fdfd)



// let idx,len = getFirstLLmatch @".*(?=.*-)&\S.*\S" @"-aaaa-"



[<Fact>]
let ``a pos complex 1.2`` () =
    assertMatchEnd @".*(?=.*-)&.*" @"a-" 0 1


[<Fact>]
let ``a pos complex 1.3`` () =
    assertMatchEnd @".*(?=.*-)&.*" @"-aaaa-" 0 5

[<Fact>]
let ``a pos complex 1.4`` () =
    assertMatchEnd @".*(?=.*-)&\S.*\S" @"-aaaa-" 0 5

[<Fact>]
let ``a pos complex 2.1a1`` () =
    assertFirstMatchText @".*(?=.*bbb)(?=.*ccc)" @"aaa bbb ccc" "aaa "

[<Fact>]
let ``a pos complex 2.1a2`` () =
    assertMatchEndNoLookback @".*(?=.*bbb)(?=.*ccc)" @"aaa bbb ccc" 0 4


[<Fact>]
let ``a pos complex 2.1b`` () =
    assertFirstMatchText @"a+\b(?=.*---)" @"aaa ---" "aaa"

[<Fact>]
let ``a pos complex 2.1c`` () =
    assertFirstMatchText @"a+(?=\W)(?=.*___)" @"aaa ___" "aaa"

[<Fact>]
let ``a pos complex 2.1d`` () =
    assertFirstMatchText @".*(?=\W)(?=.*___)" @"aaa ___" "aaa"

[<Fact>]
let ``a pos complex 2.2a`` () =
    assertFirstMatchText @"\d+(?=[aA]\.?[mM]\.?)" @"10am" "10"

[<Fact>]
let ``a pos complex 2.2b`` () =
    assertFirstMatchText @"\d+(?=[aApP]\.?[mM]\.?)" @"10pm" "10"

[<Fact>]
let ``a pos complex 2.2c`` () =
    assertFirstMatchText @"\d+(?=\s*[aApP]\.?[mM]\.?)" @"10 pm" "10"
    // printAllDerivatives @"\d+(?=\s*[aApP]\.?[mM]\.?)" @"10 pm" [
    //     ["((?<=.?[APap]φ*)(\d)+|⊤*(?<=.?[Mm].?[APap]φ*)(\d)+)"]
    //     ["((?<=φ*)(\d)+|⊤*(?<=.?[Mm].?[APap]φ*)(\d)+)"]
    //     []
    //     []
    // ]

[<Fact>]
let ``a pos complex 2.2d`` () =
    assertFirstMatchText @"\d+(?=\s*[aApP]\.?[mM]\.?)" @"10         pm" "10"




[<Fact>]
let ``b pos simple 1.1``() = assertNullablePositions "(?<=b)" "b" [ 1 ]

[<Fact>]
let ``b pos simple 1.2``() = assertNullablePositions "(?<=bb)" "bb" [ 2 ]

[<Fact>]
let ``b pos simple 1.3``() = assertNullablePositions "(?<=bbb)" "bbb" [ 3 ]

[<Fact>]
let ``b pos simple 1.4``() = assertNullablePositions "(?<=bbbb)" "bbbb" [ 4 ]

[<Fact>]
let ``b pos simple 1.5``() = assertNullablePositions "(?<=b)b" "bb" [ 1 ]

[<Fact>]
let ``b pos simple 1.6``() = assertNullablePositions "(?<=bb)b" "bbb" [ 2 ]

[<Fact>]
let ``b pos simple 1.7a``() = assertNullablePositions "(?<=b)a" "ba" [ 1 ]

[<Fact>]
let ``b pos simple 1.7b``() = assertNullablePositions "(?<=b)a" "bba" [ 2 ]

[<Fact>]
let ``b pos simple 1.7c``() = assertNullablePositions "(?<=b)a" "bbbba" [ 4 ]


[<Fact>]
let ``b pos simple 1.8``() = assertNullablePositions "(?<=bb)a" "bbbba" [ 4 ]

[<Fact>]
let ``b pos simple 1.9``() = assertNullablePositions "(?<=bbb)a" "bbbba" [ 4 ]


[<Fact>]
let ``b pos complex 1.1a``() =
    assertNullablePositions """(?<=aaa).*""" "aaabbb" [3]

[<Fact>]
let ``b pos complex 1.1b``() =
    assertMatchEndNoLookback """(?<=aaa).*""" "aaabbb" 3 6


[<Fact>]
let ``b pos complex 1.2``() =
    assertFirstMatchText """(?<=aaa).*""" "aaabbb" "bbb"


[<Fact>]
let ``b pos complex 2.1``() =
    assertFirstMatchText """(?<=aaa\{).*(?=\})""" "aaa{bbb {ccc}}" "bbb {ccc}"

[<Fact>]
let ``b pos complex 2.2a``() =
    assertFirstMatchText """(?<=\w)(?<=a)b""" "ab" "b"

[<Fact>]
let ``b pos complex 2.2b``() =
    assertFirstMatchText """(?<=\w\w)(?<=aa)b""" "aab" "b"

[<Fact>]
let ``b pos complex 2.2c``() =
    assertFirstMatchText """(?<=\w\w)(?<=aa)bb""" "aabb" "bb"

[<Fact>]
let ``b pos complex 2.2d``() =
    assertFirstMatchText """(?<=\w)(?<=aa)b""" "aab" "b"

[<Fact>]
let ``b pos complex 2.2e``() =
    assertFirstMatchText """(?<=a.*)\bb""" "a  b" "b"



[<Fact>]
let ``c intersect 1.1``() =
    assertFirstMatchText """(?<=author).*""" "author: abc and def" ": abc and def"

[<Fact>]
let ``c intersect 1.2a``() =
    assertNullablePositions """(?<=author).*&.*""" "author: abc and def" [6]

[<Fact>]
let ``c intersect 1.2b``() =
    assertMatchEndNoLookback """(?<=author).*&.*and.*""" "author: abc and def" 6 19

[<Fact>]
let ``c intersect 1.2c``() =
    assertFirstMatchText """(?<=author).*&.*""" "author: abc and def" ": abc and def"

[<Fact>]
let ``c intersect 1.3a``() =
    assertFirstMatchText """(?<=__).*&~(.*and.*)""" "__ abc and def" " abc an"

[<Fact>]
let ``c intersect 1.3b``() =
    assertFirstMatchText """(?<=__).*(?=.*def)&.*and.*""" "__abc and def" "abc and "

// TODO: maybe not support this at all?
// [<Fact>]
// let ``c intersect 1.4a``() =
//     assertFirstMatchText """.*(?=.*def)&~(.*and.*)""" "__abc and def" "__abc an"

// this works
[<Fact>]
let ``c intersect 1.4b``() =
    assertFirstMatchText """~(.*and.*)(?=.*def)""" "__abc and def" "__abc an"


[<Fact>]
let ``d compl 1.1a``() =
    assertNullablePositions """~(.*and.*)""" "__A and B" [ yield! List.rev [ 0..9 ] ]
[<Fact>]
let ``d compl 1.1b``() =
    assertMatchEnd """~(.*and.*)""" "__A and B" 0 6

[<Fact>]
let ``d compl 1.1c``() =
    assertMatchEnd """~(.*and.*)""" "__A and B" 6 9

// [<Fact>]
// let ``d compl 1.1d``() =
//     assertNullablePositions """.*(?=.*B)&~(.*A.*)""" "_A_B" [
//         yield! List.rev [ 0..3 ]
//     ]
//
// [<Fact>]
// let ``d compl 1.1e``() =
//     assertNullablePositions """.*(?=.*B)&~(.*and.*)""" "__A and B" [
//         yield! List.rev [ 0..8 ]
//     ]

[<Fact>]
let ``e range constraint 1.1a``() = assertNullablePositions @".(?=.*c)" "abc" [1; 0]

[<Fact>]
let ``e range constraint 1.1b``() = assertNullablePositions @"(?<=a.*).(?=.*c)" "abc" [1]


[<Fact>]
let ``e range constraint 1.2a``() = assertMatchEndNoLookback @".(?=.*c)" "bc" 0 1

[<Fact>]
let ``e range constraint 1.2b``() = assertMatchEndNoLookback @"(?<=a.*).(?=.*c)" "abc" 1 2

[<Fact>]
let ``e range constraint 1.3a``() = assertNullablePositions @"(?<=a.*).(?=.*c)" "a__c" [ 2; 1]

[<Fact>]
let ``e range constraint 1.3b``() =
    assertMatchEndNoLookback @"(?<=a.*).(?=.*c)" "a__c" 1 2

[<Fact>]
let ``e range constraint 1.3c``() =
    assertMatchEndNoLookback @"(?<=a.*).(?=.*c)" "a__c" 2 3

[<Fact>]
let ``f wordborder constraint 1.1a``() =
    assertNullablePositions @"(?<=a.*)(x)(?=.*c)" "a x c" [2]

[<Fact>]
let ``f wordborder constraint 1.1b``() =
    assertNullablePositions @"(\bx)" "a x c" [2]

[<Fact>]
let ``f wordborder constraint 1.1c``() =
    assertNullablePositions @"(?<=a.*)(\bx)(?=.*c)" "a x c" [2]

[<Fact>]
let ``f wordborder constraint 1.1c -``() =
    assertNullablePositions @"x\b(?=.*c)" "a x c" [2]


[<Fact>]
let ``f wordborder constraint 1.1d``() =
    assertNullablePositions @"(?<=a.*)(\bx\b)(?=.*c)" "a x c" [2]

[<Fact>]
let ``f wordborder constraint 1.1e``() =
    assertNullablePositions @"(?<=a.*)(\bx\b)(?=.*c)" "a xx c" []


[<Fact>]
let ``f wordborder constraint 1.2a``() =
    assertNullablePositions @"(?<=\W)hello(?=\W)" " hello " [ 1 ]

[<Fact>]
let ``f wordborder constraint 1.2b``() =
    assertNullablePositions @"(?<=\W)hello(?=\W)" " helloworld " [ ]

let bibtexEntry =
    @"@article{de2000thyroid,
  title={Thyroid cancer in French Polynesia between 1985 and 1995: influence of atmospheric nuclear bomb tests performed at Mururoa and Fangataufa between 1966 and 1974},
  author={De Vathaire, Florent and Le Vu, B{\'e}atrice and Challeton-de Vathaire, C{\'e}cile},
  journal={Cancer Causes \& Control},
  volume={11},
  number={1},
  pages={59--63},
  year={2000},
  publisher={Springer}
}"


// [<Fact>]
// let ``g bibtex extraction 1.1``() =
//     assertAllLLmatchTexts @"(?<=or=\{.*)\b(~(.*and.*)&\S[\w-{}\\' ,]+\S)\b(?=.*\},)" bibtexEntry [
//         "De Vathaire, Florent"; "Le Vu, B{\\'e}atrice"; "Challeton-de Vathaire, C{\\'e}cile"
//     ]
//
// [<Fact>]
// let ``g bibtex extraction 1.2``() =
//     assertAllLLmatchTexts @"(?<=or=(\{|.*\W))(~(.*and.*)&\S[\w-{}\\' ,]+\S)\b(?=.*\},)" bibtexEntry [
//         "De Vathaire, Florent"; "Le Vu, B{\\'e}atrice"; "Challeton-de Vathaire, C{\\'e}cile"
//     ]

[<Fact>]
let ``g bibtex extraction 1.3``() =
    assertAllLLmatchTexts @"(?<=or=(\{|.*\W))(~(.*and.*)&\S[\w-{}\\' ,]+\w)(?=(\W.*|)\},)" bibtexEntry [
        "De Vathaire, Florent"; "Le Vu, B{\\'e}atrice"; "Challeton-de Vathaire, C{\\'e}cile"
    ]

[<Fact>]
let ``g bibtex extraction 1.4``() =
    assertAllLLmatchTexts @"(?<=or=\{.*)(?<=\W)(~(.*and.*)&[A-Z][\w-{}\\' ,]+)(?=.*\},)(?=\W)" bibtexEntry [
        "De Vathaire, Florent"; "Le Vu, B{\\'e}atrice"; "Challeton-de Vathaire, C{\\'e}cile"
    ]


[<Fact>]
let ``g bibtex extraction 1.5``() =
    assertAllLLmatchTexts
        (String.concat "&" [
           """~(.*and.*)"""
           """[A-Z][\w-{}\\' ,]+"""
           @"(?<=or=\{.*).*"
           @"(?<=\W).*"
           @".*(?=.*\},)"
           @".*(?=\W)"
        ])
        bibtexEntry
        [
            "De Vathaire, Florent"; "Le Vu, B{\\'e}atrice"; "Challeton-de Vathaire, C{\\'e}cile"
        ]


[<Fact>]
let ``g bibtex extraction 1.6 unmatchable``() =
    assertNoMatch
        (String.concat "&" [
           """~(.*and.*)"""
           """[A-Z][\w-{}\\' ,]+"""
           @"(?<=or=\{.*).*"
           @"(?<=\W).*"
           @".*(?=.*\},)"
           @".*(?=\W)b"
        ])
        bibtexEntry


    // printAllDerivatives @"(?<=or=\{.*)(?<=\W)(~(.*and.*)&[A-Z][\w-{}\\' ,]+)(?=.*\},)(?=\W)" bibtexEntry [
    //     ["De Vathaire, Florent";]
    // ]

//
// join "&" [
//             // """(?<=or=\{.*)\b(~(.*and.*)&\S[\w-{}\\' ,]+\S)\b(?=.*\},)"""
//             """~(.*and.*)&[A-Z][\w-{}\\' ,]+"""
//             @"(?<=or=\{.*).*"
//             @"(?<=\W).*"
//             @".*(?=.*\},)"
//             @".*(?=\W)"
//         ]


[<Fact>]
let ``multiple nullables 1``() =
    assertNullablePositions "(?<=c.*)(ab){1,3}" "c_ababab" [
        6; 4; 2
    ]

[<Fact>]
let ``multiple nullables 2``() =
    assertNullablePositions "(ab){1,3}(?=.*c)" "__ababab_c" [
        6; 4; 2
    ]


//
// [<Fact>]
// let ``anchor bol 1``() =
//     _04_DerivativeTests.testPartDerivatives
//         (@"^\d+", "123", [ @"φ*" ])
//
// [<Fact>]
// let ``anchor eol 1``() =
//     _04_DerivativeTests.testPartDerivatives
//         (@"1$", "1", [ @"((?=\n)|(?!⊤))"; @"((?!⊤)|(?=\n))" ])


//
// [<Fact>]
// let ``neg simple 1``() = assertNullablePositions "(?!a)" "b" [ 1; 0 ]
//
// [<Fact>]
// let ``neg simple 1.1``() = assertNullablePositions "(?<!a)" "b" [ 1; 0 ]
//
// [<Fact>]
// let ``neg simple 1.2``() = assertNullablePositions "(?<!a)" "a" [ 0 ]
//
//
// [<Fact>]
// let ``neg simple 2``() = assertNullablePositions "b(?!a)" "bb" [ 1; 0 ]
//
// [<Fact>]
// let ``neg simple 3``() = assertNullablePositions "b(?!aa)" "bb" [ 1; 0 ]
//
// [<Fact>]
// let ``neg simple 4``() = assertNullablePositions "b(?!aaa)" "bb" [ 1; 0 ]
//
//
// [<Fact>]
// let ``neg simple 5``() = assertNullablePositions "b(?!a)" "ba" [ ]

//
// [<Fact>]
// let ``neg lookahead 1``() =
//     assertNullablePositions "bb(?!aa)" "__bb__" [ 2 ]
//
// [<Fact>] // difficult case, can start match from bb|aa
// let ``neg lookahead 2``() =
//     assertNullablePositions "bb(?!aa)" "__bbaa" [ ]
//
// [<Fact>]
// let ``neg lookahead 3``() =
//     assertNullablePositions "bb(?!aa)" "__bb" [ 2 ]
//
// [<Fact>]
// let ``neg lookahead 4``() =
//     assertNullablePositions "bb(?!aa)" "__bb_" [ 2 ]
//

// [<Fact>] // difficult case, can start match from bb|aa
// let ``neg lookahead 2``() =
//     assertNullablePositions "bb(?!aa)" "__bbaa" [ ]

// [<Fact>]
// let ``lookahead comparison 1``() =
//     // let _ = getAllLLmatches "aa(?!bb)" "aabb_aa_aab" [5,2; 8,2]
//     let expected = [5,2; 8,2] // expected for "aa(?!bb)"
//     // |bb - not null, b|b - not null, bb| - null,
//     // let matches = getAllLLmatches "aa(?=~(bb))" "aabb_aa_aab" |> matchPosToTuples
//     // let matches = getAllLLmatches "aa(?=~(⊤*))" "aabb_aa_aab" |> matchPosToTuples
//     let matches = getAllLLmatches "aa(?=~(|b|bb))" "aabb_aa_aab" |> matchPosToTuples
//     assertAllEqual expected matches
//


[<Fact>]
let ``testing anchors 1.1``() = assertRawDerivative """\ba""" "a " [
    "ε"
    @"(ε|(?<=⊥)a)"
]

[<Fact>]
let ``testing anchors 1.2``() = assertRawDerivative """⊤*\ba""" "a " [
    @"(⊤*((?<=φ)|\A)a)?"
    @"(⊤*((?<=φ)|\A)a|ε)"
    @"(⊤*(\A|(?<=φ))a|ε)"
    @"(ε|⊤*((?<=φ)|\A)a)"
    @"(ε|⊤*(\A|(?<=φ))a)"
    @"(⊤*(\A|(?<=φ))a)?"
    // --
    @"(⊤*(?<=(\A|φ))a)?"
    @"(⊤*(?<=(φ|\A))a)?"
]

[<Fact>]
let ``testing anchors 1.3``() = assertNullablePositions """a\b""" "a " [ 0 ]

[<Fact>]
let ``testing anchors 1.4a``() = assertNullablePositions """\b1\b""" "1" [ 0 ]

[<Fact>]
let ``testing anchors 1.4b``() = assertMatchEnd """\b1\b""" "1" 0 1


[<Fact>]
let ``testing anchors 1.4c``() = assertFirstMatchText """\b1\b""" "1" "1"


[<Fact>]
let ``testing anchors 1.5a``() =
    assertFirstMatchText """\b11\b""" "11" "11"

[<Fact>]
let ``testing anchors 1.5b``() =
    assertFirstMatchText """\b11\b""" " 11" "11"

[<Fact>]
let ``testing anchors 1.5b1``() =
    assertNullablePositions """\b11""" " 11" [ 1 ]


[<Fact>]
let ``testing anchors 1.5b2``() =
    assertMatchEndNoLookback """\b11""" " 11" 1 3

[<Fact>]
let ``testing anchors 1.5c``() =
    assertFirstMatchText """\b11\b""" "11 " "11"

[<Fact>]
let ``testing anchors 1.5d``() =
    assertFirstMatchText """\b11\b""" " 11 " "11"


[<Fact>]
let ``testing anchors 1.6a1``() =
    assertNullablePositions """(?<=(\W|\A))11""" "11" [0]


[<Fact>]
let ``testing anchors 1.6a2``() =
    assertFirstMatchText """(?<=\W|\A)11""" "11" "11"

[<Fact>]
let ``testing anchors 1.6b``() =
    assertFirstMatchText """(\A|(?<=\W))11""" "11" "11"


[<Fact>]
let ``testing anchors 1.6c``() =
    assertFirstMatchText """(?<=\W|\A)11""" " 11" "11"

[<Fact>]
let ``testing anchors 1.7a``() =
    assertFirstMatchText """11(?=\W|\z)""" "11" "11"

[<Fact>]
let ``testing anchors 1.7b``() =
    assertFirstMatchText """11(?=\W|\z)""" "11 " "11"

[<Fact>]
let ``testing anchors 1.7c``() =
    assertNullablePositions @"1\b(?=.*2)" " 1 2" [1]

[<Fact>]
let ``testing anchors 1.7c1``() =
    assertNullablePositions @"1(?=.*3)(?=.*2)" " 1 23" [1]


[<Fact>]
let ``rewrite anchors 1.1a``() =
    assertFirstMatchText """11\b""" "11" "11"

[<Fact>]
let ``rewrite anchors 1.1b``() =
    assertFirstMatchText """11\b""" "11 " "11"


[<Fact>]
let ``app test 1``() =
    assertFirstMatchText """[0-9]{2}[/.-][0-9]{2}[/.-]([0-9]{4}|[0-9]{2})&^.*$""" "01.01.2023" "01.01.2023"

[<Fact>]
let ``app test 2``() =
    assertFirstMatchText """[0-9]{2}[/.-][0-9]{2}[/.-]([0-9]{4}|[0-9]{2})&.*$""" "01.01.2023\n" "01.01.2023"


#endif