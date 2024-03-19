#r "nuget: FParsec"
#load "Library.fs"

open System
open FParsec
open System.Text.RegularExpressions
open System.Globalization

// implication: →
// equivalence: ≡
// non-equivalence: ≢

// run (many1 Parsers.r_regex) "ab(cd|ef)"
// run (r_regex) "[ab\]cabc]"

// let pat1 = "ab(cd|ef)"
//
// let test = run pcharset "(123)"

let logicOperator = (anyOf [ '→'; '≡'; '≢'; ])
let parseLogicExpression: Parser<_,unit> =
    parse {
        let! left, operator = many1CharsTillApply anyChar logicOperator (fun str op -> str, op )
        let! right = restOfLine false
        return [
            left; operator.ToString(); right
        ]
    }


let reg = Sbre.Parser.Syntax.r_regex
let parse1 = CharParsers.run (many1 reg) "ab(cd|ef)"

stdout.WriteLine $"%A{parse1}"







