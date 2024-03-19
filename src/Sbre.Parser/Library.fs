module Sbre.Parser

open FParsec

type Pattern =
    | Implication of a:Pattern * b:Pattern
    | Concat of Pattern list
    | String of string
    | Group of Pattern
    | Union of Pattern list
    | Intersection of Pattern list
    | CharSet of string

#nowarn "3370"
module Syntax =
    let str = pstring
    let ws = spaces
    let c = pchar
    let logicChars = "→≡≢"
    let unescaped chr: Parser<_,unit> = noneOf @"\" >>? pchar chr
    let r_nonmetachar: Parser<string,unit> = many1Chars (noneOf ("|&~()" + logicChars))
    let r_basic: Parser<Pattern,unit> = r_nonmetachar |>> Pattern.String
    let r_charset: Parser<Pattern,unit> = pstring "[" >>. manyCharsTill anyChar (unescaped ']') |>> CharSet
    let r_group, r_groupRef = createParserForwardedToRef()
    let r_concat, r_concatRef = createParserForwardedToRef()
    let r_simple, r_simpleRef = createParserForwardedToRef()
    let r_inter, r_interRef = createParserForwardedToRef()
    let r_union, r_unionRef = createParserForwardedToRef()
    let r_regex, r_regexRef = createParserForwardedToRef()
    let r_logicExpression, r_logicExpressionRef = createParserForwardedToRef()

    r_groupRef := between (c '(') (c ')') (r_regex) |>> Pattern.Group

    r_simpleRef := choice [
        r_group
        r_charset
        r_basic
    ]
    r_concatRef :=
        many r_simple
        |>> function [ single ] -> single | more -> Pattern.Concat more
    r_interRef :=
        sepBy1 r_concat (pchar '&')
        |>> function [ single ] -> single | more -> Pattern.Intersection more
    r_unionRef :=
        sepBy1 r_inter (pchar '|')
        |>> function [ single ] -> single | more -> Pattern.Union more
    r_logicExpressionRef :=
        r_union .>>.? (anyOf logicChars) .>>.? r_union
        |>> function
            | (left,operator),right ->
                match operator with
                | '→' -> Pattern.Implication(left,right)
                // | '≡'; '≢'
                | _ -> failwith "todo"

    r_regexRef :=
        (r_logicExpression <|> r_union)


// let pattern = ".*A.*→.*B.*"

