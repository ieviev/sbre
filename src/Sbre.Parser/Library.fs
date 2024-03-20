module Sbre.Parser

open FParsec

type Pattern =
    | Concat of Pattern list
    | String of string
    | Not of Pattern
    | Group of Pattern
    | Union of Pattern list
    | Intersection of Pattern list
    | CharSet of string
    static member toString (pat:Pattern) =
        match pat with
        | Concat patterns ->
            String.concat "" (Seq.map Pattern.toString patterns)
        | String s -> s
        | Not pattern ->
            $"~({Pattern.toString pattern})"
        | Group pattern ->
            match pattern with
            | Union _ -> Pattern.toString pattern
            | _ -> $"({Pattern.toString pattern})"
        | Union patterns ->
            let inner = String.concat "|" (Seq.map Pattern.toString patterns)
            $"({inner})"
        | Intersection patterns ->
            let inner = String.concat "&" (Seq.map Pattern.toString patterns)
            $"({inner})"
        | CharSet s -> $"[{s}]"

#nowarn "3370"
module Syntax =
    let str = pstring
    let ws = spaces
    let c = pchar
    let logicChars = "→≡≢"
    let requireEscaping = @"|&()\" + logicChars
    let unescaped chr: Parser<_,unit> = noneOf @"\" .>>? pchar chr
    // .>>.?
    let r_nonmetachar: Parser<string,unit> =
        choice [
            pchar '\\' .>>. (anyChar) |>> (fun (f,s) -> $"{f}{s}" )
            many1Chars (noneOf (requireEscaping))
        ]
    let r_basic: Parser<Pattern,unit> = r_nonmetachar |>> Pattern.String
    let r_charset: Parser<Pattern,unit> =
        pstring "["
        >>. manyCharsTillApply anyChar (unescaped ']') (fun v b -> $"{v}{b}" )
        |>> CharSet
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
        sepBy r_concat (pchar '&')
        |>> function [ single ] -> single | more -> Pattern.Intersection more
    r_unionRef :=
        sepBy r_inter (pchar '|')
        |>> function [ single ] -> single | more -> Pattern.Union more
    r_logicExpressionRef :=
        r_union .>>.? (anyOf logicChars) .>>.? r_union
        |>> function
            | (left,operator),right ->
                match operator with
                | '→' ->
                    Pattern.Union [ Pattern.Not(left); right ]
                | '≢' ->
                    Pattern.Intersection [
                        Pattern.Not(Pattern.Intersection([left;right]))
                        Pattern.Union([left;right])
                    ] // xor
                | '≡' ->
                    Pattern.Union [
                        Pattern.Not(Pattern.Union([left;right]))
                        Pattern.Intersection([left;right])
                    ]
                | _ -> failwith "todo"
    r_regexRef :=
        choice [
            r_logicExpression
            r_union
        ]



let parsePattern (pattern:string) =
    let parseResult = run Syntax.r_regex pattern
    match parseResult with
    | ParserResult.Success(res,_,_) -> Pattern.toString res
    | ParserResult.Failure(_) -> failwith (string parseResult)



