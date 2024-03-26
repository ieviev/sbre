module Sbre.Parser

open FParsec

[<RequireQualifiedAccess>]
type Pattern =
    private
    | Concat of Pattern list
    | String of string
    | Not of Pattern
    | Group of Pattern
    | Or of Pattern list
    | And of Pattern list
    | CharSet of string
    | Metachar of string
    static member toString (pat:Pattern) =
        match pat with
        | Concat patterns ->
            String.concat "" (Seq.map Pattern.toString patterns)
        | String s -> s
        | Not pattern ->
            $"~({Pattern.toString pattern})"
        | Group pattern ->
            match pattern with
            | Or _ -> Pattern.toString pattern
            | _ -> $"({Pattern.toString pattern})"
        | Or patterns ->
            let inner = String.concat "|" (Seq.map Pattern.toString patterns)
            $"({inner})"
        | And patterns ->
            let inner = String.concat "&" (Seq.map Pattern.toString patterns)
            $"({inner})"
        | CharSet s -> $"[{s}]"
        | Metachar s -> $@"\{s}"


    static member mkOr (inner:Pattern list) =
        match inner with
        | [] -> String ""
        | [ single ] -> single
        | _ -> Pattern.Or inner





let rec minimizeOrLeft (patterns:Pattern seq) =
    let strings =
        patterns |> Seq.choose (function Pattern.String v -> Some v | _ -> None)
    let others =
        patterns |> Seq.choose (function Pattern.String _ -> None | v -> Some v)
    let grouped =
        strings
        |> Seq.groupBy (fun v ->
            if v.Length = 0 then None else
            Some (v[0])
        )
    seq {
        for (prefix,group) in grouped do
            match prefix with
            | Some '\\'  | None ->
                let newOr = group |> Seq.map Pattern.String |> Seq.toList
                match newOr with
                | [ single ] ->
                    yield single
                | items ->
                    yield Pattern.Or newOr
            | Some (chr) ->
                let left = Pattern.String $"%c{chr}"
                let right =
                    minimizeOrLeft (group |> Seq.map (fun v -> Pattern.String v[1..]))
                    |> Seq.toList
                    |> Pattern.Or
                yield Pattern.Concat [
                    left
                    right
                ]
    }
    |> Seq.append others
    |> Seq.toList


let rec minimizeOrRight (patterns:Pattern seq) : Pattern list =
    let strings =
        patterns |> Seq.choose (function Pattern.String v -> Some v | _ -> None)
    let others =
        patterns |> Seq.choose (function Pattern.String _ -> None | v -> Some v)
    let grouped =
        strings
        |> Seq.groupBy (fun v ->
            if v.Length = 0 then None else
            Some (v[v.Length - 1])
        )
    seq {
        for (prefix,group) in grouped do
            match prefix with
            | Some ('\\')  | None ->
                let newOr = group |> Seq.map Pattern.String |> Seq.toList
                match newOr with
                | [ single ] ->
                    yield single
                | items ->
                    yield Pattern.Or newOr
            | Some (chr) ->
                let l =
                    minimizeOrRight (group |> Seq.map (fun v -> Pattern.String v[..v.Length - 2]))
                    |> Seq.toList
                    |> Pattern.mkOr
                let r = Pattern.String $"%c{chr}"
                yield Pattern.Concat [
                    l
                    r
                ]
    }
    |> Seq.append others
    |> Seq.toList





let rec minimize (pat:Pattern) =
    match pat with
    | Pattern.Or(nodes) ->
        minimizeOrLeft (nodes)

        |> Pattern.mkOr
        // minimizeOrRight (nodes)
        // |> Pattern.mkOr
    | Pattern.Concat(nodes) ->
        match nodes with
        | [ Pattern.Concat [h1; Pattern.String tail1 ]; Pattern.String tail2 ] ->
            Pattern.Concat [ minimize h1; Pattern.String (tail1 + tail2) ]
        | [h1; t1 ] ->
            Pattern.Concat [ minimize h1; minimize t1 ]
        | _ ->
            pat
    | _ -> pat


#nowarn "3370"
module Syntax =
    let str = pstring
    let ws = spaces
    let c = pchar
    let logicChars = "→≡≢⊃"
    let requireEscaping = @"|&()\" + logicChars
    let unescaped chr: Parser<_,unit> = noneOf @"\" .>>? pchar chr
    // .>>.?
    let r_basic: Parser<Pattern,unit> =
        choice [
            pchar '\\' .>>. (anyChar) |>> (fun (f,s) -> $"%c{s}" ) |>> Pattern.Metachar
            many1Chars (noneOf (requireEscaping)) |>> Pattern.String
        ]
    let r_charset: Parser<Pattern,unit> =
        pstring "["
        >>. manyCharsTillApply anyChar (unescaped ']') (fun v b -> $"{v}{b}" )
        |>> Pattern.CharSet
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
        |>> function [ single ] -> single | more -> Pattern.And more
    r_unionRef :=
        sepBy r_inter (pchar '|')
        |>> function [ single ] -> single | more -> Pattern.Or more
    r_logicExpressionRef :=
        r_union .>>.? (anyOf logicChars) .>>.? r_union
        |>> function
            | (left,operator),right ->
                match operator with
                | '→' | '⊃' ->
                    Pattern.Or [ Pattern.Not(left); right ]
                | '≢' ->
                    Pattern.And [
                        Pattern.Not(Pattern.And([left;right]))
                        Pattern.Or([left;right])
                    ] // xor
                | '≡' ->
                    Pattern.Or [
                        Pattern.Not(Pattern.Or([left;right]))
                        Pattern.And([left;right])
                    ]
                | _ -> failwith "todo"
    r_regexRef :=
        choice [
            r_logicExpression
            r_union
        ]



let parseAsData (pattern:string) =
    let parseResult = run Syntax.r_regex pattern
    match parseResult with
    | ParserResult.Success(res,_,_) -> (res)
    | ParserResult.Failure(errs,_,_) -> failwith errs


let processString (pattern:string) =
    let parseResult = run Syntax.r_regex pattern
    match parseResult with
    | ParserResult.Success(res,_,_) -> Pattern.toString (res)
    | ParserResult.Failure(errs,s1,s2) ->
        failwith $"{errs}\npattern:{pattern}"



