module internal rec Sbre.RegexNodeConverter

open System
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Info
open Sbre.Types
open Sbre

let children2Seq(node: System.Text.RuntimeRegexCopy.RegexNode) =
    seq { for i = 0 to node.ChildCount() - 1 do yield node.Child(i) }




let rewriteNegativeLookaround (b:RegexBuilder<BDD>) (node:RegexNode<BDD>) : RegexNode<BDD> =
    match node with
    | LookAround(regexNode, lookBack, negate, relativeNullablePos) ->
        let fixLen = Node.getFixedLength regexNode
        match fixLen with
        | None -> failwith "TODO: could not rewrite lookaround"
        | Some minLength ->
            match lookBack with
            | false ->
                // aa(?!bb) => aa(?=~(⊤{0,1}\z|bb⊤*))
                let earlyEnd = b.mkConcat2(b.mkLoop(b.uniques._true,0,minLength - 1), Anchor End)
                let requiredDistance =
                    b.mkLoop(b.uniques._true,minLength,minLength)
                let rewrittenNode =
                    b.mkConcat([b.uniques._trueStar;regexNode;b.uniques._trueStar])
                let rewrittenCompl =
                    b.mkOr([
                        earlyEnd // either end of string
                        b.mkAnd([
                            requiredDistance
                            b.mkNot(rewrittenNode)
                        ])
                    ])
                let rewrittenLookaround =
                    b.mkLookaround( rewrittenCompl, false, false )
                rewrittenLookaround
            | true ->
                let earlyStart = b.mkConcat2(Anchor Begin,b.mkLoop(b.uniques._true,0,minLength - 1))
                let requiredDistance =
                    b.mkLoop(b.uniques._true,minLength,minLength)
                let rewrittenNode =
                    b.mkConcat([b.uniques._trueStar;regexNode;b.uniques._trueStar])
                let rewrittenCompl =
                    b.mkOr([
                        earlyStart // either end of string
                        b.mkAnd([
                            requiredDistance
                            b.mkNot(rewrittenNode)
                        ])
                    ])
                let rewrittenLookaround =
                    b.mkLookaround( rewrittenCompl, true, false )
                rewrittenLookaround
    | _ -> failwith "TODO: could not rewrite lookaround"


let convertToSymbolicRegexNode
    (
        css: CharSetSolver,
        builder: RegexBuilder<BDD>,
        rootNode: System.Text.RuntimeRegexCopy.RegexNode
    )
    : RegexNode<BDD>
    =
    let b = builder

    let rec loop
        (acc: Types.RegexNode<BDD> list)
        (node: System.Text.RuntimeRegexCopy.RegexNode)
        : RegexNode<BDD> list
        =

        let inline convertSingle(node: System.Text.RuntimeRegexCopy.RegexNode) = node |> (loop [])

        let inline convertChildren(node: System.Text.RuntimeRegexCopy.RegexNode) =
            match node.Options.HasFlag(RegexOptions.RightToLeft) with
            | false -> node |> children2Seq |> Seq.collect (loop []) |> Seq.toList
            | true -> node |> children2Seq |> Seq.rev |> Seq.collect (loop []) |> Seq.toList

        match node.Kind with
        | RegexNodeKind.One -> b.one node.Ch :: acc
        | RegexNodeKind.Notone -> b.notOne node.Ch :: acc
        | RegexNodeKind.Set -> b.setFromNode node :: acc
        | RegexNodeKind.Multi -> (node.Str |> Seq.map b.one |> Seq.toList) @ acc
        | RegexNodeKind.Lazyloop
        | RegexNodeKind.Loop ->
            let inner = b.mkConcat (convertChildren node)
            let current = b.mkLoop (inner, node.M, node.N)
            current :: acc
        | RegexNodeKind.Alternate ->
            let children2 =
                node
                |> children2Seq
                |> Seq.map convertSingle
                |> Seq.map b.mkConcat
            builder.mkOr children2 :: acc
        | RegexNodeKind.Conjunction ->
            let children2 =
                node |> children2Seq |> Seq.map convertSingle |> Seq.map b.mkConcat

            builder.mkAnd children2 :: acc

        | RegexNodeKind.Concatenate ->
            let inner = convertChildren node
            inner @ acc
        | RegexNodeKind.Capture ->
            if node.N = -1 then
                convertChildren node |> b.mkConcat |> List.singleton
            else
                if node.Options.HasFlag(RegexOptions.Negated) then
                    let inner = convertChildren node |> b.mkConcat
                    b.mkNot(inner)
                    :: acc
                else
                    convertChildren node
        // Specialized loops
        | RegexNodeKind.Oneloop
        | RegexNodeKind.Onelazy
        | RegexNodeKind.Notoneloop
        | RegexNodeKind.Notonelazy ->
            let bdd =
                match node.IsNotoneFamily with
                | true -> css.Not(css.CreateBDDFromChar(node.Ch))
                | false -> css.CreateBDDFromChar(node.Ch)

            // let single = n.one bdd
            let single = b.one bdd
            // TBD: explore possibilities of rewrites here
            match node.M, node.N with
            // +
            | 1, Int32.MaxValue -> single :: b.mkLoop (single, 0, Int32.MaxValue) :: acc
            | _ -> b.mkLoop (single, node.M, node.N) :: acc

        // anchors
        | RegexNodeKind.Bol -> b.anchors._caretAnchor :: acc
        | RegexNodeKind.Beginning -> b.anchors._bigAAnchor :: acc
        | RegexNodeKind.Eol -> b.anchors._dollarAnchor :: acc
        | RegexNodeKind.EndZ -> b.anchors._endZAnchor.Value :: acc
        | RegexNodeKind.End -> b.anchors._zAnchor :: acc //b.anchors._zAnchor.Value :: acc // end of string only
        | RegexNodeKind.Boundary ->
            b.anchors._wordBorder :: acc
        | RegexNodeKind.NonBoundary ->
            failwith "TODO: reimplement word border"
            b.anchors._nonWordBorder.Value :: acc
        | RegexNodeKind.Setlazy
        | RegexNodeKind.Setloop ->
            let set = node.Str
            let bdd = b.bddFromSetString set
            // unroll loops
            // if node.M = node.N then
            //     let single = b.one bdd
            //     let nodes = List.replicate node.M single
            //     nodes @ acc
            // elif node.M > 0 then
            //     let single = b.one bdd
            //     let nodes = List.replicate node.M single
            //     let optmax =
            //         match node.N with
            //         | Int32.MaxValue ->
            //             node.N
            //         | _ ->
            //             node.N - node.M
            //     let optloop = b.mkLoop (single, 0, optmax)
            //     nodes @ [optloop] @ acc
            // else

            b.mkLoop (b.one bdd, node.M, node.N) :: acc
        | RegexNodeKind.Empty -> acc
        | RegexNodeKind.PositiveLookaround ->
            builder.mkLookaround(b.mkConcat (convertChildren node),node.Options.HasFlag(RegexOptions.RightToLeft),false)
            :: acc
        | RegexNodeKind.NegativeLookaround ->
            failwith "negative lookarounds not supported"
            // let negLookaround = builder.mkLookaround(b.mkConcat (convertChildren node),node.Options.HasFlag(RegexOptions.RightToLeft),true)
            // let rewrittenLookaround = rewriteNegativeLookaround b negLookaround
            // rewrittenLookaround
            // :: acc
        | other -> failwith $"RegexNodeKind conversion not implemented: {other}, \n{rootNode}"

    let result = loop [] rootNode

    result |> List.rev |> b.mkConcat
