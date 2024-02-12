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
        | None -> failwith $"TODO: could not rewrite lookaround:\n{regexNode}"
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


let rec determineWordBorderNodeKind (left:bool) (node:RegexNode) =
    let rtl = node.Options.HasFlag(RegexOptions.RightToLeft)
    let trueLeft = if rtl then not left else left
    let edgeIdx = if trueLeft then node.ChildCount() - 1 else 0
    let unhandled() = failwith $"todo: rewrite boundary: {node}"

    let inferSet (setStr:string) =
        match setStr with
        | RegexCharClass.NotSpaceClass
        | RegexCharClass.WordClass -> Some true
        | RegexCharClass.SpaceClass -> Some false
        | _ -> unhandled()

    match node.Kind with
    | RegexNodeKind.One ->
        if RegexCharClass.IsBoundaryWordChar node.Ch then Some true
        else Some false
    | RegexNodeKind.Oneloop when node.M > 0 ->
        if RegexCharClass.IsBoundaryWordChar node.Ch then Some true
        else Some false
    | RegexNodeKind.Notoneloop ->
        // todo: only conclude something if loop is fixed length
        None

    | RegexNodeKind.Setloop ->
        let nonEmpty = node.M > 0
        if not nonEmpty then None
        else inferSet node.Str
    | RegexNodeKind.Notone -> unhandled()
    | RegexNodeKind.Set -> inferSet node.Str

    | RegexNodeKind.Multi ->
        // AA_ <- last char if to left
        let chr = if trueLeft then node.Str[node.Str.Length - 1] else node.Str[0]
        if RegexCharClass.IsBoundaryWordChar chr then Some true
        else Some false

    | RegexNodeKind.Conjunction ->
        children2Seq node
        |> Seq.map (determineWordBorderNodeKind left)
        |> Seq.tryPick id

    | RegexNodeKind.Capture
    | RegexNodeKind.Concatenate
    | RegexNodeKind.PositiveLookaround ->
        let edgeChild = node.Child(edgeIdx)
        determineWordBorderNodeKind left edgeChild
    | _ -> None

let toLeft (outer:RegexNode array) idx =
    match idx with
    | 0 -> None
    | n ->
        let temp = outer[n-1]
        let kind = determineWordBorderNodeKind true temp
        kind

let toRight (outer:RegexNode array) idx =
    match idx with
    | n when n = outer.Length - 1 -> None
    | n ->
        let temp = outer[n+1]
        let kind = determineWordBorderNodeKind false temp
        kind

let rewriteWordBorder (b:RegexBuilder<BDD>) (outer:RegexNode array) ((idx,node): (int * RegexNode) ) =
    let left = toLeft outer idx
    let right = toRight outer idx
    match left, right with
    // wordchar right
    | _, Some true -> idx,b.anchors._nonWordLeft.Value
    | _, Some false -> idx,b.anchors._wordLeft.Value
    // wordchar left
    | Some true, _   -> idx,b.anchors._nonWordRight.Value
    | Some false, _  -> failwith "todo"

    | _ ->
        if outer.Length = 1 then
            idx, b.anchors._wordBorder.Value
        else
            failwith "Sbre does not support unconstrained word borders"
        // failwith "TODO: REWRITE WORD BORDER"


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

        let convertChildren(node: System.Text.RuntimeRegexCopy.RegexNode): RegexNode<BDD> list =
            let nodeseq =
                match node.Options.HasFlag(RegexOptions.RightToLeft) with
                | true -> node |> children2Seq |> Seq.rev
                | _ ->  node |> children2Seq
                |> Seq.toArray

            let existsWB =
                nodeseq
                |> Seq.indexed
                |> Seq.where (fun (idx,node) -> node.Kind = RegexNodeKind.Boundary )
                |> Seq.toArray
            match existsWB with
            | [| |] ->
                let defaultResult = nodeseq |> Seq.collect (loop []) |> Seq.toList
                defaultResult
            | _ ->
#if NO_REWRITE
                let defaultResult = nodeseq |> Seq.collect (loop []) |> Seq.toList
                if true then defaultResult else
#endif
                // have to rewrite word borders here
                let rewritten =
                    existsWB
                    |> Seq.map (rewriteWordBorder b nodeseq)
                    |> Map.ofSeq
                let converted =
                    nodeseq
                    |> Seq.indexed
                    |> Seq.map (fun (idx,node) ->
                        match rewritten |> Map.tryFind idx with
                        | Some v -> [v]
                        | _ -> (loop []) node
                    )
                    |> Seq.collect id
                    |> Seq.toList
                converted




        // let inline convertChildren(node: System.Text.RuntimeRegexCopy.RegexNode) =
        //     match node.Options.HasFlag(RegexOptions.RightToLeft) with
        //     | false -> node |> children2Seq |> Seq.collect (loop []) |> Seq.toList
        //     | true -> node |> children2Seq |> Seq.rev |> Seq.collect (loop []) |> Seq.toList

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
        | RegexNodeKind.Bol -> b.anchors._caretAnchor.Value :: acc
        | RegexNodeKind.Beginning -> b.anchors._bigAAnchor :: acc
        | RegexNodeKind.Eol -> b.anchors._dollarAnchor.Value :: acc
        | RegexNodeKind.EndZ -> b.anchors._endZAnchor.Value :: acc
        | RegexNodeKind.End -> b.anchors._zAnchor :: acc //b.anchors._zAnchor.Value :: acc // end of string only
        | RegexNodeKind.Boundary ->
            // failwith "TODO: rewrite to lookaround"
            // TODO :WB
            RegexNode<BDD>.Anchor WordBorder :: acc
            // b.anchors._wordBorder.Value :: acc
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
#if NO_REWRITE_NEGATIVE
            failwith $"negative lookarounds not supported: {node}"
#else
            let negLookaround = builder.mkLookaround(b.mkConcat (convertChildren node),node.Options.HasFlag(RegexOptions.RightToLeft),true)
            let rewrittenLookaround = rewriteNegativeLookaround b negLookaround
            rewrittenLookaround
            :: acc
#endif
        | other -> failwith $"RegexNodeKind conversion not implemented: {other}, \n{rootNode}"

    let result = loop [] rootNode

    result |> List.rev |> b.mkConcat
