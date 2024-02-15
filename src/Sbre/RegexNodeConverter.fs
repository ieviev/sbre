module internal rec Sbre.RegexNodeConverter

open System
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Info
open Sbre.Types
open Sbre

let children2Seq(node: System.Text.RuntimeRegexCopy.RegexNode) =
    seq { for i = 0 to node.ChildCount() - 1 do yield node.Child(i) }


let rewriteNegativeLookaround (b:RegexBuilder<BDD>) (lookBack:bool) (node:RegexNode<BDD>) : RegexNode<BDD> =
    match lookBack with
    | false ->
        // (?=~(R·⊤*)·\z) ≡ (?!R)
        let negpart = b.mkNot(b.mkConcat2(node, b.uniques._trueStar))
        let conc = b.mkConcat2(negpart, b.anchors._zAnchor)
        b.mkLookaround( conc, false, 0, Set.empty)
    | true ->
        // (?<=\A·~(⊤*R)) ≡ (?<!R)
        let negpart = b.mkNot(b.mkConcat2(b.uniques._trueStar,node))
        let conc = b.mkConcat2(b.anchors._bigAAnchor,negpart)
        b.mkLookaround( conc, false, 0, Set.empty)


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

let rewriteWordBorder (b:RegexBuilder<BDD>) (outer:RegexNode array) (idx:int) ((node): RegexNode) =
    let left = toLeft outer idx
    let right = toRight outer idx
    match left, right with
    | _, Some true -> b.anchors._nonWordLeft.Value // wordchar right
    | _, Some false -> b.anchors._wordLeft.Value // nonwordright
    | Some true, _   -> b.anchors._nonWordRight.Value // wordleft
    | Some false, _  -> b.anchors._wordRight.Value    // nonwordleft
    | _ ->
        if outer.Length = 1 then
            b.anchors._wordBorder.Value
        else
            failwith @"Sbre does not support unconstrained word borders, rewrite \b.* to \b\w.* or .*\w\b to show which side the word is on"



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

        let rec convertAdjacent (adjacent:System.Text.RuntimeRegexCopy.RegexNode[]) (idx:int) (node: System.Text.RuntimeRegexCopy.RegexNode): RegexNode<BDD> list =
            match node.Kind with
            | RegexNodeKind.Alternate ->
                let inner = node |> children2Seq
                let allrewritten =
                    inner
                    |> Seq.map (convertAdjacent adjacent idx)
                    |> Seq.map b.mkConcat
                    |> b.mkOrSeq
                [allrewritten]
            | RegexNodeKind.Boundary ->
                let rewritten = (rewriteWordBorder b adjacent idx (node))
                [rewritten]
            | _ -> (loop []) node


        let convertConcat(outerConcat: System.Text.RuntimeRegexCopy.RegexNode): RegexNode<BDD> list =
            let outerCorrectOrder =
                match outerConcat.Options.HasFlag(RegexOptions.RightToLeft) with
                | true -> node |> children2Seq |> Seq.rev
                | _ ->  node |> children2Seq
                |> Seq.toArray
            outerCorrectOrder
            |> Seq.mapi (convertAdjacent outerCorrectOrder)
            |> Seq.collect id
            |> Seq.toList

        let convertChildren(node: System.Text.RuntimeRegexCopy.RegexNode): RegexNode<BDD> list =
            let nodeseq =
                match node.Options.HasFlag(RegexOptions.RightToLeft) with
                | true -> node |> children2Seq |> Seq.rev
                | _ ->  node |> children2Seq
                |> Seq.toArray

            nodeseq |> Seq.collect (loop []) |> Seq.toList


        let convertSingle(node: System.Text.RuntimeRegexCopy.RegexNode) =
            node |> (loop [])

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
            let adjacent = node.Parent |> children2Seq |> Seq.toArray
            let ownIndex = adjacent |> Seq.findIndex (fun v -> obj.ReferenceEquals(node,v) )
            let children2 =
                node
                |> children2Seq
                |> Seq.map (convertAdjacent adjacent ownIndex)
                |> Seq.map b.mkConcat
            builder.mkOrSeq children2 :: acc
        | RegexNodeKind.Conjunction ->
            let children2 =
                node |> children2Seq |> Seq.map convertSingle |> Seq.map b.mkConcat

            builder.mkAnd children2 :: acc

        | RegexNodeKind.Concatenate ->
            let inner = convertConcat node
            inner @ acc
        | RegexNodeKind.Capture ->
            if node.N = -1 then
                convertConcat node |> b.mkConcat |> List.singleton
            else
                if node.Options.HasFlag(RegexOptions.Negated) then
                    let inner = convertConcat node |> b.mkConcat
                    b.mkNot(inner)
                    :: acc
                else
                    convertConcat node
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
            failwith "TODO: rewrite to lookaround"
            // // TODO :WB
            // RegexNode<BDD>.Anchor WordBorder :: acc
            // b.anchors._wordBorder.Value :: acc
        | RegexNodeKind.NonBoundary ->
            failwith "TODO: reimplement word border"
            b.anchors._nonWordBorder.Value :: acc
        | RegexNodeKind.Setlazy
        | RegexNodeKind.Setloop ->
            let set = node.Str
            let bdd = b.bddFromSetString set
            b.mkLoop (b.one bdd, node.M, node.N) :: acc
        | RegexNodeKind.Empty -> acc
        | RegexNodeKind.PositiveLookaround ->
            let conc = b.mkConcat (convertConcat (node))
            builder.mkLookaround(conc,node.Options.HasFlag(RegexOptions.RightToLeft), 0, Set.empty)
            :: acc
        | RegexNodeKind.NegativeLookaround ->
#if NO_REWRITE_NEGATIVE
            failwith $"negative lookarounds not supported: {node}"
#else
            let lookBack = node.Options.HasFlag(RegexOptions.RightToLeft)
            let lookBody = b.mkConcat (convertChildren node)
            let rewrittenLookaround = rewriteNegativeLookaround b lookBack lookBody
            rewrittenLookaround
            :: acc
#endif
        | other -> failwith $"RegexNodeKind conversion not implemented: {other}, \n{rootNode}"

    let result = loop [] rootNode

    result |> List.rev |> b.mkConcat
