module internal rec Sbre.RegexNodeConverter

open System
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open Sbre

let children2Seq(node: System.Text.RuntimeRegexCopy.RegexNode) =
    seq {
        for i = 0 to node.ChildCount() - 1 do
            yield node.Child(i)
    }

let convertToSymbolicRegexNode
    (
        css: CharSetSolver,
        runtimeBuilder: SymbolicRegexBuilder<BDD>,
        builder: RegexBuilder<BDD>,
        rootNode: System.Text.RuntimeRegexCopy.RegexNode
    )
    : RegexNode<BDD>
    =

    let bddSolver = css
    let n = builder

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
        | RegexNodeKind.One -> n.one node.Ch :: acc
        | RegexNodeKind.Notone -> n.notOne node.Ch :: acc
        | RegexNodeKind.Set -> n.setFromNode node :: acc
        | RegexNodeKind.Multi -> (node.Str |> Seq.map n.one |> Seq.toList) @ acc
        | RegexNodeKind.Lazyloop
        | RegexNodeKind.Loop ->
            let inner = n.mkConcat (convertChildren node)
            let current = n.mkLoop (inner, node.M, node.N)
            current :: acc
        | RegexNodeKind.Alternate ->
            let children2 =
                node
                |> children2Seq
                |> Seq.map convertSingle
                |> Seq.map n.mkConcat
                |> Seq.toArray

            let nodeset = ofSeq children2
            builder.mkOr children2 :: acc
        | RegexNodeKind.Conjunction ->

            let children2 =
                node |> children2Seq |> Seq.map convertSingle |> Seq.map n.mkConcat |> Seq.toArray

            builder.mkAnd children2 :: acc

        | RegexNodeKind.Concatenate ->
            let inner = convertChildren node
            inner @ acc
        | RegexNodeKind.Capture ->
            if node.N = -1 then
                convertChildren node |> n.mkConcat |> List.singleton
            else
                let _ = 1

                if node.Options.HasFlag(RegexOptions.Negated) then
                    let inner = convertChildren node |> n.mkConcat
                    let mutable flags = Info.Flags.inferNode inner

                    if flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) then
                        Info.removeFlag &flags RegexNodeFlags.CanBeNullable
                        Info.removeFlag &flags RegexNodeFlags.IsAlwaysNullable
                    else if not (flags.HasFlag(RegexNodeFlags.CanBeNullable)) then
                        Info.addFlag &flags RegexNodeFlags.CanBeNullable
                        Info.addFlag &flags RegexNodeFlags.IsAlwaysNullable

                    RegexNode.Not(inner, { Flags = flags; Startset = Unchecked.defaultof<_> })
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
            let single = Singleton bdd
            // TBD: explore possibilities of rewrites here
            match node.M, node.N with
            // +
            | 1, Int32.MaxValue -> single :: n.mkLoop (single, 0, Int32.MaxValue) :: acc
            | _ -> n.mkLoop (single, node.M, node.N) :: acc

        // anchors
        | RegexNodeKind.Beginning -> n.anchors._bigAAnchor.Value :: acc // TBD:  ^ or \A in multiline
        | RegexNodeKind.EndZ -> n.anchors._dollarAnchor.Value :: acc // TBD:  $ or \z in multiline
        | RegexNodeKind.Boundary -> n.anchors._wordBorder.Value :: acc
        | RegexNodeKind.NonBoundary -> n.anchors._nonWordBorder.Value :: acc
        | RegexNodeKind.Setlazy
        | RegexNodeKind.Setloop ->
            let set = node.Str
            let bdd = n.bddFromSetString set
            n.mkLoop (n.one bdd, node.M, node.N) :: acc
        | RegexNodeKind.Empty -> acc
        | RegexNodeKind.PositiveLookaround ->
            let inner = n.mkConcat (convertChildren node)

            RegexNode.LookAround(
                inner,
                lookBack = node.Options.HasFlag(RegexOptions.RightToLeft),
                negate = false
            )
            :: acc
        | RegexNodeKind.NegativeLookaround ->
            let inner = n.mkConcat (convertChildren node)

            RegexNode.LookAround(
                inner,
                lookBack = node.Options.HasFlag(RegexOptions.RightToLeft),
                negate = true
            )
            :: acc
        | other -> failwith $"RegexNodeKind conversion not implemented: {other}, \n{rootNode}"

    let result = loop [] rootNode

    result |> List.rev |> n.mkConcat
