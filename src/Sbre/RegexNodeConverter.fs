module internal rec Sbre.RegexNodeConverter

open System
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open Sbre

let children2Seq(node: System.Text.RuntimeRegexCopy.RegexNode) =
    seq { for i = 0 to node.ChildCount() - 1 do yield node.Child(i) }

let convertToSymbolicRegexNode
    (
        css: CharSetSolver,
        runtimeBuilder: SymbolicRegexBuilder<BDD>,
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
                |> Seq.toArray

            let nodeset = ofSeq children2
            builder.mkOr children2 :: acc
        | RegexNodeKind.Conjunction ->

            let children2 =
                node |> children2Seq |> Seq.map convertSingle |> Seq.map b.mkConcat |> Seq.toArray

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
                    let mutable flags = Info.Flags.inferNode inner

                    if flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) then
                        Info.removeFlag &flags RegexNodeFlags.CanBeNullable
                        Info.removeFlag &flags RegexNodeFlags.IsAlwaysNullable
                    else if not (flags.HasFlag(RegexNodeFlags.CanBeNullable)) then
                        Info.addFlag &flags RegexNodeFlags.CanBeNullable
                        Info.addFlag &flags RegexNodeFlags.IsAlwaysNullable

                    RegexNode.Not(inner, RegexNodeInfo<BDD>( Flags = flags, Startset = Unchecked.defaultof<BDD>, InitialStartset = Uninitialized))
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
            | 1, Int32.MaxValue -> single :: b.mkLoop (single, 0, Int32.MaxValue) :: acc
            | _ -> b.mkLoop (single, node.M, node.N) :: acc

        // anchors
        | RegexNodeKind.Beginning -> b.anchors._bigAAnchor.Value :: acc // TBD:  ^ or \A in multiline
        | RegexNodeKind.EndZ -> b.anchors._dollarAnchor.Value :: acc // TBD:  $ or \z in multiline
        | RegexNodeKind.Boundary -> b.anchors._wordBorder.Value :: acc
        | RegexNodeKind.NonBoundary -> b.anchors._nonWordBorder.Value :: acc
        | RegexNodeKind.Setlazy
        | RegexNodeKind.Setloop ->
            let set = node.Str
            let bdd = b.bddFromSetString set
            b.mkLoop (b.one bdd, node.M, node.N) :: acc
        | RegexNodeKind.Empty -> acc
        | RegexNodeKind.PositiveLookaround ->
            RegexNode.LookAround(
                b.mkConcat (convertChildren node),
                lookBack = node.Options.HasFlag(RegexOptions.RightToLeft),
                negate = false
            )
            :: acc
        | RegexNodeKind.NegativeLookaround ->
            RegexNode.LookAround(
                b.mkConcat (convertChildren node),
                lookBack = node.Options.HasFlag(RegexOptions.RightToLeft),
                negate = true
            )
            :: acc
        | other -> failwith $"RegexNodeKind conversion not implemented: {other}, \n{rootNode}"

    let result = loop [] rootNode

    result |> List.rev |> b.mkConcat
