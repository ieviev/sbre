module rec Sbre.RegexNodeConverter

open System
open System.Collections.Generic
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open System.Globalization
open Sbre
open Sbre.Patterns
// 1. convert to symbolic regex node (BDD) [ <-- ]
// 2. add markers and compute minterms
// 3. convert to either BitVector or u64


type BddNodeBuilder(bcss: System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver) =
    let runtimeBuilder =
        SymbolicRegexBuilder<BDD>(bcss, bcss)

    let _singletonLoopCache =
        Dictionary<BDD, RegexNode<BDD>>()

    member this.epsilon = []
    member this.true' = RegexNode.Singleton(bcss.Full)
    member this.false' = RegexNode.Singleton(bcss.Empty)
    member this.one(char: char) = RegexNode.Singleton(bcss.CreateBDDFromChar char)

    member this.notOne(char: char) =
        RegexNode.Singleton(bcss.Not(bcss.CreateBDDFromChar(char)))

    member this.setFromStr(setPattern: string) =
        let tree =
            RegexParser.Parse(setPattern, RegexOptions.ExplicitCapture, CultureInfo.InvariantCulture)

        let converter =
            RegexNodeConverter(runtimeBuilder, null)

        let setStr = tree.Root.Child(0).Str
        let bdd = converter.CreateBDDFromSetString(setStr)
        RegexNode.Singleton(bdd)

    member this.wordCharacter = this.setFromStr @"\w"
    member this.nonWordCharacter = this.setFromStr @"\W"

    member this.setFromNode(node: RegexNode) =
        let converter =
            RegexNodeConverter(runtimeBuilder, null)

        let bdd = converter.CreateBDDFromSetString(node.Str)
        RegexNode.Singleton(bdd)

    member this.createOr(set: NodeSet<BDD>) : RegexNode<BDD> =
        match set with
        | Empty -> failwith "tried to create or with less than 2 nodes"
        | rest->
            let infos =
                [| for n in set do
                       Info.createNullabilityFlags (n) |]

            let mutable mergedInfo = Info.mergeOrFlags (infos)

            if forall Info.canSkip set then
                mergedInfo.Flags <- RegexNodeFlags.CanSkip

            RegexNode.Or(set, mergedInfo)

    member this.createAnd(set: NodeSet<BDD>) : RegexNode<BDD> =
        match set with
        | Empty -> failwith "tried to create or with less than 2 nodes"
        | rest ->
            let infos =
                [| for n in set do
                       Info.createNullabilityFlags (n) |]

            let mutable mergedInfo = Info.mergeAndInfos (infos)

            if forall Info.canSkip set then
                mergedInfo.Flags <- RegexNodeFlags.CanSkip

            RegexNode.And(set, mergedInfo)

    member this.nothing = RegexNode.Singleton(bcss.Empty)
    member this.anyChar = RegexNode.Singleton(bcss.Full)

    member this.anyStar =
        [ RegexNode.Loop(
              [ RegexNode.Singleton(bcss.Full) ],
              low = 0,
              up = Int32.MaxValue,
              info =
                  { Flags =
                      RegexNodeFlags.IsAlwaysNullable
                      ||| RegexNodeFlags.CanBeNullable
                      ||| RegexNodeFlags.CanSkip
                    Startset = bcss.Full }
          ) ]

    member this.loop(body: RegexNode<BDD> list, lower: int, upper: int) =
        match body, (lower, upper) with
        | [ Singleton bdd ], LoopKind LoopKind.Star ->
            // .*, etc is very common, cache it
            match _singletonLoopCache.TryGetValue(bdd) with
            | true, v -> v
            | _ ->
                let loop =
                    RegexNode.Loop(
                        body,
                        lower,
                        upper,
                        info =
                            { Flags =
                                RegexNodeFlags.IsAlwaysNullable
                                ||| RegexNodeFlags.CanBeNullable
                                ||| RegexNodeFlags.CanSkip
                              Startset = bdd }
                    )

                _singletonLoopCache[bdd] <- loop
                loop
        | _, (x, y) when x > 0 ->
            let mutable innerflags =
                Info.createNullabilityFlags (body)

            Info.removeFlag &innerflags RegexNodeFlags.CanSkip
            Info.removeFlag &innerflags RegexNodeFlags.IsAlwaysNullable
            Info.removeFlag &innerflags RegexNodeFlags.CanBeNullable

            let loop =
                RegexNode.Loop(
                    body,
                    lower,
                    upper,
                    info =
                        { Flags = innerflags
                          Startset = Unchecked.defaultof<_> }
                )

            loop

        | _ ->
            let mutable innerflags =
                Info.createNullabilityFlags (body)

            Info.addFlag &innerflags RegexNodeFlags.CanSkip
            Info.addFlag &innerflags RegexNodeFlags.IsAlwaysNullable
            Info.addFlag &innerflags RegexNodeFlags.CanBeNullable

            RegexNode.Loop(
                body,
                lower,
                upper,
                info =
                    { Flags = innerflags
                      Startset = Unchecked.defaultof<_> }
            )

    ///  (?!⊤)  \z anchor as a lookaround
    member this.zAnchor =
        RegexNode.LookAround([ this.true' ], lookBack = false, negate = true)

    /// 3.7: (?=\n|\z) ≡ $
    member this.dollarAnchor =
        let info =
            { Flags = RegexNodeFlags.CanBeNullable //||| RegexNodeFlags.ContainsLookaround
              Startset = Unchecked.defaultof<_>

            }
        //[ this.zAnchor ]
        Or(ofSeq [
            [ this.zAnchor ]

            [RegexNode.LookAround([ this.one '\n' ] ,
                lookBack = false,
                negate = false
            )]

        ], info)


    // for correct .NET semantics
    // https://github.com/dotnet/runtime/blob/1fe9c0bba15e23b65be007ddf38c43d28b2f9dd2/src/libraries/System.Text.RegularExpressions/src/System/Text/RegularExpressions/Symbolic/UnicodeCategoryConditions.cs#L67
    // member this.wordCharForWordBorder = this.setFromStr "[\w\u200C\u200D]"

    /// 3.7: \b ≡ (?<=ψ\w)·(?!ψ\w)|(?<!ψ\w)·(?=ψ\w)
    member this.wordBorder =
        let info =
            { Flags = RegexNodeFlags.CanBeNullable
              //||| RegexNodeFlags.ContainsLookaround
              // ||| RegexNodeFlags.None
              Startset = Unchecked.defaultof<_> }

        let c1 =
            [ RegexNode.LookAround([ this.wordCharacter ], lookBack = true, negate = false) // (?<=ψ\w)
              RegexNode.LookAround([ this.wordCharacter ], lookBack = false, negate = true) ] // (?!ψ\w)

        let c2 =
            // a
            // .*a
            [ RegexNode.LookAround([ this.wordCharacter ], lookBack = true, negate = true) // (?<!ψ\w)
              RegexNode.LookAround([ this.wordCharacter ], lookBack = false, negate = false) ] // (?=ψ\w)

        Or(ofSeq [ c1; c2 ], info)

    member this.nonWordBorder =
        let info =
            { Flags = RegexNodeFlags.CanBeNullable
              Startset = Unchecked.defaultof<_> }

        let c1 =
            [ RegexNode.LookAround([ this.wordCharacter ], lookBack = true, negate = true) // (?<!ψ\w)
              RegexNode.LookAround([ this.wordCharacter ], lookBack = false, negate = true) ] // (?!ψ\w)

        let c2 =
            // a
            // .*a
            [ RegexNode.LookAround([ this.wordCharacter ], lookBack = true, negate = false) // (?<=ψ\w)
              RegexNode.LookAround([ this.wordCharacter ], lookBack = false, negate = false) ] // (?=ψ\w)

        Or(ofSeq [ c1; c2 ], info)




    /// \A ≡ (?<!⊤)
    member this.bigAAnchor =
        RegexNode.LookAround([ this.true' ], lookBack = true, negate = true)

    /// (?<=\A|\A\n) ≡ \a
    member this.aAnchor =
        let info =
            { Flags =
                RegexNodeFlags.CanBeNullable
                // ||| RegexNodeFlags.ContainsLookaround
                ||| RegexNodeFlags.None
              Startset = Unchecked.defaultof<_> }

        RegexNode.LookAround(
            [ Or(ofSeq [[ this.bigAAnchor ]; [ this.bigAAnchor; this.one '\n' ]], info) ],
            lookBack = true,
            negate = true
        )

    /// ^ ≡ \A|(?<=\n)
    member this.caretAnchor =
        let info =
            { Flags =
                RegexNodeFlags.CanBeNullable
                // ||| RegexNodeFlags.ContainsLookaround
                ||| RegexNodeFlags.None
              Startset = Unchecked.defaultof<_> }
        Or(ofSeq [
            [ this.bigAAnchor ];
            [RegexNode.LookAround( [ this.one '\n'  ], lookBack = true, negate = false)]
        ], info)



let children2Seq (node: System.Text.RuntimeRegexCopy.RegexNode) =
    seq {
        for i = 0 to node.ChildCount() - 1 do
            yield node.Child(i)
    }


let convertToSymbolicRegexNode
    (
        css: CharSetSolver,
        bddBuilder: SymbolicRegexBuilder<BDD>,
        rootNode: System.Text.RuntimeRegexCopy.RegexNode
    ) =

    let converter = RegexNodeConverter(bddBuilder, null)
    let n = BddNodeBuilder(css)

    let rec loop (acc: Types.RegexNode<BDD> list) (node: System.Text.RuntimeRegexCopy.RegexNode) =

        let inline convertSingle (node: System.Text.RuntimeRegexCopy.RegexNode) = node |> (loop [])

        let inline convertChildren (node: System.Text.RuntimeRegexCopy.RegexNode) =
            node |> children2Seq |> Seq.collect (loop []) |> Seq.toList



        match node.Kind with
        | RegexNodeKind.One -> n.one node.Ch :: acc
        | RegexNodeKind.Notone -> n.notOne node.Ch :: acc
        | RegexNodeKind.Set -> n.setFromNode node :: acc
        | RegexNodeKind.Multi ->
            let current = node.Str |> Seq.map n.one |> Seq.toList
            current @ acc
        | RegexNodeKind.Lazyloop
        | RegexNodeKind.Loop ->
            let inner = convertChildren node
            let current = n.loop (inner, node.M, node.N)
            current :: acc
        | RegexNodeKind.Alternate ->
            let inner =
                node |> children2Seq |> Seq.map convertSingle |> Seq.toList

            n.createOr (ofSeq inner) :: acc


        | RegexNodeKind.Conjunction ->
            let inner =
                node |> children2Seq |> Seq.map convertSingle |> Seq.toList

            n.createAnd (ofSeq inner) :: acc

        | RegexNodeKind.Concatenate ->
            let inner = convertChildren node
            inner @ acc
        | RegexNodeKind.Capture ->

            if node.N = -1 then
                convertChildren node
            else
                let _ = 1

                if node.Options.HasFlag(RegexOptions.Negated) then
                    let inner = convertChildren node

                    let mutable flags = Info.createNullabilityFlags inner

                    if flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) then
                        Info.removeFlag &flags RegexNodeFlags.CanBeNullable
                        Info.removeFlag &flags RegexNodeFlags.IsAlwaysNullable
                    else if not (flags.HasFlag(RegexNodeFlags.CanBeNullable)) then
                        Info.addFlag &flags RegexNodeFlags.CanBeNullable
                        Info.addFlag &flags RegexNodeFlags.IsAlwaysNullable


                    RegexNode.Not(
                        inner,
                        { Flags = flags
                          Startset = Unchecked.defaultof<_> }
                    )
                    :: acc
                else
                    let inner = convertChildren node
                    inner
        // Specialized loops
        | RegexNodeKind.Oneloop
        | RegexNodeKind.Onelazy
        | RegexNodeKind.Notoneloop
        | RegexNodeKind.Notonelazy ->
            let mutable bdd = css.CreateBDDFromChar(node.Ch)
            // TBD: explore possibilities of rewrites here
            if node.IsNotoneFamily then
                bdd <- css.Not(bdd)

            match node.M, node.N with
            // +
            | 1, Int32.MaxValue ->
                Types.RegexNode.Singleton(bdd)
                :: n.loop ([ Types.RegexNode.Singleton(bdd) ], 0, Int32.MaxValue)
                :: acc
            | _ ->
                n.loop ([ Types.RegexNode.Singleton(bdd) ], node.M, node.N) :: acc

        // anchors
        | RegexNodeKind.Beginning -> n.bigAAnchor :: acc  // TBD:  ^ or \A in multiline
        | RegexNodeKind.EndZ -> n.dollarAnchor :: acc // TBD:  $ or \z in multiline
        | RegexNodeKind.Boundary -> n.wordBorder :: acc
        | RegexNodeKind.NonBoundary -> n.nonWordBorder :: acc
        | RegexNodeKind.Setlazy
        | RegexNodeKind.Setloop ->
            let set = node.Str
            let bdd = converter.CreateBDDFromSetString(set)
            n.loop ([ Singleton(bdd) ], node.M, node.N) :: acc
        | RegexNodeKind.Empty -> acc
        | RegexNodeKind.PositiveLookaround ->
            let inner = convertChildren node

            RegexNode.LookAround(inner, lookBack = node.Options.HasFlag(RegexOptions.RightToLeft), negate = false)
            :: acc
        | RegexNodeKind.NegativeLookaround ->
            let inner = convertChildren node

            RegexNode.LookAround(inner, lookBack = node.Options.HasFlag(RegexOptions.RightToLeft), negate = true)
            :: acc
        | other -> failwith $"RegexNodeKind conversion not implemented: {other}, \n{rootNode}"

    let result = loop [] rootNode
    result
