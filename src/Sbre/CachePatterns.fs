namespace Sbre

open System
open System.Buffers
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices
open Sbre.Types
open Sbre.Common
open Sbre.Patterns
open Sbre.Info
open System.Linq


module Cache =

    // --> cache patterns
    [<return: Struct>]
    let inline (|IsImplicitDotStarred|_|) (c: RegexCache< ^t >) (node: RegexNode< ^t > list) =
        if c.IsImplicitDotStarred(node) then
            ValueSome()
        else
            ValueNone

    [<return: Struct>]
    let inline (|IsAnyStar|_|) (c: RegexCache< ^t >) (node: RegexNode< ^t >) =
        if c.IsAnyStar node then
            ValueSome()
        else
            ValueNone

    [<return: Struct>]
    let inline (|IsAnyStarList|_|) (c: RegexCache< ^t >) (node: RegexNode< ^t > list) =
        match node with
        | [ IsAnyStar c _ ] -> ValueSome()
        | _ -> ValueNone



    [<return: Struct>]
    let inline (|IsFalse|_|) (c: RegexCache< ^t >) (node: RegexNode< ^t >) =
        if c.IsFalse node then ValueSome() else ValueNone

    [<return: Struct>]
    let inline (|IsFalseList|_|) (c: RegexCache< ^t >) (node: RegexNode< ^t > list) =
        if c.IsFalseList node then
            ValueSome()
        else
            ValueNone

    [<return: Struct>]
    let inline (|IsNoneOrFalseList|_|) (c: RegexCache< ^t >) (node: RegexNode< ^t > list voption) =
        match node with
        | ValueNone -> ValueSome()
        | ValueSome(IsFalseList c) -> ValueSome()
        | _ -> ValueNone

    [<return: Struct>]
    let inline (|IsTrue|_|) (c: RegexCache< ^t >) (node: RegexNode< ^t >) =
        if c.IsTrue node then ValueSome() else ValueNone

    [<return: Struct>]
    let inline (|IsAnchor|_|) (_: RegexCache< ^t >) (node: RegexNode< ^t > list) =
        match node with
        | [ LookAround _ ] -> ValueSome()
        | _ -> ValueNone


    [<return: Struct>]
    let inline (|IsValidPredicate|_|) (cache: RegexCache< ^t >, location: Location) (node: RegexNode< ^t >) =
        let mterm = cache.MintermForLocation(location)

        match node with
        | Singleton pred -> ValueSome(cache.Solver.isElemOfSet (pred, mterm))
        | _ -> ValueNone


    [<return: Struct>]
    let rec (|CanSkip|_|) (c: RegexCache<'t>) (node: RegexNode<'t> list) =
        match node with
        | [ And(xs, info) ] -> ValueNone // TBD: can be optimized
        | Not(IsAnyStar c :: _, info) :: _ -> ValueSome()
        | IsAnyStar c :: _ -> ValueSome()
        // TBD: could also make a limited length hop
        // for ex. in .*a `.` matches a as well, meaning it can skip
        | Loop(node = [ Singleton pred ]; low = 0; up = Int32.MaxValue) :: Singleton tailPred :: _ when
            c.Solver.isElemOfSet (pred, tailPred)
            ->
            ValueSome()
        // this is to jump in cases like .* followed by \n, which form a full set
        | Loop(node = [ Singleton pred ]; low = 0; up = Int32.MaxValue) :: Singleton tailPred :: _ when
            c.Solver.IsFull(c.Solver.Or(pred, tailPred))
            ->
            ValueSome()
        | _ -> ValueNone

    [<return: Struct>]
    let rec (|AllStartWithTrueStar|_|) (c: RegexCache<'t>) (nodes: seq<RegexNode<'t> list>) =
        if isNull nodes then
            ValueNone
        else
            let mutable failed = false
            let mutable e = nodes.GetEnumerator()
            let mutable sharedStartset = c.Solver.Empty

            while not failed && e.MoveNext() do
                match e.Current with
                | Not(IsAnyStar c :: Singleton pred :: _, info) :: _ ->
                    sharedStartset <- c.Solver.Or(sharedStartset, pred)
                | IsAnyStar c :: Singleton pred :: _ -> sharedStartset <- c.Solver.Or(sharedStartset, pred)
                | Loop(node = [ Singleton pred ]; low = 0; up = Int32.MaxValue) :: Singleton tailPred :: _ when
                    c.Solver.isElemOfSet (pred, tailPred)
                    ->
                    sharedStartset <- c.Solver.Or(sharedStartset, tailPred)
                // this is to jump in cases like .* followed by \n, which form a full set
                | Loop(node = [ Singleton pred ]; low = 0; up = Int32.MaxValue) :: Singleton tailPred :: _ when
                    c.Solver.IsFull(c.Solver.Or(pred, tailPred))
                    ->
                    sharedStartset <- c.Solver.Or(sharedStartset, tailPred)
                // can't eliminate these cases
                // | Or(x1=Singleton _::_)::_ | Or(x2=Singleton _::_)::_ -> failed <- true
                | _ ->
                    let dbg = 1
                    failed <- true

            if failed then
                ValueNone
            else
                ValueSome(sharedStartset)

    // <-- cache patterns


    let inline tryAppend (cache: RegexCache<'t>) (x1: RegexNode<'t> list) (x2: RegexNode<'t> list) =
        match x1 with
        | [] -> x2
        | IsFalseList cache -> x1
        | _ -> (x1 @ x2)


    let mkInfoOfAnd (cache: RegexCache<'t>, xs: seq<RegexNode<'t> list>) =
        let mutable orFlags = RegexNodeFlags.None

        let mutable andFlags =
            RegexNodeFlags.IsAlwaysNullable ||| RegexNodeFlags.CanBeNullable

        let mutable canHopFlags = RegexNodeFlags.CanSkip

        let mutable ss = cache.Solver.Empty

        for x in xs do
            let otherFlags = Info.createNullabilityFlags (x)
            orFlags <- orFlags ||| otherFlags

            if not (canSkip x) then
                removeFlag &canHopFlags (RegexNodeFlags.CanSkip)

            andFlags <- andFlags &&& otherFlags

            ss <- cache.Solver.Or(ss, Info.inferStartset1 (cache.Solver) (x))
            ()

        if orFlags.HasFlag(RegexNodeFlags.ContainsLookaround) then
            removeFlag &andFlags (RegexNodeFlags.IsAlwaysNullable)
        orFlags <- orFlags &&& (RegexNodeFlags.ContainsEpsilon ||| RegexNodeFlags.ContainsLookaround)

        Info.ofFlagsAndStartset (orFlags ||| andFlags ||| canHopFlags, ss)


    let mkInfoOfOr (cache: RegexCache<'t>, xs: NodeSet<'t>) =
        let mutable orFlags = RegexNodeFlags.None

        let mutable canHopFlags = RegexNodeFlags.CanSkip

        let mutable ss = cache.Solver.Empty

        for x in xs do
            let otherFlags = Info.createNullabilityFlags (x)
            orFlags <- orFlags ||| otherFlags

            if not (canSkip x) then
                removeFlag &canHopFlags (RegexNodeFlags.CanSkip)

            ss <- cache.Solver.Or(ss, Info.inferStartset1 (cache.Solver) (x))

        removeFlag &orFlags (RegexNodeFlags.CanSkip)

        Info.ofFlagsAndStartset (orFlags ||| canHopFlags, ss)

    let rec loopSubsumesBranch =
        fun (c:RegexCache<'t>) (pred:'t) (nodes:RegexNode<'t> list) ->
            match nodes with
            | [] -> true
            | Loop([Singleton pred1],min1,max1,info1)::tail ->
                if c.Solver.isElemOfSet(pred,pred1) then
                    loopSubsumesBranch c pred tail
                else false
            | Singleton pred1::tail ->
                if c.Solver.isElemOfSet(pred,pred1) then
                    loopSubsumesBranch c pred tail
                else false
            | _ ->
                // TBD: could be optimized into a data structure
                false
    let tryRewriteOr (cache: RegexCache<'t>, nodes: NodeSet<'t>) =
        // singleton loop subsumption
        let singleLoopOpt =
            nodes
            |> Seq.tryPick (fun v ->
                match v with
                | [Loop([Singleton pred1],0,Int32.MaxValue,info1)] as loop ->
                    Some (loop,pred1)
                | _ -> None
            )
        match singleLoopOpt with
        | Some (loop,loopPred) ->
            let filtered =
                nodes
                |> Set.filter (loopSubsumesBranch cache loopPred >> not)

            if filtered.Count >= nodes.Count - 1 then ValueNone else
            if filtered.Count = 0 then ValueSome (Set.singleton loop) else
            ValueSome (filtered |> Set.add (loop))
        | _ -> ValueNone //tbd more optimizations

    let tryRewriteAnd (cache: RegexCache<'t>, nodes: NodeSet<'t>) =
        // singleton loop subsumption
        let singleLoopOpt =
            nodes
            |> Seq.tryPick (fun v ->
                match v with
                | [Loop([Singleton pred1],0,Int32.MaxValue,info1)] as loop ->
                    Some (loop,pred1)
                | _ -> None
            )
        match singleLoopOpt with
        | Some (loop,loopPred) ->

            let canBeRemoved =
                nodes
                |> Set.exists
                    (fun other ->
                    if obj.ReferenceEquals(loop,other) then false else
                    match other with
                    | [] -> false
                    | head::_ when head = loop.Head -> true
                    | _ -> false
                )

            if canBeRemoved then
                Set.remove loop nodes
            else nodes

        | _ -> nodes


    let mkOrOfSeq (cache: RegexCache<'t>, nodes: NodeSet<'t>) =
        let rewritten =
            match tryRewriteOr (cache, nodes) with
            | ValueSome x -> x
            | ValueNone -> nodes
        // let rewritten = nodes
        if rewritten |> exists cache.IsAnyStarList then
            cache.AnyStarList
        else

            let filtered =
                rewritten
                |> filter (cache.IsFalseList >> not)

            match filtered with
            | _ when filtered.IsEmpty -> cache.FalseList
            | _ when filtered.Count = 1 -> head filtered
            | twoormore ->
                let info = mkInfoOfOr (cache, filtered)
                [ Or(twoormore, info) ]

    let mkOr2 (cache: RegexCache<'t>, x1: RegexNode<'t> list, x2: RegexNode<'t> list) =
        match x1, x2 with
        | IsAnyStarList cache, _
        | _, IsAnyStarList cache -> cache.AnyStarList
        | IsFalseList cache, x
        | x, IsFalseList cache -> x
        | other, []
        | [], other -> [ Or(of2 ([], other), Info.ofEpsilon ()) ]
        | _ when cache.CheckEquality(x1, x2) -> x1
        // loop subsumption
        | [ Loop([ Singleton pred1 ], 0, Int32.MaxValue, info) ],
          [ Loop([ Singleton pred2 ], 0, Int32.MaxValue, info2) ] ->
            if cache.Solver.isElemOfSet (pred1, pred2) then
                x2 // x1 loop subsumes x2
            elif cache.Solver.isElemOfSet (pred2, pred1) then
                x1 // x1 loop subsumes x2
            else
                let info = mkInfoOfOr (cache, of2 (x1, x2))
                [ Or(of2 (x1, x2), info) ]
        | [ Or (xs,xsinfo) as or1 ], [ Or (ys,ysinfo) as or2 ] ->
            mkOrOfSeq(cache, Set.union xs ys)
        // loop subsumption 2
        | ([ Loop([ Singleton pred1 ], 0, Int32.MaxValue, info) ] as loop), other
        | other, ([ Loop([ Singleton pred1 ], 0, Int32.MaxValue, info) ] as loop) ->
            if loopSubsumesBranch cache pred1 other then loop else
            let nodes = of2 (x1, x2)
            [ Or(nodes, mkInfoOfOr (cache, nodes)) ]
        | _ ->
            let nodes = of2 (x1, x2)
            [ Or(nodes, mkInfoOfOr (cache, nodes)) ]

    let mkAndOfSeq
        (cache: RegexCache<'t>)
        (isNullable: RegexNode<'t> list -> bool)
        (nodes: NodeSet<'t>)
        (tail: RegexNode<'t> list)
        : RegexNode<'t> list =
            let filtered =
                tryRewriteAnd (cache, nodes |> filter (cache.IsAnyStarList >> not))
            match filtered with
            | _ when filtered.IsEmpty -> cache.AnyStarList
            | _ when filtered.Count = 1 -> tryAppend cache (head filtered) tail
            | twoormore ->
                let info = mkInfoOfAnd (cache, filtered)
                And(filtered, info) :: tail



    let mkNot (cache: RegexCache<'t>, derivative: RegexNode<'t> list) : RegexNode<'t> list =


        let singletonInfo () =
            let flags =
                RegexNodeFlags.IsAlwaysNullable ||| RegexNodeFlags.CanBeNullable

            { Flags = flags; Startset = cache.Solver.Full }

        // ~(Derx(R))
        match derivative with
        // optional rewrite, needs testing
        // ~(a) = epsilon | [^a] | a.+
        // | [ Singleton tset ] ->
        //     let ornode =
        //         Or(
        //             ofSeq [| derivative @ cache.TruePlusList
        //                      []
        //                      [ Singleton(cache.Solver.Not(tset)) ] |],
        //             singletonInfo ()
        //         )
        //     [ ornode ]
        | [ IsFalse cache ] -> cache.AnyStarList // ~(⊥) -> ⊤*
        | IsAnyStarList cache -> cache.FalseList // ~(⊤*) -> ⊥
        | [] -> cache.True :: cache.AnyStarList // ~(ε) -> ⊤+
        // all non-epsilon zero minimum width nodes resolve to false
        // e.g. ~(_{0,_}) -> ⊥  (negation of any loop with lower bound 0 is false)
        // or containing always nullable nodes is also false
        | ConcatIsAlwaysNullable -> cache.FalseList
        | _ ->
            let dbg = 1
            [ let mutable flags =
                  Info.createNullabilityFlags derivative

              if flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) then
                  Info.removeFlag &flags RegexNodeFlags.CanBeNullable
                  Info.removeFlag &flags RegexNodeFlags.IsAlwaysNullable
              else if not (flags.HasFlag(RegexNodeFlags.CanBeNullable)) then
                  Info.addFlag &flags RegexNodeFlags.CanBeNullable
                  Info.addFlag &flags RegexNodeFlags.IsAlwaysNullable

              let startset =
                  Info.inferStartset1 (cache.Solver) (derivative)

              if Info.canSkip (derivative) then
                  Info.addFlag &flags RegexNodeFlags.CanSkip

              let info = Info.ofFlagsAndStartset (flags, startset)
              Not(derivative, info) ]
