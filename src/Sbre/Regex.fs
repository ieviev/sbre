namespace Sbre

open System
open System.Globalization
open System.Text.RuntimeRegexCopy.Symbolic
open System.Text.RuntimeRegexCopy
open Sbre.Algorithm
open Sbre.Types
open Sbre.Pat

[<Struct>]
type MatchResult = {
    Success: bool
    Value: string
    Index: int
    Length: int
}

[<Struct>]
type MatchPositionResult = {
    Success: bool
    Index: int
    Length: int
}

[<CLIMutable>]
[<Struct>]
type MatchPosition = { Index: int; Length: int }

[<Sealed>]
type Regex(pattern: string, ?warnUnoptimized:bool) =

    // experimental parser!
    let pattern = pattern.Replace("⊤", @"[\s\S]")

    let regexTree =
        ExtendedRegexParser.Parse(
            pattern,
            RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking,
            CultureInfo.InvariantCulture
        )

    let charsetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()

    // builder from .net runtime
    let bddBuilder = SymbolicRegexBuilder<BDD>(charsetSolver, charsetSolver)

    let converter = RegexNodeConverter(bddBuilder, null)

    // builder from this library
    let bddBuilder2 = RegexBuilder(converter, charsetSolver, charsetSolver)

    let symbolicBddnode: RegexNode<BDD> =
        RegexNodeConverter.convertToSymbolicRegexNode (
            charsetSolver,
            bddBuilder,
            bddBuilder2,
            regexTree.Root
        )
    let implicitTrueStar = bddBuilder2.trueStar

    let trueStarPattern: RegexNode<BDD> =
        match symbolicBddnode with
        | Concat(head, tail, info) -> Concat(implicitTrueStar, Concat(head, tail, info), info)
        | Singleton pred as node ->
            Concat(implicitTrueStar, node, Info.defaultInfo charsetSolver)
        | And(xs, info) as node -> Concat(implicitTrueStar, node, info)
        | Or(xs, info) as node -> Concat(implicitTrueStar, node, info)
        | Loop(xs, low, up, info) as node -> Concat(implicitTrueStar, node, info)
        | LookAround(xs, low, up) as node ->
            Concat(implicitTrueStar, node, Info.defaultInfo charsetSolver)
        | Epsilon -> implicitTrueStar
        | Not(xs, info) as node ->
            let negflags = Info.Flags.inferNode xs
            Concat(implicitTrueStar, node, bddBuilder2.CreateInfo(negflags, charsetSolver.Full))

    let minterms = trueStarPattern |> Minterms.compute bddBuilder

    let solver = UInt64Solver(minterms, charsetSolver)

    let uintbuilder = RegexBuilder(converter, solver, charsetSolver)

#if DEBUG
    do debuggerSolver <- Some solver
#endif
    let trueStarredUint64Node: RegexNode<uint64> =
        (Minterms.transform uintbuilder charsetSolver solver) trueStarPattern

    let rawUint64Node: RegexNode<uint64> =
        (Minterms.transform uintbuilder charsetSolver solver) symbolicBddnode



    let optimizations = RegexFindOptimizations(regexTree.Root, RegexOptions.NonBacktracking)

    let cache =
        Sbre.RegexCache(
            solver,
            charsetSolver,
            _implicitDotstarPattern = trueStarredUint64Node,
            _rawPattern = rawUint64Node,
            _builder = uintbuilder,
            _optimizations = optimizations
        )

    do
        match warnUnoptimized with
        | Some true ->
            if cache.Solver.IsFull(cache.GetInitialStartsetPredicate()) then
                failwith "the pattern has a startset of ⊤, which may result in extremely long match time. specify the beginning of the pattern more"
        | _ -> ()

    let reverseUint64Node = RegexNode.rev cache rawUint64Node

    member this.IsMatch(input: string) =
        let mutable currPos = 0

        let foundStartPos =
            optimizations.TryFindNextStartingPositionLeftToRight(input, &currPos, currPos)
        if not foundStartPos then
            false
        else
            let mutable startLocation = Location.create input currPos
            match RegexNode.matchEnd (cache, &startLocation, ValueNone, trueStarredUint64Node) with
            | ValueNone -> false
            | ValueSome _ -> true

    member internal this.MatchFromLocation(location: byref<Location>) =
        RegexNode.matchEnd (cache, &location, ValueNone, trueStarredUint64Node)

    member internal this.FindMatchEnd(input: string) =
        let mutable currPos = 0

        let foundStartPos =
            optimizations.TryFindNextStartingPositionLeftToRight(input, &currPos, currPos)

        if not foundStartPos then
            match rawUint64Node with
            | Not _ -> // a negation that was not found means its the entire string
                ValueSome input.Length
            | _ -> ValueNone
        else

        let mutable startLocation = Location.create input currPos
        RegexNode.matchEnd (cache, &startLocation, ValueNone, trueStarredUint64Node)


    member this.Match(input: string) : MatchResult =
        let mutable startPos = 0

        let success =
            optimizations.TryFindNextStartingPositionLeftToRight(input.AsSpan(), &startPos, 0)

        if not success then
            {
                Success = false
                Value = ""
                Index = 0
                Length = 0
            }
        else

            let mutable startLocation = Location.create input startPos

            match RegexNode.matchEnd (cache, &startLocation, ValueNone, trueStarredUint64Node) with
            | ValueNone -> {
                Success = false
                Value = ""
                Index = 0
                Length = 0
              }
            | ValueSome endPos ->
                let mutable reverseLocation = (Location.rev { startLocation with Position = endPos })

                let startPos =
                    RegexNode.matchEnd (cache, &reverseLocation, ValueNone, reverseUint64Node)

                match startPos with
                | ValueNone ->
                    failwith
                        $"match succeeded left to right but not right to left:\nmatch end: {endPos}\nreverse pattern: {reverseUint64Node}"
                | ValueSome start ->
                    {
                        Success = true
                        Value = input[start .. endPos - 1]
                        Index = start
                        Length = endPos - start
                    }


    member this.Matches(input: string) =
        this.MatchPositions(input)
        |> Seq.map (fun result ->
            {
                Success = true
                Value = input[result.Index .. result.Index + result.Length]
                Index = result.Index
                Length = result.Length
            }
        )

    member this.MatchText(input: string) =
        let mutable startPos = 0

        let success =
            optimizations.TryFindNextStartingPositionLeftToRight(input.AsSpan(), &startPos, 0)

        if not success then
            None
        else
            let mutable startLocation = Location.create input startPos

            match RegexNode.matchEnd (cache, &startLocation, ValueNone, trueStarredUint64Node) with
            | ValueNone -> None
            | ValueSome endPos ->
                let mutable reverseLocation = (Location.rev { startLocation with Position = endPos })

                let startPos =
                    RegexNode.matchEnd (cache, &reverseLocation, ValueNone, reverseUint64Node)

                match startPos with
                | ValueNone ->
                    failwith
                        $"match succeeded left to right but not right to left:\nmatch end: {endPos}\nreverse pattern: {reverseUint64Node}"
                | ValueSome start -> Some(input[start .. endPos - 1])


    member this.CountMatches(input: string) =
        let mutable currPos = 0
        let mutable location = Location.create input 0
        let mutable reverseLocation = (Location.rev { location with Position = 0 })
        let mutable looping = true
        let mutable counter = 0

        while looping do
            // let jump =
            //     optimizations.TryFindNextStartingPositionLeftToRight(
            //         input.AsSpan(),
            //         &currPos,
            //         currPos
            //     )
            // if not jump then
            //     looping <- false
            // else
                location.Position <- currPos
                match RegexNode.matchEnd (cache, &location, ValueNone, trueStarredUint64Node) with
                | ValueNone -> looping <- false
                | ValueSome(endPos: int) ->
                    counter <- counter + 1
                    // continue
                    if endPos < input.Length then
                        if endPos = currPos then
                            currPos <- currPos + 1
                        else
                            currPos <- endPos
                    else
                        looping <- false

        counter




    member this.MatchPositions(input: string) =
        let mutable currPos = 0
        let mutable location = Location.create input 0
        let mutable reverseLocation = (Location.rev { location with Position = 0 })
        let mutable looping = true
        let initialPrefix = this.Cache.GetInitialStartsetPrefix()

        seq {

            while looping do
                let inputSpan = input.AsSpan()
                match initialPrefix with
                | InitialStartset.Unoptimized ->
                    ()
                | InitialStartset.MintermArrayPrefix(arr, loopEnd) ->
                    let commonStartsetLocation = this.Cache.TryNextStartsetLocationArray(location,arr)
                    match commonStartsetLocation with
                    | ValueNone ->
                        looping <- false
                    | ValueSome newPos ->
                        location.Position <- newPos

                // let success =
                //     optimizations.TryFindNextStartingPositionLeftToRight(
                //         inputSpan,
                //         &currPos,
                //         currPos
                //     )
                //
                // if not success then
                //     looping <- false
                // else
                //     location.Position <- currPos

                match RegexNode.matchEnd (cache, &location, ValueNone, trueStarredUint64Node) with
                | ValueNone -> looping <- false
                | ValueSome(endPos: int) ->
                    reverseLocation.Position <- endPos

                    let startPos =
                        RegexNode.matchEnd (
                            cache,
                            &reverseLocation,
                            ValueNone,
                            reverseUint64Node
                        )

                    match startPos with
                    | ValueNone ->
                        failwith
                            $"match succeeded left to right but not right to left\nthis may occur because of an unimplemented feature\nend-pos:{endPos}, pattern:{reverseUint64Node}"
                    | ValueSome start ->
                        let startIdx = max currPos start
                        let response: MatchPosition = {
                            Index = startIdx
                            Length = (endPos - 1) - startIdx
                        }
                        yield response

                    // continue
                    if endPos < input.Length then
                        if endPos = currPos then
                            currPos <- currPos + 1
                        else
                            currPos <- endPos
                    else
                        looping <- false

        }


    // accessors
    member this.ImplicitPattern: RegexNode<uint64> = trueStarredUint64Node

    member this.RawPattern: RegexNode<uint64> = rawUint64Node

    member this.ReversePattern: RegexNode<uint64> = reverseUint64Node

    member this.Cache: RegexCache<uint64> = cache


#if DEBUG
module Debugging =
    open System.Diagnostics

    [<DebuggerDisplay("{ToString()}")>]

    type ConcatDebugView(concat: FSharp.Collections.List<RegexNode<uint64>>) =
        override this.ToString() =
            match concat with
            | [] -> "[]"
            | head :: _ -> $"{head.TagName()}:{head.ToStringHelper()}"

        [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
        member val Nodes = concat |> Seq.map (fun v -> v.TagName(), v) |> Seq.toArray

    [<assembly: DebuggerDisplay("{Head}, tail:{Tail.Length}",
                                Target = typeof<RegexNode<uint64> list>)>]

    do ()

#endif
