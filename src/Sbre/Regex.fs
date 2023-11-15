namespace Sbre

open System
open System.Globalization
open System.Text.RuntimeRegexCopy.Symbolic
open System.Text.RuntimeRegexCopy
open Sbre.Algorithm
open Sbre.Types
open Sbre.Pat
open System.Runtime.InteropServices

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
type Regex(pattern: string, [<Optional; DefaultParameterValue(false)>] warnUnoptimized: bool) =
    let pattern = pattern.Replace("⊤", @"[\s\S]")
    // experimental parser!
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
    let reverseUint64Node = RegexNode.rev uintbuilder rawUint64Node
    let cache =
        Sbre.RegexCache(
            solver,
            charsetSolver,
            _implicitDotstarPattern = trueStarredUint64Node,
            _rawPattern = rawUint64Node,
            _reversePattern = reverseUint64Node,
            _builder = uintbuilder,
            _optimizations = optimizations
        )

    do
        match warnUnoptimized with
        | true ->
            match cache.GetInitialStartsetPrefix() with
            | InitialStartset.MintermArrayPrefix(prefix=prefix) ->
                if prefix.Length = 0 then
                    failwith $"TODO UNOPTIMIZED REGEX {pattern}"

            | _ ->
                match rawUint64Node.TryGetInfo with
                | ValueSome (info) ->
                    if info.ContainsEpsilon then () else
                    if cache.Solver.IsFull(cache.GetInitialStartsetPredicate()) then
                        failwith "the pattern has a startset of ⊤, which may result in extremely long match time. specify the beginning of the pattern more"
                | _ -> ()
        | _ -> ()



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

    /// replace all occurrences in string
    member this.Replace(input: string, replacement: string) =
        let sb = System.Text.StringBuilder(input)
        let mutable offset = 0
        for result in this.MatchPositions(input) do
            let start = offset + result.Index
            sb.Remove(start, result.Length + 1).Insert(start, replacement) |> ignore
            offset <-  replacement.Length - result.Length - 1
        sb.ToString()

    /// return all matches on input
    member this.Matches(input: string) =
        this.MatchPositions(input)
        |> Seq.map (fun result ->
            {
                Success = true
                Value = input[result.Index .. result.Index + result.Length - 1]
                Index = result.Index
                Length = result.Length
            }
        )

    /// used internally
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


    /// counts the number of matches
    member this.Count(input: string) =
        let mutable currPos = 0
        let mutable location = Location.create input 0
        let mutable looping = true
        let mutable counter = 0
        let _cache = this.Cache
        let inputSpan = input.AsSpan()
        let initialPrefix =
            match _cache.GetInitialStartsetPrefix() with
            | InitialStartset.MintermArrayPrefix(arr, loopEnd) ->
                arr
            | _ ->
                [|_cache.Solver.Full|]

        while looping do
            location.Position <- currPos

            // use our prefix optimizations
            match _cache.TryNextStartsetLocationArray(&location,initialPrefix) with
            | ValueNone ->
                looping <- false
            | ValueSome newPos ->
                location.Position <- newPos

            // use .net prefix optimizations
            // if not (optimizations.TryFindNextStartingPositionLeftToRight( inputSpan, &currPos, currPos ))
            // then looping <- false
            // else
            //     location.Position <- currPos

                match RegexNode.matchEnd (cache, &location, ValueNone, trueStarredUint64Node) with
                | ValueNone -> looping <- false
                | ValueSome(endPos: int) ->
                    counter <- counter + 1
                    if endPos < inputSpan.Length then
                        if endPos = currPos then
                            currPos <- currPos + 1
                        else
                            currPos <- endPos
                    else
                        looping <- false

        counter



    /// return just the positions of matches without allocating the result
    member this.MatchPositions(input: string) =
        let mutable currPos = 0
        let mutable location = Location.create input 0
        let mutable reverseLocation = (Location.rev { location with Position = 0 })
        let mutable looping = true
        let mutable _cache = cache
        let initialPrefix =
            match _cache.GetInitialStartsetPrefix() with
            | InitialStartset.MintermArrayPrefix(arr, loopEnd) ->
                arr
            | _ ->
                [|_cache.Solver.Full|]
        seq {

            while looping do
                location.Position <- currPos
                let inputSpan = input.AsSpan()
                //
                match _cache.TryNextStartsetLocationArray(&location,initialPrefix) with
                | ValueNone ->
                    looping <- false
                | ValueSome newPos ->
                    location.Position <- newPos

                // .NET startset search
                // if not (optimizations.TryFindNextStartingPositionLeftToRight( inputSpan, &currPos, currPos ))
                // then looping <- false
                // else location.Position <- currPos

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
                            Length = (endPos) - startIdx
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

