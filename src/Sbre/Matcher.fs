namespace Sbre

open System
open System.Globalization
open System.Text.RuntimeRegexCopy.Symbolic
open System.Text.RuntimeRegexCopy
open Sbre.Minterms
open Sbre.Regex
open Sbre.Types
open Sbre.Patterns


[<CLIMutable>]
type MatchPosition = { startIndex: int; endIndex: int }

[<Sealed>]
type Matcher(pattern: string) =

    // experimental parser!
    let pattern = pattern.Replace("⊤", @"[\s\S]")
    let regexTree =
        ExtendedRegexParser.Parse(
            pattern,
            RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking,
            CultureInfo.InvariantCulture
        )

    let charsetSolver =
        System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()

    // builder from .net runtime
    let bddBuilder =
        SymbolicRegexBuilder<BDD>(charsetSolver, charsetSolver)

    // builder from this library
    let _ =
        RegexNodeConverter.BddNodeBuilder(charsetSolver)

    let symbolicBddnode =
        RegexNodeConverter.convertToSymbolicRegexNode (charsetSolver, bddBuilder, regexTree.Root)

    let implicitAnyStar =
        Loop(
            [ Singleton charsetSolver.Full ],
            0,
            Int32.MaxValue,
            { Flags =
                RegexNodeFlags.IsAlwaysNullable
                ||| RegexNodeFlags.CanBeNullable
                ||| RegexNodeFlags.CanSkip
              Startset = charsetSolver.Full }
        )

    let dotStarredPattern =
        implicitAnyStar :: symbolicBddnode

    let minterms =
        dotStarredPattern |> Minterms.compute bddBuilder

    let solver = UInt64Solver(minterms, charsetSolver)

    let singletonCache =
        SingletonCache(charsetSolver, solver)

#if DEBUG
    do debuggerSolver <- Some solver
#endif
    let dotStarredUint64Node =
        List.map (Minterms.transform singletonCache charsetSolver solver) dotStarredPattern

    let rawUint64Node =
        List.map (Minterms.transform singletonCache charsetSolver solver) symbolicBddnode



    let optimizations =
        RegexFindOptimizations(regexTree.Root, RegexOptions.NonBacktracking)

    let cache =
        Sbre.RegexCache(
            solver,
            charsetSolver,
            _implicitDotstarPattern = dotStarredUint64Node,
            _singletonCache = singletonCache,
            _optimizations = optimizations
        )

    do Optimizations.generateStartsets (cache)

    // init startsets
    do
        cache.InitialStartsetCharsArray <-
            [| for minterm in cache.initialMintermsArray do
                   yield Startsets.createStartsetCharsOfMinterm (cache.Solver, charsetSolver, minterm)

               |]

    let reverseUint64Node =
        List.rev (List.map (RegexNode.rev cache) rawUint64Node)

    member this.IsMatch(input: string) =
        let mutable currPos = 0

        let foundStartPos =
            optimizations.TryFindNextStartingPositionLeftToRight(input, &currPos, currPos)

        if not foundStartPos then
            false
        else
            let startLocation = Location.create input currPos

            match RegexNode.matchEnd (cache, startLocation, ValueNone, dotStarredUint64Node) with
            | ValueNone -> false
            | ValueSome _ -> true

    member this.MatchFromLocation(location: Location) =
        RegexNode.matchEnd (cache, location, ValueNone, dotStarredUint64Node)

    member this.FindMatchEnd(input: string) =
        let mutable currPos = 0

        let foundStartPos =
            optimizations.TryFindNextStartingPositionLeftToRight(input, &currPos, currPos)

        if not foundStartPos then
            match rawUint64Node with
            | [ Not(_) ] -> // a negation that was not found means its the entire string
                ValueSome input.Length
            | _ -> ValueNone
        else
        let startLocation = Location.create input currPos
        RegexNode.matchEnd (cache, startLocation, ValueNone, dotStarredUint64Node)

    member this.Match(input: string) =
        let mutable startPos = 0

        let success =
            optimizations.TryFindNextStartingPositionLeftToRight(input.AsSpan(), &startPos, 0)

        if not success then
            None
        else

            let startLocation = Location.create input startPos
            match RegexNode.matchEnd (cache, startLocation, ValueNone, dotStarredUint64Node) with
            | ValueNone -> None
            | ValueSome endPos ->
                let reverseLocation =
                    (Location.rev { startLocation with Position = endPos })

                let startPos =
                    RegexNode.matchEnd (cache, reverseLocation, ValueNone, reverseUint64Node)

                match startPos with
                | ValueNone ->
                    failwith
                        $"match succeeded left to right but not right to left:\nmatch end: {endPos}\nreverse pattern: {reverseUint64Node}"
                | ValueSome start -> Some(input[start .. endPos - 1])








    member this.MatchWithoutOptimizations(input: string) =
        let mutable startPos = 0

        let startLocation = Location.create input startPos

        match RegexNode.matchEnd (cache, startLocation, ValueNone, dotStarredUint64Node) with
        | ValueNone -> None
        | ValueSome endPos ->
            let reverseLocation =
                (Location.rev { startLocation with Position = endPos })

            let startPos =
                RegexNode.matchEnd (cache, reverseLocation, ValueNone, reverseUint64Node) //(RegexNode.rev dotStarredUint64Node)

            match startPos with
            | ValueNone ->
                failwith
                    $"match succeeded left to right but not right to left:\nmatch end: {endPos}\nreverse pattern: {reverseUint64Node}"
            | ValueSome start -> Some(input[start .. endPos - 1])

    member this.MatchPositions(input: string) =

        let mutable location = Location.create input 0

        let rec loop location =
            seq {
                let initialpos = location.Position //

                match this.MatchFromLocation(location) with
                | ValueNone ->
                    ()
                | ValueSome (endPos:int) ->
                    let reverseLocation =
                        (Location.rev { location with Position = endPos })

                    let startPos =
                        RegexNode.matchEnd (cache, reverseLocation, ValueNone, reverseUint64Node)

                    match startPos with
                    | ValueNone ->
                        failwith "match succeeded left to right but not right to left\nthis may occur because of an unimplemented feature"
                    | ValueSome start ->

                        // initialpos -1 is the end of the previous match, cancel the overlap
                        let response : MatchPosition = { endIndex = endPos - 1; startIndex =  max (initialpos) start }
                        yield response
                        // continue
                    if endPos <> input.Length then
                        if endPos = initialpos then
                            yield! (loop (Location.create input (endPos + 1)))
                        else
                            yield! (loop (Location.create input (endPos)))



            }
        loop location


    // accessors
    member this.DotStarredPattern: RegexNode<uint64> list =
        dotStarredUint64Node

    member this.RawPattern: RegexNode<uint64> list =
        rawUint64Node

    member this.ReversePattern: RegexNode<uint64> list =
        reverseUint64Node

    member this.Cache = cache


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

    [<assembly: DebuggerDisplay("{Head}, tail:{Tail.Length}", Target = typeof<RegexNode<uint64> list>)
    >]

    do ()

#endif
