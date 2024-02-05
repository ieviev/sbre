module rec Sbre.Optimizations

open Sbre.Types
open Sbre.Pat
open System

[<RequireQualifiedAccess>]
type InitialOptimizations =
    | NoOptimizations
    | ReverseSetsPrefix of prefix:Memory<TSet> * transitionNodeId:int
    | PotentialStartPrefix of prefix:Memory<TSet>

type SbreOptimizations =
    | ReverseStringPrefix of string
    | ReverseSetsPrefix of TSet array
    | NoOptimizations

let tryGetReversePrefix (c:RegexCache<TSet>) (node:RegexNode<TSet>) =
    Some NoOptimizations

#if DEBUG
let printPrefixSets (cache:RegexCache<_>) (sets:TSet list) =
    sets
    |> Seq.map cache.PrettyPrintMinterm
    |> String.concat ";"
#endif


let getImmediateDerivatives (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    cache.Minterms()
    |> Array.map (fun minterm ->
        let loc = Pat.Location.getDefault ()
        let state = Sbre.CountingSet.RegexState(cache.NumOfMinterms())
        let der = Algorithm.createDerivative (cache, state, &loc, minterm, node)
        minterm, der
    )
let getImmediateDerivativesMerged (cache: RegexCache<_>) (node: RegexNode<TSet>) =
    cache.Minterms()
    |> Array.map (fun minterm ->
        let loc = Pat.Location.getDefault ()
        let state = Sbre.CountingSet.RegexState(cache.NumOfMinterms())
        let der = Algorithm.createDerivative (cache, state, &loc, minterm, node)
        minterm, der
    )
    |> Seq.groupBy snd
    |> Seq.map (fun (_, group) ->
        group |> Seq.map fst |> Seq.fold (|||) cache.Solver.Empty, group |> Seq.head |> snd
    )

let getNonRedundantDerivatives
    (cache: RegexCache<TSet>)
    (redundantNodes: System.Collections.Generic.HashSet<RegexNode<TSet>>)
    (node: RegexNode<TSet>)
    =
    getImmediateDerivativesMerged cache node
    |> Seq.where (fun (mt, deriv) -> not (redundantNodes.Contains(deriv)))
    |> Seq.toArray

let rec calcPrefixSets (getStateFlags: RegexNode<_> -> RegexStateFlags) (cache: RegexCache<_>) (startNode: RegexNode<_>) =
    let redundant = System.Collections.Generic.HashSet<RegexNode<TSet>>([ cache.False ])
    let rec loop acc node =
        let stateFlags = getStateFlags node
        if stateFlags.CanSkip then acc |> List.rev else
        if not (redundant.Add(node)) then
            []
        else if node.IsAlwaysNullable then
            acc |> List.rev
        else
            let prefix_derivs = getNonRedundantDerivatives cache redundant node
            match prefix_derivs with
            | [| (mt, deriv) |] ->
                let acc' = mt :: acc
                loop acc' deriv
            | _ ->
                // let merged_pred = prefix_derivs |> Seq.map fst |> Seq.fold (|||) cache.Solver.Empty
                prefix_derivs |> Seq.map snd |> Seq.iter (redundant.Add >> ignore)
                acc |> List.rev
    loop [] startNode


let rec calcPotentialMatchStart (getStateFlags: RegexNode<_> -> RegexStateFlags) (cache: RegexCache<_>) (startNode: RegexNode<_>) =
    let redundant = System.Collections.Generic.HashSet<RegexNode<TSet>>([ cache.False ])
    let rec loop acc (nodes:RegexNode<_> list) =
        let stateFlags =
            nodes |> Seq.map getStateFlags
        let shouldExit =
            stateFlags |> Seq.exists (_.CanSkip) || nodes |> Seq.exists (_.IsAlwaysNullable)
        if shouldExit then acc |> List.rev else
        let nonRedundant =
            nodes |> List.where (redundant.Add)
        if nonRedundant.IsEmpty then
            acc |> List.rev
        else
            let prefixDerivsList =
                nonRedundant
                |> List.map (getNonRedundantDerivatives cache redundant)

            let merged_pred =
                prefixDerivsList
                |> Seq.collect id
                |> Seq.map fst
                |> Seq.fold (|||) cache.Solver.Empty

            let remainingNodes =
                prefixDerivsList
                |> Seq.collect id
                |> Seq.map snd
                |> Seq.toList
            let acc' = merged_pred :: acc
            // let dbg = printPrefixSets cache acc'
            loop acc' remainingNodes
    loop [] [startNode]




let rec applyPrefixSets (cache:RegexCache<_>) (node:RegexNode<TSet>) (sets:TSet list) =
    assert (not node.ContainsLookaround)
    match sets with
    | [] -> node
    | head :: tail ->
        let loc = Pat.Location.getDefault ()
        let state = Sbre.CountingSet.RegexState(cache.NumOfMinterms())
        let der = Algorithm.createDerivative (cache, state, &loc, head, node)
        applyPrefixSets cache der tail

let findInitialOptimizations
    (nodeToId:RegexNode<TSet> -> int)
    (nodeToStateFlags:RegexNode<TSet> -> RegexStateFlags)
    (c:RegexCache<TSet>) (node:RegexNode<TSet>) (trueStarredNode:RegexNode<TSet>) =
    if node.ContainsLookaround then InitialOptimizations.NoOptimizations else
    match Optimizations.calcPrefixSets nodeToStateFlags c node with
    | prefix when prefix.Length > 0 ->
        let applied = Optimizations.applyPrefixSets c trueStarredNode prefix
        let mem = Memory(Seq.toArray prefix)
        InitialOptimizations.ReverseSetsPrefix(mem,nodeToId applied)
    | _ ->
        match Optimizations.calcPotentialMatchStart nodeToStateFlags c node with
        | potentialStart when potentialStart.Length > 0 ->
            let mem = Memory(Seq.toArray potentialStart)
            InitialOptimizations.PotentialStartPrefix(mem)
        | _ -> InitialOptimizations.NoOptimizations

let tryJumpToStartset (c:RegexCache<TSet>)(loc:byref<Location>) (node:RegexNode<TSet>) : int32 =
    loc.Position
    // let prefix : InitialStartset<_> =
    //     c.Builder.GetInitializedPrefix(node)
    //
    // match prefix with
    // | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->
    //     let arr = arr.Span
    //     let loopEnd = loopEnd.Span
    //     let commonStartsetLocation =
    //         if refEq c.InitialPatternWithoutDotstar node || loopEnd.Length = 0 then
    //             if not loc.Reversed then
    //                 let chars = c.MintermStartsetChars(arr[0])
    //                 c.TryNextStartsetLocationArray(&loc,arr, chars)
    //                 ValueSome loc.Position
    //             else c.TryNextStartsetLocationArrayReversed(&loc,arr)
    //         elif arr.Length = 1 && loopEnd.Length = 1 then
    //             c.TryNextStartsetLocation(&loc,c.Solver.Or(arr[0],loopEnd[0]))
    //             ValueSome loc.Position
    //         else
    //             c.TryNextStartsetLocationArrayWithLoopTerminator(&loc,arr,loopEnd)
    //     match commonStartsetLocation with
    //     | ValueNone ->
    //         Location.final loc
    //
    //     | ValueSome newPos -> newPos
    // | _ ->
    //     // TODO: does occur
    //     loc.Position

let tryJumpToStartset2 (c:RegexCache<TSet>)(loc:byref<Location>) (node:RegexNode<TSet>) : unit =
    ()
    //
    // match c.Builder.GetInitializedPrefix(node) with
    // | InitialStartset.MintermArrayPrefix(arr,loopEnd) ->
    //     let arr = arr.Span
    //     let loopEnd = loopEnd.Span
    //
    //     let commonStartsetLocation =
    //         if loopEnd.Length = 0 then
    //             let chars = c.MintermStartsetChars(arr[0])
    //             c.TryNextStartsetLocationArray(&loc,arr, chars)
    //             ValueSome loc.Position
    //         // elif arr.Length = 1 then
    //         //     let chars = c.MintermStartsetChars(arr[0] ||| loopEnd[0])
    //         //     c.SkipIndexOfAny(&loc,chars)
    //         //     ValueSome loc.Position
    //         else c.TryNextStartsetLocationArrayWithLoopTerminator(&loc,arr,loopEnd)
    //     match commonStartsetLocation with
    //     | ValueNone ->
    //         loc.Position <- Location.final loc
    //     | ValueSome newPos -> loc.Position <- newPos
    // | _ -> ()
    //
