#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open Sbre
open Sbre.Types
open Sbre.Optimizations


type MsaglGraphNode = {
    id: obj
    label: string
    shape: string
}

type MsaglGraphEdge = {
    id: obj
    source: obj
    target: obj
    labelText: string
}

type MsaglGraph = { nodes: MsaglGraphNode[]; edges: MsaglGraphEdge[] }

let serialize (g:MsaglGraph) = 
    [|
        "digraph G {"
        for n in g.nodes do 
            $"{n.id} [label=\"{n.label}\"]" 
        for e in g.edges do
            $"{e.source} -> {e.target} [ label=\"{e.labelText}\"]"
        "}"
    |]
    |> String.concat "\n"


// let mkGraph(pattern: string) =
//     let node = Regex(pattern).TSetMatcher.RawPattern

//     let rec loop (nodeIds: Set<string>) (edges: Set<string * string>) (node: Types.RegexNode<_>) =
//         let current = node.ToString()
//         let nodeIds' = nodeIds |> Set.add (current)

//         match node with
//         | RegexNode.Concat(head, tail, info) ->
//             let shead = head.ToString()
//             let stail = tail.ToString()
//             let n1, e1 = loop Set.empty Set.empty head
//             let n2, e2 = loop Set.empty Set.empty tail

//             let edges =
//                 Set.unionMany [
//                     edges |> Set.add (current, shead) |> Set.add (current, stail)
//                     e1
//                     e2
//                 ]

//             nodeIds' |> Set.union n1 |> Set.union n2, edges
//         | RegexNode.Epsilon
//         | RegexNode.Begin
//         | RegexNode.End
//         | RegexNode.Singleton(_) -> nodeIds', edges
//         | RegexNode.And(nodes, info)
//         | RegexNode.Or(nodes, info) ->
//             let connections = nodes |> Seq.map (fun v -> (current, v.ToString())) |> set

//             nodes
//             |> Seq.fold
//                 (fun (nodes, edges) v -> loop nodes edges v)
//                 (nodeIds', edges |> Set.union connections)
//         | RegexNode.LookAround(node = node)
//         | RegexNode.Not(node = node)
//         | RegexNode.Loop(node = node) ->
//             let connections = (current, node.ToString())
//             loop (nodeIds') (Set.add connections edges) node

//     let _nodes, _edges = loop Set.empty Set.empty node

//     {
//         nodes =
//             (_nodes
//              |> Seq.map (fun v -> {
//                  id = v
//                  label = v
//                  shape = "box"
//                  color = "green"
//              })
//              |> Seq.toArray)
//         edges =
//             (_edges
//              |> Seq.map (fun (v1, v2) -> {
//                  source = v1
//                  target = v2
//                  labelText = ""
//                  color = null
//              }))
//             |> Seq.toArray
//     }




open Sbre.Pat
open System.Text.RuntimeRegexCopy.Symbolic

fsi.PrintWidth <- 2000
fsi.AddPrinter<RegexNode<uint64>>(fun v -> v.ToString())

let mkder(m: RegexMatcher<_>) =
    (fun (mt, node) ->
        let loc = Location.getNonInitial ()
        m.CreateDerivative(&loc, mt, node)
    )

// let printTransitions (regex:Sbre.Regex) node =
//     let pretty derivs =
//         derivs
//         |> (Array.map (fun (mt, node) -> $"{regex.TSetMatcher.Cache.PrettyPrintMinterm(mt), -13} ==> {node.ToString()}"))
//         |> String.concat "\n"
//         |> (fun v -> "\n" + v)

//     let possibleTransitions =
//         node |> Optimizations.getImmediateDerivativesMerged (mkder regex.TSetMatcher) regex.TSetMatcher.Cache |> Seq.toArray
//     pretty possibleTransitions


let getMovesFrom (regexEngine: Sbre.Regex) (node: RegexNode<_>) =
    let possibleTransitions =
        node
        |> Optimizations.getImmediateDerivativesMerged
            (mkder regexEngine.TSetMatcher)
            regexEngine.TSetMatcher.Cache
        |> Seq.toArray

    let charsetSolver = System.Text.RuntimeRegexCopy.Symbolic.CharSetSolver()

    let charClassToBdd(charClass) =
        regexEngine.TSetMatcher.Cache.Solver.ConvertToBDD(charClass, charsetSolver)

    possibleTransitions
    |> Seq.map (fun (charClass, transition) ->
        let charString = regexEngine.TSetMatcher.Cache.PrettyPrintMinterm(charClass)
        charString, transition
    )
    |> Seq.toArray

let createFullAutomaton (r:Sbre.Regex) (startState:RegexNode<uint64>) =
    let visited = System.Collections.Generic.HashSet([startState])
    let edges = System.Collections.Generic.Dictionary()
    let rec loop (node: RegexNode<uint64>) = 
        let moves = getMovesFrom r node
        for (tset,nextState) in moves do 
            if not (refEq r.TSetMatcher.Cache.False nextState) then
                edges.Add((node,tset),nextState)
            if visited.Contains(nextState) then () else
            visited.Add(nextState) |> ignore
            loop nextState
    loop startState

    let escape (str:string) =
        str.Replace(@"\n",@"\𝑛")

    {
        nodes =
            (visited
             |> Seq.map (fun v -> {
                 id = unbox (LanguagePrimitives.PhysicalHash v)
                 label = escape (v.ToString())
                 shape = "box"
             })
             |> Seq.toArray)
        edges =
            (edges
             |> Seq.map (fun (entry) -> {
                 id = LanguagePrimitives.PhysicalHash(entry.Key)
                 source = unbox (LanguagePrimitives.PhysicalHash (fst entry.Key))
                 target = unbox (LanguagePrimitives.PhysicalHash (entry.Value))
                 labelText = escape(snd entry.Key)
             }))
            |> Seq.toArray
    }


let r = Sbre.Regex(String.concat "&" [ 
    // "Tom|Sawyer|Huckleberry|Finn" 
    ".{2,4}(Tom|Sawyer|Huckleberry|Finn)" 
    // @"\s[a-zA-Z]{0,12}ing\s" 
])

let fullAutomaton = createFullAutomaton r r.TSetMatcher.RawPattern





fullAutomaton
    |> serialize
    |> File.writeTo "/home/ian/f/ieviev/sbre-visual/src/Client/assets/g.dot"

// mkGraph "abc"
