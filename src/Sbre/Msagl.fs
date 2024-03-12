module Msagl
open Sbre
open Sbre.Types

type MsaglGraphNode = {
    id: string
    label: string
    shape: string
}

type MsaglGraphEdge = {
    source: string
    target: string
    color: string
}

type MsaglGraph = { nodes: MsaglGraphNode[]; edges: MsaglGraphEdge[] }

let mkGraph(node: RegexNode<'t>) =
    let rec loop (nodeIds: Set<string>) (edges: Set<string * string>) (node: Types.RegexNode<_>) =
        let current = node.ToString()
        let nodeIds' = nodeIds |> Set.add (current)

        match node with
        | RegexNode.Concat(head, tail, info) ->
            let shead = head.ToString()
            let stail = tail.ToString()
            let n1, e1 = loop Set.empty Set.empty head
            let n2, e2 = loop Set.empty Set.empty tail

            let edges =
                Set.unionMany [
                    edges |> Set.add (current, shead) |> Set.add (current, stail)
                    e1
                    e2
                ]

            nodeIds' |> Set.union n1 |> Set.union n2, edges
        | RegexNode.Epsilon
        | RegexNode.Begin
        | RegexNode.End
        | RegexNode.Singleton(_) -> nodeIds', edges
        | RegexNode.And(nodes, info)
        | RegexNode.Or(nodes, info) ->
            let connections = nodes |> Seq.map (fun v -> (current, v.ToString())) |> set

            nodes
            |> Seq.fold
                (fun (nodes, edges) v -> loop nodes edges v)
                (nodeIds', edges |> Set.union connections)
        | RegexNode.LookAround(node = node)
        | RegexNode.Not(node = node)
        | RegexNode.Loop(node = node) ->
            let connections = (current, node.ToString())
            loop (nodeIds') (Set.add connections edges) node

    let _nodes, _edges = loop Set.empty Set.empty node

    {
    nodes =
        (_nodes
         |> Seq.map (fun v -> {
             id = v
             label = v
             shape = "box"
         })
         |> Seq.toArray)
    edges =
        (_edges
         |> Seq.map (fun (v1, v2) -> {
             source = v1
             target = v2
             color = null
         }))
        |> Seq.toArray
    }

// Debug.debuggerSolver

// let mk =
//     // @"abc"
//     @"~(.*and.*)&[A-Z][\w-{}\\' ,]+&(?<=or=\{.*).*&(?<=\W).*&.*(?=.*\},)&.*(?=\W)"
//     |> mkGraph
//     |> Json.serialize (ignoreNulls=true)
//     |> File.writeTo "/home/ian/f/ieviev/sbre-wasm/src/Sbre.Visualization/assets/g.json"

