#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open Sbre
open Sbre.Types
open Sbre.Optimizations


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

type MsaglGraph = { 
    nodes: MsaglGraphNode[]; edges: MsaglGraphEdge[] }

let mkGraph(pattern: string) =
    let node = Regex(pattern).TSetMatcher.RawPattern

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


let simpleGraph _nodes _edges = {
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
// @"abc"

// |> File.writeTo "/mnt/g/repos/msagljs/examples/minimal_webgl_renderer/src/g.json"



let outfile = 
    "/home/ian/f/ieviev/sbre-wasm/src/Sbre.Visualization/assets/g.json"

let show nodes edges =
    simpleGraph nodes edges
    |> Json.serialize (ignoreNulls = true)
    |> File.writeTo outfile


let rng = System.Random.Shared


type Props = {
    // m vertices
    vertices: int
    prob: float
    // mean degree
    // c = (2x edges) / verts
}

let a = { vertices = 100; prob = 3.5 }

let nodes = List.init a.vertices (fun v -> $"node {v}")
let edges = int (float a.vertices * a.prob)

let edgesrng = [
    for i = 1 to edges do
        let src = nodes[rng.Next(nodes.Length)]
        let dest = nodes[rng.Next(nodes.Length)]
        src, dest
]

let numOfTriangles = 
    edges

let sample = show nodes edgesrng


// let edgesrng = [
//     for node in nodes do
//         let id1 = rng.Next(nodes.Length)
//         node, nodes[id1]

//         if rng.NextDouble() < a.prob then
//             let targetId = rng.Next(nodes.Length)
//             node, nodes[targetId]
// ]
