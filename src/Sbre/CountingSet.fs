module Sbre.CountingSet

open System.Collections.Generic
open Sbre.Pat
open Sbre.Types


[<RequireQualifiedAccess>]
type CounterState =
    | CanIncr
    | CanExit
    | CanIncrExit
    | Fail

// CSA
// 218:5
[<Sealed>]
type CountingSet(lowerbound: int, upperbound: int, numOfMinterms:int) =
    // offset: o, elem N
    member val Offset: int = 0 with get, set
    // queue : l, strictly increasing natural numbers, Sc = o - n
    member val Queue: LinkedList<int> = LinkedList(Seq.singleton 0) with get, set
    member val Transitions: int[] = Array.zeroCreate 1 with get, set
    member private this.Max = this.Offset - this.Queue.First.Value
    member private this.Min = this.Offset - this.Queue.Last.Value
    member private this.TryRemoveFirst() =
        if this.Max > upperbound && not (obj.ReferenceEquals(this.Queue.First.Next,null)) then
            this.Queue.RemoveFirst()
    member this.CanExit() : bool = this.Max > lowerbound
    // member this.CanExitImmediate() : bool = this.Max >= (lowerbound - 1)
    member this.CanIncr() : bool = this.Min < upperbound

    member this.Step(minterm) =
        this.TryRemoveFirst()
        let canIncr = this.CanIncr()
        if canIncr then this.Incr()
    member this.GetState() : CounterState =
        let canIncr = this.CanIncr()
        let canExit = this.CanExit()

        match canExit, canIncr with
        | true, true ->
            CounterState.CanIncrExit
        | true, false ->

            CounterState.CanExit
        | false, true ->
            CounterState.CanIncr
        | false, false ->

            CounterState.Fail

    member this.TryReset() =
        match this.GetState() with
        | CounterState.CanIncr -> ()
        | CounterState.CanExit -> this.Reset()
        | CounterState.CanIncrExit -> ()
        | CounterState.Fail -> this.Reset()

    member this.Incr() =
        this.Offset <- this.Offset + 1
    member this.Incr0() =
        this.Incr()
        this.Queue.AddLast(this.Offset) |> ignore

    member this.AddToQueue() =
        if this.Queue.Last.Value <> this.Offset then
            this.Queue.AddLast(this.Offset) |> ignore
    member this.Reset() =
        this.Offset <- 0
        this.Queue.Clear()
        this.Queue.AddFirst(0) |> ignore


// statefulness
// type RegexState(numOfMinterms:int) =
//     member val ActiveCounters: Dictionary<RegexNode<TSet>,CountingSet> = Dictionary() with get, set
//
//     member this.InitializeCounter(node:RegexNode<TSet>) : CountingSet =
//         let decr x = if x = 0 then 0 else x - 1
//         match node with
//         | Concat(Loop(node=inner;low=low;up=up), tail, info) ->
//             assert (info.NodeFlags.IsCounter)
//             let cs = CountingSet(decr low, up - 1, numOfMinterms)
//             this.ActiveCounters.Add(node,cs)
//             cs
//         | Loop(low=low;up=up;info=info) ->
//             assert (info.NodeFlags.IsCounter)
//             let cs = CountingSet(decr low,up - 1, numOfMinterms)
//             this.ActiveCounters.Add(node,cs)
//             cs
//         | _ ->
//             failwith $"todo: initialize counter: {node.ToString()}"
//
//     member this.GetOrInitializeCounter(node:RegexNode<TSet>) =
//         match this.ActiveCounters.TryGetValue(node) with
//         | true, v -> v
//         | _ -> this.InitializeCounter(node)
//
//
//     member this.EnqueueCounter(node) =
//         match this.ActiveCounters.TryGetValue(node) with
//         | true, v -> v.AddToQueue(); v
//         | _ -> this.InitializeCounter(node)
//
//     member this.RemoveCounter(node) = this.ActiveCounters.Remove(node)
//
//
//     member this.TryGetCounter(node) =
//         match this.ActiveCounters.TryGetValue(node) with
//         | true, v -> ValueSome(v)
//         | _ -> ValueNone
//
//     member this.Counters(node) =
//         this.ActiveCounters.Values
//         |> Seq.toList
//
//     member this.Clear() = this.ActiveCounters.Clear()
//     member this.NumOfMinterms = numOfMinterms


// let rec tryGetCounter (state:RegexState) (minterm:TSet) (node:RegexNode<_>)  =
//     match state.ActiveCounters.TryGetValue(node) with
//     | true, v -> ValueSome v
//     | _ -> ValueNone

// let rec enqueueNewCounter (state:RegexState) (minterm:TSet) (node:RegexNode<_>)  =
//     assert (node.IsCounter)
//     match node with
//     | Concat(Loop(low=low;up=up), tail, info) ->
//         assert (info.NodeFlags.IsCounter)
//         let cs = CountingSet(low - 1, up - 1, state.NumOfMinterms)
//         state.ActiveCounters.Add(node,cs)
//     | Loop(low=low;up=up;info=info) ->
//         assert (info.NodeFlags.IsCounter)
//         let cs = CountingSet(low - 1,up - 1, state.NumOfMinterms)
//         state.ActiveCounters.Add(node,cs)
//     | _ ->
//         failwith $"todo: initialize counter: {node.ToString()}"

// let rec bumpCounters (state:RegexState) (minterm:TSet) (node:RegexNode<_>)  =
//     if not node.HasCounter then () else
//
//     match node with
//     | Concat(Loop(node=inner), tail, info) ->
//         match info.NodeFlags.IsCounter with
//         | true ->
//             match inner with
//             | Singleton pred ->
//                 if Solver.elemOfSet minterm pred then
//                     state.EnqueueCounter(node) |> ignore
//                 else
//                     failwith "ASDASD"
//             | _ -> failwith "non singleton csa-s not supported"
//         | false ->
//             bumpCounters (state) minterm tail
//
//     | Not(inner, info) ->
//         bumpCounters (state) minterm inner
//     | Or(nodes, info) -> nodes |> Seq.iter (bumpCounters (state) minterm)
//     | And(nodes, info) -> nodes |> Seq.iter (bumpCounters (state) minterm)
//     | Loop(inner, low, up, info) ->
//         match info.NodeFlags.IsCounter with
//         | true ->
//             match inner with
//             | Singleton pred ->
//                 if Solver.elemOfSet minterm pred then
//                     state.EnqueueCounter(node) |> ignore
//
//                 else
//                     failwith "DAfsdgfds"
//                     // state.RemoveCounter(node) |> ignore
//             | _ -> failwith "non singleton csa-s not supported"
//
//         | false -> failwith "??"
//     // some other concat
//     | Concat(head, tail, info) ->
//         assert(head.CanBeNullable)
//         assert(tail.HasCounter)
//         bumpCounters (state) minterm tail
//     | _ ->
//         failwith $"todo: traverse AST for counters\n{node.ToString()}"


// let rec enqueueAvailableCounters (state:RegexState) (minterm:TSet) (node:RegexNode<_>)  =
//     if not node.HasCounter then () else
//
//     match node with
//     | Concat(Loop(node=inner), tail, info) ->
//         match info.NodeFlags.IsCounter with
//         | true ->
//             match inner with
//             | Singleton pred ->
//                 if Solver.elemOfSet minterm pred then
//                     state.EnqueueCounter(node)
//             | _ -> failwith "non singleton csa-s not supported"
//         | false ->
//             bumpCounters (state) minterm tail
//
//     | Not(inner, info) ->
//         bumpCounters (state) minterm inner
//     | Or(nodes, info) -> nodes |> Seq.iter (bumpCounters (state) minterm)
//     | And(nodes, info) -> nodes |> Seq.iter (bumpCounters (state) minterm)
//     | Loop(node=inner;info= info) ->
//         match info.NodeFlags.IsCounter with
//         | true ->
//             match inner with
//             | Singleton pred ->
//                 if Solver.elemOfSet minterm pred then
//                     state.EnqueueCounter(node)
//             | _ -> failwith "non singleton csa-s not supported"
//
//         | false -> failwith "??"
//     // some other concat
//     | Concat(head, tail, info) ->
//         assert(head.CanBeNullable)
//         assert(tail.HasCounter)
//         bumpCounters (state) minterm tail
//     | _ ->
//         failwith $"todo: traverse AST for counters\n{node.ToString()}"




// let stepCounters (state:RegexState) (minterm) =
//     let toRemove = ResizeArray()
//     state.ActiveCounters |> Seq.iter (fun v ->
//         match v.Key with
//         | Concat(head=Loop(node=Singleton ph)) | Loop(node=Singleton ph) ->
//             if Solver.elemOfSet ph minterm then
//                 v.Value.Step(minterm)
//             else
//                 toRemove.Add(v.Key)
//         | _ ->
//             failwith "unhandled counter"
//     )
//     for v in toRemove do
//         state.ActiveCounters.Remove(v) |> ignore

// asd