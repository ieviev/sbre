module Sbre.Info

open System
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Types
open Patterns


// accessors and common functions
let rec canSkip (node: RegexNode<'t> list) : bool =
    match node with
    | Singleton _::_ -> false
    | Or(info=info)::_ -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
    | And(info=info)::_ -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
    | Loop(low=0;up=Int32.MaxValue)::_ -> true
    | Loop(_)::_ -> false
    | Not (inner,info)::_ -> info.Flags.HasFlag(RegexNodeFlags.CanSkip)
    | LookAround _::_ -> false
    | [] -> false


let inline removeFlag (flags:byref<RegexNodeFlags>) (flagsToRemove:RegexNodeFlags) = flags <- flags &&& ~~~flagsToRemove
let inline addFlag (flags:byref<RegexNodeFlags>) (flagsToAdd:RegexNodeFlags) = flags <- flags ||| flagsToAdd
let inline invertFlag (flags:byref<RegexNodeFlags>) (flagsToInvert:RegexNodeFlags) =
    if flags.HasFlag(flagsToInvert)
    then flags <- flags &&& ~~~flagsToInvert
    else flags <- flags ||| flagsToInvert



[<AutoOpen>]
module Extensions =
    type ISolver<'t> with
        /// si ∈ [[ψ]]
        /// - i.e. location si is elem of Singleton ψ
        /// - (location is smaller than singleton)
        /// - predicate matches location
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.isElemOfSet(predicate: 't, locationMinterm: 't) =
            not (this.IsEmpty(this.And(locationMinterm, predicate)))


// Patterns
[<return:Struct>]
let (|CanNotBeNullable|_|) (x:RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.CanBeNullable) with
    | false -> ValueSome()
    | _ -> ValueNone

[<return:Struct>]
let (|IsAlwaysNullable|_|) (x:RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) with
    | true -> ValueSome()
    | _ -> ValueNone


[<return:Struct>]
let (|NodeIsAlwaysNullable|_|) (x:RegexNode<'t>) =
    match x with
    | Or(xs, IsAlwaysNullable) -> ValueSome()
    | Singleton foo -> ValueNone
    | Loop(node, low, up, IsAlwaysNullable) -> ValueSome()
    | And(xs, IsAlwaysNullable) -> ValueSome()
    | Not(fSharpList, IsAlwaysNullable) -> ValueSome()
    | LookAround(node, lookBack, negate) -> ValueNone
    | _ -> ValueNone

[<return:Struct>]
let (|ConcatIsAlwaysNullable|_|) (nodes:RegexNode<'t> list) =
    nodes |> seqforall (fun v ->
        match v with
        | NodeIsAlwaysNullable -> true
        | _ -> false
    ) |> vopt

[<return:Struct>]
let (|CanBeNullable|_|) (x:RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.CanBeNullable) with
    | true -> ValueSome()
    | _ -> ValueNone

[<return:Struct>]
let (|ContainsLookaround|_|) (x:RegexNodeInfo<'t>) =
    match x.Flags.HasFlag(RegexNodeFlags.ContainsLookaround) with
    | true -> ValueSome()
    | _ -> ValueNone

// //


let rec isAlwaysNullable (node: RegexNode<'t> list) : bool voption =
    let inline Null node = isAlwaysNullable node

    let inline NullTrue node =
        (isAlwaysNullable node |> (fun v -> v.IsSome && v.Value))

    let V x = ValueSome x
    let OR x y = y |> ValueOption.orElse x
    let AND x y = y |> ValueOption.map (fun _ -> x)

    match node with
    | [] -> V true // Nullx () = true
    | head :: tail ->

        match Null tail with
        | ValueNone -> V false
        | ValueSome false -> V false
        | _ ->

            match head with
            | Singleton _ -> V false // Nullx (ψ) = false
            // 3.2 Nullability and anchor-contexts
            | Or(xs,info) -> // Nullx (R) or Nullx (S)
                match info with
                | IsAlwaysNullable -> V true
                | _ -> V false

            | And(xs,info) -> // Nullx (R) and Nullx (S)
                match info with
                | IsAlwaysNullable -> V true
                | _ -> V false
            | Loop(regexNode, lower, _,info) -> // Nullx (R{m, n, _}) = m = 0 or Nullx (R)
                V(lower = 0 || (NullTrue regexNode))
            // (lower = 0 || isNullable' regexNode) && isNullable' tail
            | Not (node,info) -> V(not (NullTrue node)) // not(Nullx (R))
            // 3.7 Lookarounds
            | LookAround _ -> ValueNone

let rec private getStartset(_solver:ISolver<uint64>,node: RegexNode< uint64 > list) =
    let rec loop (pos:int, node:RegexNode< uint64> list) =
        if pos > 3 then _solver.Full
        else
        let current =
            match node with
            | RegexNode.Singleton pred::_ -> pred
            | RegexNode.Or(xs,_)::_ ->
                [yield! xs]
                |> List.fold (fun acc v -> _solver.Or(acc, loop(pos,v))) _solver.Empty
            | RegexNode.Loop(R,low,_,info)::tail ->
                match low with
                | 0 -> _solver.Or(loop(pos,R),loop(pos,tail))
                | _ -> loop(pos,R)
            | RegexNode.And(xs,info)::_ ->
                xs
                |> Solver.mapAnd _solver (fun v -> loop(pos,v) )
                // |> Set.map (fun v -> loop(pos,v))
                // |> Set.fold (fun acc v -> _solver.And(acc, v)) (_solver.And(loop(pos,x1),loop(pos,x2)))
            | RegexNode.Not(inner,info)::tail -> _solver.Or(loop(pos,inner),loop(pos,tail))
            // TBD: return full set for lookbacks now
            | RegexNode.LookAround(lookBack=true)::_ -> _solver.Full
            | RegexNode.LookAround(body,_,negate)::tail ->
            // (?=\d)2
                if not negate then _solver.And(loop(pos,body),loop(pos,tail))
                else _solver.Full
            | [] -> _solver.Empty

        match node with
        | ZeroWidthNode::_ ->
            if _solver.IsFull(current) then current else
            let currset = _solver.Or(current,loop(pos+1,node))
            currset
        | _ ->
            current
    loop (0,node)


let rec inferStartset1List (s:ISolver<'t>) (acc:'t) (nodeLists: NodeSet<'t>)  =
    match nodeLists with
    | Empty -> acc
    | nodeset ->
        let nodes = head nodeset
        if s.IsFull(acc) then acc else
        match nodes with
        | [] -> s.Empty
        | Singleton pred::tail -> inferStartset1List s (s.Or(acc,pred)) (remove nodes nodeset)
        | Loop(inner,low,up,info)::tail ->
            let headpred = inferStartset1List s acc (singleton inner)

            if low = 0 then
                let tailpred = inferStartset1List s acc ((remove nodes nodeset)|> add tail)
                if
                    s.isElemOfSet(headpred,tailpred) then
                    tailpred
                else inferStartset1List s (s.Or(acc,s.Or(headpred,tailpred))) (remove nodes nodeset)
            else inferStartset1List s (s.Or(acc,headpred)) ((remove nodes nodeset))// getHeadPred()
        | Or(xs,info)::tail ->
            // TBD: proper implementation - unoptimized for now
            s.Full

        | Not(inner,info)::tail ->
            inferStartset1List s acc ((remove nodes nodeset)|> add tail |> add inner) //(inner::tail::listTail)
        | LookAround(node=body; lookBack=false)::tail ->
            // TBD: proper implementation - unoptimized for now
            s.Full
        | LookAround(lookBack=true)::tail -> s.Full
        | _ ->
            // TBD: proper implementation - unoptimized for now
            s.Full


let rec inferStartset1(_solver:ISolver<'t>) (nodes: RegexNode<'t> list)  =
    let rec loop (ss) tail =
        match nodes with
        | [] -> _solver.Empty
        | Singleton pred::tail -> pred
        | Loop(loopBody,low,up,info)::tail ->
            let loopBodyStartset = inferStartset1(_solver)(loopBody)
            if low = 0 then
                let tailStartset = inferStartset1(_solver)(tail)
                if _solver.isElemOfSet(loopBodyStartset,tailStartset) then
                    tailStartset
                else _solver.Or(loopBodyStartset,tailStartset)
            else loopBodyStartset
        | Or(xs,info)::tail ->
            let mutable disjointStartset = _solver.Empty
            for x in xs do
                disjointStartset <- _solver.Or(disjointStartset,inferStartset1(_solver)(x))
            if not (info.Flags.HasFlag(RegexNodeFlags.CanBeNullable)) then
                disjointStartset
            else // overestimation
                _solver.Or(disjointStartset,inferStartset1(_solver)(tail))
        | Not(inner,info)::tail ->
            let headPred = inferStartset1(_solver)(inner)
            let tailPred = inferStartset1(_solver)(tail)
            _solver.Or(headPred,tailPred)
        | LookAround(node=body; lookBack=false)::tail ->
            _solver.Or(inferStartset1(_solver)(body),inferStartset1(_solver)(tail))
        | LookAround(lookBack=true)::tail -> _solver.Full
        | _ ->
            // TBD: proper implementation - unoptimized for now
            _solver.Full
    loop _solver.Empty nodes


let rec inferStartset2(s:ISolver<'t>)(nodes: RegexNode<'t> list)  =
    match nodes with
    | [] -> s.Empty
    | Singleton _::tail -> inferStartset1(s)(tail)
    | [ Not(Solver.TrueStar s::Singleton _::tail,info) ] -> inferStartset1(s)(tail)
    | Not(Solver.TrueStar s::Singleton _::innertail,info)::outertail ->
        let ss1 = inferStartset1(s)(innertail)
        let ss2 = inferStartset2(s)(outertail)
        s.Or(ss1,ss2)
    | Loop(inner,low,Int32.MaxValue,info)::Singleton _::tail ->
        let headPred = inferStartset1(s)(inner)
        let tailPred = inferStartset1(s)(tail)
        if low < 2 then
            if
                s.isElemOfSet(headPred,tailPred) then
                s.And(headPred,tailPred)
            else s.Or(headPred,tailPred)
        else headPred
    | Loop(inner,low,Int32.MaxValue,info)::(Or(info=CanNotBeNullable) as orNode)::tail ->
        let headPred = inferStartset1(s)(inner)
        let tailPred = inferStartset1(s)(orNode::tail)
        if low < 2 then
            if
                s.isElemOfSet(headPred,tailPred) then
                // s.And(headPred,tailPred)
                tailPred
            else s.Or(headPred,tailPred)
        else headPred
    | Or(xs=xs;info=info)::tail ->
        if info.Flags.HasFlag(RegexNodeFlags.CanBeNullable) then
            let ss2 = inferStartset2(s)(tail)
            s.Or(info.Startset,ss2)
        else
            xs
            |> Set.map (fun v -> inferStartset2 s v )
            |> Seq.reduce (fun c v -> s.Or(c,v) )
    | Loop([Singleton pred],0,Int32.MaxValue,info)::tail ->
        // invert loop startset
        let innerstartset1 = s.Not(pred)
        let tailstartset = inferStartset2(s)(tail)
        s.Or(innerstartset1,tailstartset)
    | Not(inner,info)::tail ->
        let innerstartset2 = inferStartset2(s)(inner)
        let tailstartset = inferStartset2(s)(tail)
        s.Or(innerstartset2,tailstartset)
    | head::tail ->
        // TBD: proper implementation - unoptimized for now
        s.Full

let rec createNullabilityFlags(nodes: RegexNode<'t> list)  =

    let mutable flags = RegexNodeFlags.None

    match nodes with
    | [] ->
        flags <- flags ||| (RegexNodeFlags.CanBeNullable ||| RegexNodeFlags.IsAlwaysNullable)
    | Singleton _::tail ->
        flags <- flags &&& ~~~RegexNodeFlags.CanBeNullable
        flags <- flags &&& ~~~RegexNodeFlags.IsAlwaysNullable
    | Loop(low=0)::tail ->
        flags <- createNullabilityFlags(tail)
    | Not(inner,info)::tail ->
        let tailFlags = createNullabilityFlags(tail)
        if tailFlags.HasFlag(RegexNodeFlags.ContainsLookaround) then
            flags <- flags ||| RegexNodeFlags.ContainsLookaround
        if not (tailFlags.HasFlag(RegexNodeFlags.CanBeNullable)) then () else

        let innerFlags = createNullabilityFlags(inner)
        // if inner nullable then not nullable
        if innerFlags.HasFlag(RegexNodeFlags.IsAlwaysNullable) then () else
        // conditional nullable
        if innerFlags.HasFlag(RegexNodeFlags.CanBeNullable) then
            flags <- flags ||| RegexNodeFlags.CanBeNullable
        else // not nullable => always nullable
            flags <- flags ||| RegexNodeFlags.IsAlwaysNullable
            flags <- flags ||| RegexNodeFlags.CanBeNullable

        // not nullable
    | Or(_,info)::tail ->
        if not (info.Flags.HasFlag(RegexNodeFlags.CanBeNullable)) then ()
        let tailFlags = createNullabilityFlags(tail)
        if tailFlags.HasFlag(RegexNodeFlags.ContainsLookaround) then
            flags <- flags ||| RegexNodeFlags.ContainsLookaround
        // markIfHasAnchor(&flags,tailFlags)
        if tailFlags.HasFlag(RegexNodeFlags.CanBeNullable) then
            flags <- flags ||| RegexNodeFlags.CanBeNullable
            if tailFlags.HasFlag(RegexNodeFlags.IsAlwaysNullable)
               && info.Flags.HasFlag(RegexNodeFlags.IsAlwaysNullable)
            then flags <- flags ||| RegexNodeFlags.IsAlwaysNullable
        else ()
    | LookAround _::tail ->
        let tailFlags = createNullabilityFlags(tail)
        if tailFlags.HasFlag(RegexNodeFlags.CanBeNullable) then
            flags <- flags ||| RegexNodeFlags.CanBeNullable
        else ()
        flags <- flags ||| RegexNodeFlags.ContainsLookaround
    | Loop _::tail ->
        let tailFlags = createNullabilityFlags(tail)
        if tailFlags.HasFlag(RegexNodeFlags.ContainsLookaround) then
            flags <- flags ||| RegexNodeFlags.ContainsLookaround
        if tailFlags.HasFlag(RegexNodeFlags.ContainsLookaround) then
            flags <- flags ||| RegexNodeFlags.ContainsLookaround
    | And(_,info)::tail ->
        if not (info.Flags.HasFlag(RegexNodeFlags.CanBeNullable)) then ()
        let tailFlags = createNullabilityFlags(tail)
        if tailFlags.HasFlag(RegexNodeFlags.ContainsLookaround) then
            flags <- flags ||| RegexNodeFlags.ContainsLookaround
        // markIfHasAnchor(&flags,tailFlags)
        if tailFlags.HasFlag(RegexNodeFlags.CanBeNullable) then
            flags <- flags ||| RegexNodeFlags.CanBeNullable
            if tailFlags.HasFlag(RegexNodeFlags.IsAlwaysNullable)
               && info.Flags.HasFlag(RegexNodeFlags.IsAlwaysNullable)
            then flags <- flags ||| RegexNodeFlags.IsAlwaysNullable
        else ()

    flags


let mergeOrFlags(infos:RegexNodeFlags[])  =

    let mutable flags = RegexNodeFlags.None


    let anyCanBeNull =
        infos
        |> Array.exists (fun v -> v.HasFlag(RegexNodeFlags.CanBeNullable))

    if anyCanBeNull then
        flags <- flags ||| RegexNodeFlags.CanBeNullable


    let anyAlwaysNull =
        infos
        |> Array.exists (fun v -> v.HasFlag(RegexNodeFlags.IsAlwaysNullable))

    if anyAlwaysNull then
        flags <- flags ||| RegexNodeFlags.IsAlwaysNullable

    if infos |> Array.exists (fun v -> v.HasFlag(RegexNodeFlags.ContainsLookaround)) then
        flags <- flags ||| RegexNodeFlags.ContainsLookaround


    {Flags = flags; Startset = Unchecked.defaultof<_>}


let mergeAndInfos(infos:RegexNodeFlags[])  =
    let mutable flags = RegexNodeFlags.None


    let allCanBeNull =
        infos
        |> Array.forall (fun v -> v.HasFlag(RegexNodeFlags.CanBeNullable))

    if allCanBeNull then
        flags <- flags ||| RegexNodeFlags.CanBeNullable


    let allAlwaysNull =
        infos
        |> Array.forall (fun v -> v.HasFlag(RegexNodeFlags.IsAlwaysNullable))

    if allAlwaysNull then
        flags <- flags ||| RegexNodeFlags.IsAlwaysNullable

    if infos |> Array.exists (fun v -> v.HasFlag(RegexNodeFlags.ContainsLookaround)) then
        flags <- flags ||| RegexNodeFlags.ContainsLookaround

    {Flags = flags; Startset = Unchecked.defaultof<_>}


let ofFlagsAndStartset(flags, ss) = {Flags = flags; Startset = ss}

let ofEpsilon<'t>() = {
    Flags =
        RegexNodeFlags.IsAlwaysNullable
        ||| RegexNodeFlags.CanBeNullable
        ||| RegexNodeFlags.ContainsEpsilon
    Startset = Unchecked.defaultof<'t>
}


let convertFromSetBDD(xs:NodeSet<'t>,solver: ISolver<'t>,info:RegexNodeInfo<BDD>): RegexNodeInfo<'t> =
    let mutable ss = solver.Empty
    if not (isNull xs) then
        for x in xs do
            ss <- solver.Or(ss,inferStartset1(solver)(x))
    {
        Flags = info.Flags
        Startset = ss
    }


let convertFromBDD(newStartset:'t2) (info:RegexNodeInfo<BDD>): RegexNodeInfo<'t2> =
    {
        Flags = info.Flags
        Startset = newStartset
    }



let reverseFromSet(xs:NodeSet<'t>,solver: ISolver<'t>,info:RegexNodeInfo<'t>): RegexNodeInfo<'t> =
    let mutable ss = solver.Empty

    let mutable canHopFlag = RegexNodeFlags.CanSkip

    if not (isNull xs) then
        for x in xs do
            ss <- solver.Or(ss,inferStartset1(solver)(x))
            if not (canSkip x) then
                canHopFlag <- canHopFlag &&& RegexNodeFlags.None

    {
        Flags = info.Flags ||| canHopFlag
        Startset = ss
    }



let ofNegationInner(xs:RegexNode<'t> list,solver: ISolver<'t>,info:RegexNodeInfo<'b>): RegexNodeInfo<'t> =
    let mutable ss = inferStartset1(solver)(xs)

    let canhopflag =
        if not(canSkip xs) then RegexNodeFlags.None else RegexNodeFlags.CanSkip

    {
        Flags = info.Flags ||| canhopflag
        Startset = ss
    }


let convertLoop(xs:RegexNode<'t> list,solver: ISolver<'t>,info:RegexNodeInfo<'b>): RegexNodeInfo<'t> =
    let mutable ss = inferStartset1(solver)(xs)
    {
        Flags = info.Flags
        Startset = ss
    }

let inline updateLoopInfo(solver:ISolver<'t>,xs:RegexNode<'t> list,info:RegexNodeInfo<'t>,newlow:int) =
    if newlow = 0 then
        {
          info with
            Flags =
                info.Flags ||| RegexNodeFlags.IsAlwaysNullable ||| RegexNodeFlags.CanSkip ||| RegexNodeFlags.CanBeNullable
            Startset = solver.Or(inferStartset1 solver (xs), info.Startset)
        }
    else info


[<return: Struct>]
let rec (|DoesNotMatchStartset|_|) (s:ISolver<uint64>) (loc_pred:uint64) (node: RegexNode< uint64 > list) : unit voption =

    let inline tailNoMatch (info:RegexNodeInfo<uint64>,loc_pred:uint64,tail: RegexNode< uint64 > list) =
        if s.isElemOfSet(info.Startset,loc_pred) then ValueNone else
        if info.Flags.HasFlag(RegexNodeFlags.CanBeNullable) then ValueNone else
        match tail with
        | DoesNotMatchStartset s loc_pred _ -> ValueSome()
        | _ -> ValueNone

    match node with
    | [] -> ValueNone
    | Or(info=info)::tail -> tailNoMatch(info,loc_pred,tail)
    | And(info=info)::tail -> tailNoMatch(info,loc_pred,tail)
    | Not(info=info)::tail -> tailNoMatch(info,loc_pred,tail)
    | Loop(info=info)::tail -> tailNoMatch(info,loc_pred,tail)
    | Singleton pred::tail -> if not (s.isElemOfSet(pred,loc_pred)) then ValueSome() else ValueNone
    | LookAround _::_ -> ValueNone





