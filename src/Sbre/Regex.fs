module rec Sbre.Regex

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core.CompilerServices
open Sbre.Info
open Sbre.Optimizations
open Sbre.Patterns
open Sbre.Types
open Sbre.Cache
module P = Sbre.Patterns
module C = Sbre.Cache


module Helpers =
    let isNullablePosition(c: RegexCache< uint64 >,loc:inref<Location>, nodes: HashSet<ToplevelOR>) =
        let mutable e = nodes.GetEnumerator()
        let mutable isNullable = false
        while e.MoveNext() && not isNullable do
            let curr = e.Current
            match Regex.RegexNode.isNullable(c,loc,curr.Node) with
            | true -> isNullable <- true
            | _ -> ()
        isNullable



module RegexNode =

    #if DEBUG
    let display (node: RegexNode<'tset> list) =
        node |> Seq.map (fun v -> v.ToStringHelper()) |> String.concat ""

    let displayIndividual (node: RegexNode<'tset> list) =
        node |> Seq.map (fun v -> v.ToStringHelper()) |> String.concat "\n"

    let displayTopOR (toplevelor: HashSet<ToplevelOR>) =
        toplevelor |> Seq.map (fun v -> display v.Node ) |> String.concat "\n"

    let displayTopORIndividual (toplevelor: HashSet<ToplevelOR>) =
        toplevelor |> Seq.map (fun v -> displayIndividual v.Node ) |> String.concat "\n"

    #endif

    // 3.6 Reversal

    // Concat is done to a list
    //  (R·S)r = Sr·Rr
    // important identities are preserved with cache
    let rec rev (cache:RegexCache<'tset>) (node: RegexNode<'tset>) =
        let inline revList (nodes: RegexNode<'tset> list) =
            match nodes with
            | Cache.IsAnyStarList cache
            | Cache.IsFalseList cache -> nodes
            | [ _ ] -> List.map (rev cache) nodes
            | _ -> nodes |> List.map (rev cache) |> List.rev

        match node with
        | Cache.IsTrue cache
        | Cache.IsFalse cache
        | Cache.IsAnyStar cache -> node
        | _ ->

        match node with
        | Singleton _ -> node // ψr = ψ

        // // (R|S)r = Rr|Sr
        | Or(xs,info) ->
            let xs' =
                if isNull xs then Unchecked.defaultof<_> else
                xs |> map revList
            let info' = Info.reverseFromSet(xs',cache.Solver,info)
            Or(xs',info')
        // R{m, n, b}r = Rr{m, n, b}
        | Loop(xs, low, up,info) ->
            let xs' = revList xs
            let info' = Info.ofNegationInner(xs',cache.Solver,info)
            Loop(xs', low, up, info')
        // B: EXTENDED REGEXES
        // (R & S)r = Rr & S r
        | And(xs,info) ->
            let xs' =
                if isNull xs then Unchecked.defaultof<_> else
                xs |> map revList
            let info' = Info.reverseFromSet(xs',cache.Solver,info)
            And(xs',info')
        // (~R)r = ~(Rr)
        | Not(xs, info) ->
            let xs' = revList xs
            let info' = Info.ofNegationInner(xs',cache.Solver,info)
            Not(xs',info')
        // 3.7 Lookarounds
        // (?=R)r = (?<=Rr)
        | LookAround(node = node'; lookBack = false; negate = false) ->
            LookAround(revList node', lookBack = true, negate = false)
        // (?<=R)r = (?=Rr)
        | LookAround(node = node'; lookBack = true; negate = false) ->
            LookAround(revList node', lookBack = false, negate = false)
        // (?!R)r = (?<!Rr)
        | LookAround(node = node'; lookBack = false; negate = true) ->
            LookAround(revList node', lookBack = true, negate = true)
        // (?<!R)r = (?!Rr)
        | LookAround(node = node'; lookBack = true; negate = true) ->
            LookAround(revList node', lookBack = false, negate = true)



    let rec  isNullableSingle (cache: RegexCache<uint64>, loc: Location, node: RegexNode<uint64>) : bool =
        let inline Null node = isNullable(cache,loc,node)
        let isMatch' loc body =
            match matchEnd(cache, loc, ValueNone, body) with
            | ValueNone -> false
            | ValueSome _ -> true

        // 3.2 Nullability and anchor-contexts
        // epsilon is considered an empty list of nodes
        match node with
        // Nullx (ψ) = false
        | Singleton _ -> false
        // Nullx (R) or Nullx (S)
        | Or(info=Info.CanNotBeNullable) -> false
        | Or(info=Info.IsAlwaysNullable) -> true
        | Or(xs, info) ->
            (if isNull xs then false else exists Null xs)
        // Nullx (R) and Nullx (S)
        | And(info=Info.CanNotBeNullable) -> false
        | And(info=Info.IsAlwaysNullable) -> true
        | And(xs, info) ->
            if isNull xs then true else
            forall Null xs
        // Nullx (R{m, n}) = m = 0 or Nullx (R)
        | Loop (R, low, _,info) -> low = 0 || Null R
        // not(Nullx (R))
        | Not(info=Info.CanNotBeNullable) -> false
        | Not(info=Info.IsAlwaysNullable) -> true
        | Not (node,info) -> not (Null node)
        // 3.7 Lookarounds
        | LookAround (body, lookBack, negate) ->
            match lookBack, negate with
            | false, false -> isMatch' loc body         // Nullx ((?=R)) = IsMatch(x, R)
            | false, true -> not (isMatch' loc body)    // Nullx ((?!R)) = not IsMatch(x, R)
            | true, false ->                            // Nullx ((?<=R)) = IsMatch(xr, Rr)
                isMatch' (Location.rev loc) (List.rev (List.map (RegexNode.rev cache) body))
            | true, true -> // Nullx ((?<!R)) = not IsMatch(x r, Rr)
                let revloc = Location.rev loc
                let revr = (List.rev (List.map (RegexNode.rev cache) body))
                not (isMatch' revloc revr)



    let rec isNullable (cache: RegexCache<uint64>, loc: Location, node: RegexNode<uint64> list) : bool =
        let inline isNullable' node = isNullable(cache,loc,node)
        let isMatch' loc body =
            match matchEnd(cache, loc, ValueNone, body) with
            | ValueNone -> false
            | ValueSome _ -> true
#if DIAGNOSTIC
        // logDiagnostic $"{RegexNode.display nodes}"
#endif
        match node with
        | Singleton _::_ -> false // Nullx (ψ) = false
        | [] -> true // Nullx () = true
        // 3.2 Nullability and anchor-contexts
        | Or(info=Info.CanNotBeNullable)::tail -> false
        | Or(info=Info.IsAlwaysNullable)::tail -> isNullable' tail
        // Nullx (R) or Nullx (S)
        | Or(xs,_)::tail ->
             (not (obj.ReferenceEquals(xs,null)) && exists isNullable' xs)
            && isNullable' tail
        // Nullx (R) and Nullx (S)
        | And(info=Info.CanNotBeNullable)::tail -> false
        | And(info=Info.IsAlwaysNullable)::tail -> isNullable' tail
        | And(xs,_)::tail ->
            let headNullable =
                forall isNullable' xs
            headNullable && isNullable' tail
        | Loop (regexNode, lower, _,info)::tail ->               // Nullx (R{m, n, _}) = m = 0 or Nullx (R)
            (lower = 0 || isNullable' regexNode) && isNullable' tail
        | Not (node,info)::tail ->                          // not(Nullx (R))
            not (isNullable' node) && isNullable' tail
        // 3.7 Lookarounds
        | LookAround (body, lookBack, negate)::tail ->
            let lookaroundIsNullable() =
                match lookBack, negate with
                | false, false -> isMatch' loc body         // Nullx ((?=R)) = IsMatch(x, R)
                | false, true -> not (isMatch' loc body)    // Nullx ((?!R)) = not IsMatch(x, R)
                | true, false ->                            // Nullx ((?<=R)) = IsMatch(xr, Rr)
                    isMatch' (Location.rev loc) (List.rev (List.map (RegexNode.rev cache) body))
                | true, true -> // Nullx ((?<!R)) = not IsMatch(x r, Rr)
                    let revloc = Location.rev loc
                    let revr = (List.rev (List.map (RegexNode.rev cache) body))
                    not (isMatch' revloc revr)
            lookaroundIsNullable() && isNullable' tail



    // max(x,⊥) = max(⊥,x) = x
    let inline maxPos (x: int voption, y: int voption) =
        match y with
        | ValueSome _ -> y
        | ValueNone -> x

    /// 3.3 Derivatives and MatchEnd: Null(fail)x(R) = if Nullx (R) then x else None
    let isNullpos(cache:RegexCache<uint64>,loc:Location,node:RegexNode<uint64> list) =
        match isNullable(cache,loc,node) with
        | true -> ValueSome(loc.Position)
        | false -> ValueNone


    /// uses .NET FindMatchOptimizations to find the next startset
    let inline jumpNextLocation(cache: RegexCache<'t>, loc : byref<Location>) : bool =
        let mutable newPos = loc.Position
        let success =
            cache.Optimizations.TryFindNextStartingPositionLeftToRight(
                loc.Input.AsSpan(),
                &newPos,
                loc.Position
            )
        if success then
            loc.Position <- newPos
            true

        else false


    /// 3.3 Derivatives and MatchEnd: if Final(x) then Nullx (R) else max(Nullx (R), MatchEnd(x+1, Derx (R)))
    let matchEnd
        (cache: RegexCache<uint64>,
        initialLocation: Location,
        initialMax: int voption,
        initialNode: RegexNode<uint64> list)
        : int voption =

        let mutable loc = initialLocation
        let mutable currentMax = initialMax
        // current active branches, without implicit dotstar node
        let mutable toplevelOr = HashSet<ToplevelOR>()
        let pendingRemoval = ResizeArray()
        let pendingAddition = ResizeArray()

        let mutable looping = true
        let mutable foundmatch = false


        // initial node
        let initialIsDotStarred = cache.IsImplicitDotStarred initialNode
        let initialWithoutDotstar =
            if initialIsDotStarred then initialNode.Tail else initialNode
        let initialIsNegation =
            match initialWithoutDotstar with
            | Not(_)::_ -> true
            | _ -> false

        let mutable initialContainsLookarounds =
            let mutable ancs = false
            let e = (initialWithoutDotstar :>seq<_>).GetEnumerator()
            while not ancs && e.MoveNext() do
                match e.Current with
                | Or(info=Info.ContainsLookaround) -> ancs <- true
                | And(info=Info.ContainsLookaround) -> ancs <- true
                | Not(info=Info.ContainsLookaround) -> ancs <- true
                | LookAround(_) -> ancs <- true
                | Loop(node=R) ->
                    for n in R do
                        match n with
                        | Or(info=Info.ContainsLookaround) -> ancs <- true
                        | And(info=Info.ContainsLookaround) -> ancs <- true
                        | Not(info=Info.ContainsLookaround) -> ancs <- true
                        | LookAround(_) -> ancs <- true
                        | _ -> ()
                | _ -> ()
            ancs

        if not initialIsDotStarred then
            let branchNullPos = if isNullable(cache,loc,initialNode) then loc.Position else -1
            toplevelOr.Add(ToplevelOR.Of(initialNode,branchNullPos)) |> ignore

        let inline exitWith (pos: int voption) =
            currentMax <- pos
            looping <- false


        while looping do
            // 3.3 Derivatives and MatchEnd optimizations
            match Location.isFinal loc || foundmatch with
            | true ->
                let isnullable = (fun v -> isNullable(cache,loc,v) )
                match toplevelOr with
                | ToplevelOrNullable isnullable -> exitWith(ValueSome loc.Position)
                | _ -> exitWith(currentMax)

            | false ->

            let locationPredicate = cache.MintermForLocation(loc)

            let createDerivatives() =
                // current active branches
                if toplevelOr.Count > 0 then
                    for curr in toplevelOr do
                        match createDerivative(cache,loc,locationPredicate,curr.Node) with
                        | (Cache.IsFalseList cache as derivative) ->
                            if currentMax.IsSome && curr.LastNullablePos > -1 then
                                // a pattern successfully matched and turned to false,
                                // so we can return match
                                foundmatch <- true
                            else
                                pendingRemoval.Add(curr)
                        | deriv ->
                            let currloc = loc
                            if isNullable(cache,loc,deriv) then
                                curr.LastNullablePos <- loc.Position + 1
                            let inst = ToplevelOR.Of(deriv,curr.LastNullablePos)
                            if toplevelOr.Contains(inst) then
                                pendingRemoval.Add(curr)
                                pendingAddition.Add(inst)
                            else
                                curr.Node <- deriv // mutate directly if non-duplicate

                // create implicit dotstar derivative only if startset matches
                if initialIsDotStarred && cache.IsStartSetPredicate(locationPredicate) then
                    match createDerivative(cache,loc,locationPredicate,initialWithoutDotstar) with
                    | Cache.IsFalseList cache -> ()
                    | deriv ->
                        let branchNullPos = if isNullable(cache,loc,deriv) then loc.Position else -1
                        toplevelOr.Add(ToplevelOR.Of(deriv,branchNullPos)) |> ignore


            if initialIsDotStarred then
                ()

            createDerivatives()

            for node in pendingRemoval do
                toplevelOr.Remove(node) |> ignore
            pendingRemoval.Clear()
            for node in pendingAddition do
                toplevelOr.Add(node) |> ignore
            pendingAddition.Clear()
            // found successful match - exit early
            if foundmatch = true then
                ()
            else

            loc.Position <- Location.nextPosition loc

            #if DIAGNOSTIC
            if loc.Position < 500 then
                let locationstr = if Location.isFinal loc then "" else loc.DebugDisplay()
                logDiagnostic ($"pos:{locationstr}\n{displayTopOR toplevelOr}" )

            #endif

            if toplevelOr.Count = 0 then
                if not initialIsDotStarred then looping <- false
                else
                    let oldLocation = loc.Position
                    // this uses .net optimizations to find the next startset
                    // could be adjusted for & and ~ specifically
                    if jumpNextLocation(cache,&loc) then

                        if oldLocation = loc.Position then
                            if not (cache.IsStartSetPredicate(locationPredicate))
                               // && toplevelOr |> seqforall (Info.canHop)
                            then
                                Optimizations.tryJumpToStartset(cache,&loc,&toplevelOr)
                            // else looping <- false
                    else
                        // .net optimizations did not find a match
                        // but in the case of negation this means we have a match
                        if initialIsNegation then
                            exitWith(ValueSome(Location.endPos loc))
                        else looping <- false
            else

            let currPosNullable = Helpers.isNullablePosition(cache,&loc,toplevelOr)

            if Helpers.isNullablePosition(cache,&loc,toplevelOr) then
                currentMax <- ValueSome(loc.Position)

            if not (cache.IsStartSetPredicate(locationPredicate)) then
                ()
                // if toplevelOr |> seqforall (Info.canHop) then
                Optimizations.tryJumpToStartset(cache,&loc,&toplevelOr)


        currentMax


/// creates derivative without returning initial dot-starred pattern
let rec createDerivative (
     c: RegexCache<uint64>,
     loc: Location,
     loc_pred: uint64,
     nodes: RegexNode<uint64> list)
    : RegexNode<uint64> list =
    let inline Der newNode = createDerivative(c,loc,loc_pred,newNode) //

    match nodes with
    | IsAnyStar c::DoesNotMatchStartset c.Solver loc_pred -> nodes

#if DIAGNOSTIC
    // | _ when (logDiagnostic $"{RegexNode.display nodes}";true ) = false -> failwith "todo"
#endif
    // 3.3: Derx (R) = ⊥ if R ∈ ANC or R = ()
    | [LookAround _] | [] ->  c.FalseList
    // ----------------

    // 3.3: Der s⟨i⟩ (ψ) = if si ∈ [[ψ]] then () else ⊥
    | Singleton pred::tail ->
        if c.IsValidPredicate(pred,loc_pred)
        then tail
        else c.FalseList



    // 3.3: Derx (R{m, n}) =
    // if m=0 or Null ∀(R)=true or Nullx (R)=false
    // then Derx (R)·R{m −1, n −1}
    // else Derx (R·R{m −1, n −1})
    | Loop(R, low, up,info) as head :: tail ->

        let inline decr x = if x = Int32.MaxValue || x = 0 then x else x - 1

        // add tail derivative if concat is nullable
        let inline S'() = Der tail

        // this does case 1 in a single step
        // returns Derx (R)·R{m −1, n −1} :: tail
        let makeR_S newHead =
            match newHead, (decr low, decr up) with
            | IsFalseList c, _ -> newHead
            | [], LoopKind LoopKind.EmptyLoop -> tail           // no remainder, just tail
            | [], LoopKind LoopKind.Single -> tryAppend c R  tail          // single R remaining
            | [], LoopKind LoopKind.Star ->
                if low = 0 then nodes        // returns original pattern
                else Loop(R, 0, up,info) :: tail // turned + to *
            | [], (LoopKind LoopKind.Normal as (newlow,newup)) -> // decrement loop by 1
                Loop(R, newlow, newup,updateLoopInfo(c.Solver,R,info,newlow)) ::tail
            | _, LoopKind LoopKind.EmptyLoop -> tryAppend c newHead tail
            | _, LoopKind LoopKind.Single -> tryAppend c (tryAppend c newHead R) tail
            | _, LoopKind LoopKind.Star ->
                if low = 0 then tryAppend c newHead nodes
                else newHead @ Loop(R, 0, up, info ) :: tail
            | _, (LoopKind LoopKind.Normal as (newlow,newup)) ->
                newHead @ Loop(R, newlow, newup, updateLoopInfo(c.Solver,R,info,newlow)) ::tail
            | _ -> failwith "impossible pattern"

        // this does case 2
        // returns Derx (R·R{m −1, n −1}) :: tail
        let makeCase2(R: RegexNode<uint64> list) =
            let loopremainder =
                match decr low, decr up with
                | LoopKind LoopKind.EmptyLoop -> []
                | LoopKind LoopKind.Single -> R
                | LoopKind LoopKind.Star -> if low = 0 then [head] else [Loop(R, 0, up,info )]
                | LoopKind LoopKind.Normal as (newlow,newup) -> [Loop(R, newlow, newup, updateLoopInfo(c.Solver,R,info,newlow) )]
                | _ -> failwith "impossible pattern"

            tryAppend c (Der(tryAppend c R loopremainder)) tail


        if not tail.IsEmpty && RegexNode.isNullableSingle(c,loc,head) then

            let R_S =
                // case 1 if low = 0 or always nullable, Derx (R)·R{m −1, n −1}
                if low = 0 then
                    (makeR_S(Der R))
                // case 2 otherwise, Derx (R·R{m −1, n −1}) :: tail
                else
                    makeCase2(R)

            let S_ = S'() // alt tail derivative because R is nullable
            mkOr2(c,S_,R_S)


        else
            // always case 1, Derx (R)·R{m −1, n −1}
            let R'S = makeR_S(Der R)


            R'S

    // 3.3: Derx (R | S) = Derx (R) | Derx (S)
    | Or (xs,info) as head::tail ->
        // add concat tail if nullable
        let S'() = Der tail

        let newset = Set.empty

        let inline DerIfNeeded (node) =
            match node with
            | IsAnyStar c::DoesNotMatchStartset c.Solver loc_pred -> node
            | _ -> Der node

        let derivatives =
            seq {
            for x in xs do
                match x with
                | [Or (xs,info)] ->
                    yield! xs |> map DerIfNeeded
                | _ -> yield DerIfNeeded x
            }


        let R'S = tryAppend c (Cache.mkOrOfSeq(c,ofSeq derivatives))  tail

        if
            not tail.IsEmpty
            && info.Flags.HasFlag(RegexNodeFlags.CanBeNullable)
            && (info.Flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) || RegexNode.isNullableSingle(c,loc,head)) then
            let S' = S'()
            Cache.mkOr2(c,S',R'S)

        else
            R'S
    // B: EXTENDED REGEXES
    // Derx (R & S) = Derx (R) & Derx (S)
    | And (xs,info) as head::tail ->
        let inline isNullable node = RegexNode.isNullable(c,loc,node)
        // add concat tail if nullable
        let S'() = Der tail

        let inline DerIfNeeded (node) =
            match node with
            | IsAnyStar c::DoesNotMatchStartset c.Solver loc_pred -> node
            | _ -> Der node

        let derivatives =
            xs
            |> map DerIfNeeded

        if derivatives |> Set.exists (fun v -> c.IsFalseList v) then c.FalseList else


        let R'S = (Cache.mkAndOfSeq c isNullable (derivatives) tail)

        if
            not tail.IsEmpty
            && info.Flags.HasFlag(RegexNodeFlags.CanBeNullable)
            && (info.Flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) || RegexNode.isNullableSingle(c,loc,head)) then
            let S' = S'()
            Cache.mkOr2(c, S', R'S)
        else
            R'S

    | Not (R,info) as head::tail ->

        let S'() = Der tail

        let R'S =
            match Der R with
            | IsFalseList c -> (c.AnyStar :: tail)
            | deriv -> tryAppend (c) (mkNot(c,deriv)) (tail)

        if not tail.IsEmpty && RegexNode.isNullableSingle(c,loc,head) then
            let S' = S'()
            mkOr2(c, S', R'S)
        else
            R'S


    // 3.3: Derx (R·S) = if Nullx (R) then Derx (R)·S|Derx (S) else Derx (R)·S
    // the only way to get over a lookaround is if it's nullable
    | LookAround _ as head::tail ->
        let isnullable = RegexNode.isNullableSingle(c,loc,head)
        if isnullable then Der tail
        else c.FalseList

    | _ :: _ -> failwith $"not implemented derivative for: {nodes}"

