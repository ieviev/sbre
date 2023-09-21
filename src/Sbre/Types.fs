namespace rec Sbre.Types

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy
open System.Text.RuntimeRegexCopy.Symbolic
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open Microsoft.FSharp.Reflection
open Sbre
open System.Diagnostics


/// 2. Preliminaries
/// A location in s is a pair ⟨s, i⟩, where −1 ≤ i ≤ |s |
[<DebuggerDisplay("{DebugDisplay()}")>]
[<Struct>]
type Location = {
    Input : string
    mutable Position : int32
    Reversed : bool
}
#if DEBUG
    with
    member this.DebugDisplay() =
        // display entire input if it is short
        if this.Input.Length < 60 then
            let inserted = this.Input.Insert(this.Position, "|")
            $"%s{this.Input[this.Position].ToString()}, %s{inserted}, %i{this.Position}"
        else
            $"%s{this.Input[this.Position].ToString()}, %i{this.Position}"

#endif

[<Flags>]
type RegexNodeFlags =
    | None =                0uy
    | CanBeNullable =       1uy
    | IsAlwaysNullable =    2uy
    | ContainsLookaround =  4uy
    | ContainsEpsilon =     8uy
    | CanSkip =              16uy


[<Struct>]
type RegexNodeInfo<'tset> = {
    mutable Flags : RegexNodeFlags
    Startset : 'tset
}


// TBD: experimenting with various other sets
type NodeSet<'tset
    when
        'tset :> IComparable<'tset>
    and 'tset :> IEquatable<'tset>
    and 'tset : equality
    and 'tset : comparison> =
        Microsoft.FSharp.Collections.Set<
            RegexNode<'tset> list>

[<DebuggerDisplay("{ToStringHelper()}")>]
// [<ReferenceEquality>]
[<CustomEquality;CustomComparison>]
type RegexNode<'tset
    when
        'tset :> IComparable<'tset>
    and 'tset :> IEquatable<'tset>
    and 'tset : equality
    and 'tset : comparison
    > =
    // ------------
    // Concat is just a linked list of RegexNodes
    // Epsilon is an empty linked list
    | Or of                                 // RE|RE
        xs:NodeSet<'tset>
        * info:RegexNodeInfo<'tset>
    | Singleton of set: 'tset               // 𝜓 predicate
    | Loop of                               // RE{𝑚, 𝑛}
        node:RegexNode<'tset> list * low: int * up: int
        * info:RegexNodeInfo<'tset>
    | And of                                // RE&RE ..
        xs:NodeSet<'tset>
        * info:RegexNodeInfo<'tset>

    | Not of RegexNode<'tset> list          // ~RE
        * info:RegexNodeInfo<'tset>
    | LookAround of node:RegexNode<'tset> list  // anchors
        * lookBack:bool * negate:bool




#if DEBUG
    override this.ToString() : string =
        let display (nodes:RegexNode<_> list) =
            nodes |> List.map (fun v -> v.ToStringHelper()) |> String.concat ""
        match this with
        | Or(xs,_) -> $"Or({xs})"
            // $"Or({display x1}; {display x2}; {xs})"
        | Singleton _ -> this.ToStringHelper()
        | Loop _ -> this.ToStringHelper()
        | And _ -> this.ToStringHelper()
        | Not _ -> this.ToStringHelper()
        | LookAround _ -> this.ToStringHelper()

    member this.TagName() =
        match this with
        | Or _ -> "Or"
        | Singleton _ -> "φ"
        | Loop _ -> "Loop"
        | And _ -> "And"
        | Not _ -> "Not"
        | LookAround _ -> "Look"


    member this.debuggerSolver =
        match Common.debuggerSolver with
        | None -> failwith "debugger solver not initialized"
        | Some solver -> solver

    member this.isFull(node:RegexNode<'t>) =
        match node with
        | Singleton v ->
            match box v with
            | :? System.Text.RuntimeRegexCopy.Symbolic.BDD as v -> v = debugcharSetSolver.Full
            | :? uint64 -> box v = this.debuggerSolver.Full
            | _ -> false
        | _ -> false

    /// used to display the node during debugging
    member this.ToStringHelper() =
        let display (nodes: RegexNode<'tset> seq) = nodes |> Seq.map (fun f -> f.ToStringHelper())
        let asString (nodes: RegexNode<'tset> seq) = nodes |> Seq.map (fun f -> f.ToStringHelper()) |> String.concat ""
        let paren str = $"({str})"

        let tostr (v: 'tset) =
            match debuggerSolver with
            | None -> $"{v}L"
            | Some db ->
                match box v with
                | :? System.Text.RuntimeRegexCopy.Symbolic.BDD as v ->
                    if v = debugcharSetSolver.Full then "⊤"
                    elif debugcharSetSolver.IsEmpty(unbox v) then "⊥"
                    else
                        match debugcharSetSolver.PrettyPrint(v) with
                        | @"[^\n]" -> "."
                        | c when c.Length > 5 -> "φ" // dont expand massive sets
                        | c -> c
                | _ ->
                    if unbox v = db.Full then "⊤"
                    elif db.IsEmpty(unbox v) then "⊥"
                    else
                        match db.PrettyPrint(unbox (box v), debugcharSetSolver) with
                        | @"[^\n]" -> "."
                        | c when c.Length > 5 -> "φ" // dont expand massive sets
                        | c -> c

        match this with
        | Singleton v -> tostr v
        | Or (items,_) ->
            let setItems : string list =
                if not (obj.ReferenceEquals(items,null)) then
                    items |> Seq.map (display >> String.concat "") |> Seq.toList   else []
            let combinedList = setItems
            combinedList
            |> String.concat "|"
            |> paren
        | And (items,_) ->
            let setItems : string list =
                if not (obj.ReferenceEquals(items,null)) then
                    items |> Seq.map (display >> String.concat "") |> Seq.toList   else []
            setItems
            |> String.concat "&"
            |> paren
        | Not (items,info) ->
            let inner =
                items
                |> Seq.map (fun v -> v.ToStringHelper())
                |> String.concat ""
            $"~({inner})"
        | Loop (body, lower, upper, info) ->
            let inner =
                body
                |> Seq.map (fun v -> v.ToStringHelper())
                |> String.concat ""

            let isStar = lower = 0 && upper = Int32.MaxValue

            let inner = $"{inner}"
            let loopCount =
                if isStar then "*"
                elif lower = 1 && upper = Int32.MaxValue then "+"
                elif lower = 0 && upper = 1 then "?"
                else $"{{{lower},{upper}}}"

            match isStar with
            | true -> $"{inner}*"
            | false -> inner + loopCount

        | LookAround(body, lookBack, negate) ->
            let inner =
                body
                |> Seq.map (fun v -> v.ToStringHelper())
                |> String.concat ""
            match lookBack, negate with
            // | true, true when this.isFull body.Head -> "\\A"
            // | false, true when this.isFull body.Head -> "\\z"
            | false, true -> $"(?!{inner})"
            | false, false -> $"(?={inner})"
            | true, true -> $"(?<!{inner})"
            | true, false -> $"(?<={inner})"




#endif


    // proper equality
    member inline this.GetTagId () =
        match this with
        | Or _ -> 0
        | Singleton _ -> 1
        | Loop _ -> 2
        | And _ -> 3
        | Not _ -> 4
        | LookAround _ -> 5


    interface IComparable with
        member this.CompareTo(other) =

             let vother = other  :?> RegexNode<'tset>
             if obj.ReferenceEquals(this,vother) then 0 else
             match this,vother  with
             | Loop (xs,low,up,info1), Loop (ys,low2,up2,info2) ->
                 let x = compare low low2
                 if x <> 0 then x else
                 let x = compare up up2
                 if x <> 0 then x else
                 if obj.ReferenceEquals(xs,ys) then 0 else
                 -1

             | Singleton tset1, Singleton tset2 -> compare tset1 tset2
             | Not (t1,info1), Not (t2,info2) ->
                 let x = compare info1 info2
                 if x <> 0 then x else
                 compare t1 t2
             | Or(xs, info1), Or(ys, info2)
             | And(xs, info1), And(ys, info2) ->
                 if obj.ReferenceEquals(xs,ys) then 0 else
                 let x = compare info1 info2
                 if x <> 0 then x else
                 compare xs ys

             | LookAround(R, lookBack, negate), LookAround(R2, back, negate2) ->
                let x = compare lookBack back
                if x <> 0 then x else
                let x = compare negate negate2
                if x <> 0 then x else
                compare R R2
             | _ -> compare (this.GetTagId()) (vother.GetTagId())

    override this.Equals(other) =
        if obj.ReferenceEquals(this, other) then true else
        let tagcompare = compare (this.GetTagId()) ((other :?> RegexNode<'tset>).GetTagId())
        if tagcompare <> 0 then false else

        match this, (other :?> RegexNode<_>) with
        | Or(xs,info), Or(ys,info2) -> info = info2 && setequals xs ys
        | And(xs,info), And(ys,info2) ->
            if obj.ReferenceEquals(xs,ys) then true else
            info = info2
            && setequals xs ys
        | Not(x1,info), Not(x2,info2) -> x1 = x2
        | Singleton(x1), Singleton(x2) -> x1 = x2
        | Loop(xs,x12,x13,info), Loop(ys,x22,x23,info2) ->
            x12 = x22 && x13 = x23 && xs = ys
        | LookAround(x11,x12,x13), LookAround(x21,x22,x23) -> x12 = x22 && x13 = x23 && x11 = x21
        | _ -> failwith "impossible case"
    override this.GetHashCode() =
        match this with
        | Or(xs, info) -> LanguagePrimitives.GenericLimitedHash 3 xs
        | Singleton foo -> Convert.ToInt32(foo)
        | Loop(node, low, up, info) -> LanguagePrimitives.GenericLimitedHash 3 node
        | And(xs, info) -> LanguagePrimitives.GenericLimitedHash 3 xs
        | Not(xs, info) -> LanguagePrimitives.GenericLimitedHash 3 xs
        | LookAround(node, lookBack, negate) -> LanguagePrimitives.GenericLimitedHash 3 node



[<Flags>]
type StartsetFlags =
    | None = 0uy
    | IsFull = 1uy
    | IsEmpty = 2uy
    | Inverted = 4uy

[<Struct>]
type StartsetChars = {
    Flags : StartsetFlags
    Chars : char[]
}
    with
    static member Of (inverted, startset) = { Flags = inverted; Chars = startset }


[<DebuggerDisplay("{DebugDisplay}")>]
[<CLIMutable>]
[<CustomEquality; NoComparison>]
type ToplevelOR = {
    mutable Node : RegexNode<uint64> list
    mutable LastNullablePos : int
}
    with
    static member Of (node:RegexNode<_> list, hasbeennull) = { Node = node; LastNullablePos = hasbeennull }
    member this.SetNode (node) = this.Node <- node
    member this.SetNullability (node) = this.LastNullablePos <- node

    override this.Equals(other) =
        if obj.ReferenceEquals(this,other) then true else
        this.Node = ((other :?> ToplevelOR).Node)


    override this.GetHashCode() =
        // TBD: this currently always collides,
        // this could have a heuristic
        0

#if DEBUG
    member this.DebugDisplay =
        let currnode = this.Node |> Seq.map (fun v -> v.ToStringHelper()) |> String.concat ""
        $"{this.LastNullablePos}, {currnode}"
#endif



[<AutoOpen>]
module Common =

    //
    let inline head coll = Seq.head coll
    let inline tail coll = List.tail coll
    let inline iter f coll = Seq.iter f coll
    let inline remove f (coll:Set<'t>) = coll.Remove(f)
    let setequals set1 set2 = set1 = set2
    let inline add f (coll:Set<'t>) = coll.Add(f)
    let inline exists f (coll:Set<'t>) = coll |> Set.exists f
    let inline forall f (coll:Set<'t>) = Set.forall f coll
    let inline flatten (coll:Set<'t>) = Seq.collect id coll
    let inline seqforall f (coll) = Seq.forall f coll
    let inline singleton item = Set.singleton item
    let inline of2 (x,y) = Set.ofSeq( seq{yield x;yield y})
    let inline ofSeq coll = Set.ofSeq(coll)
    let inline filter f (coll) = Set.filter f coll
    let inline map f (coll) = Set.map f coll

    [<return: Struct>]
    let inline (|Empty|_|) (node: Set<'tset>) =
        match node.IsEmpty with
        | true -> ValueSome()
        | _ -> ValueNone


    // TBD: other implementations
    // let inline singleton item = ImmutableHashSet.Create(equalityComparer,item=item)
    // let inline of2 (x,y) = ImmutableHashSet.CreateRange(equalityComparer, seq{yield x;yield y})
    // let inline ofSeq coll = ImmutableHashSet.CreateRange(equalityComparer,coll)
    // let inline filter f (coll:ImmutableHashSet<_>) = ImmutableHashSet.CreateRange(equalityComparer,Seq.filter f coll)
    // let inline map f (coll:ImmutableHashSet<'t>) = ImmutableHashSet.CreateRange(equalityComparer,Seq.map f coll)
