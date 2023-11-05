namespace Sbre

open System
open System.Text.RuntimeRegexCopy.Symbolic
open Sbre.Pat
open Sbre.Types
open Sbre.Info

module Cache =

    // --> cache patterns
    [<return: Struct>]
    let inline (|IsImplicitDotStarred|_|) (c: RegexCache< uint64 >) (node: RegexNode< _ >) =
        if c.IsImplicitDotStarred(node) then
            ValueSome()
        else
            ValueNone

    [<return: Struct>]
    let inline (|IsTrueStar|_|) (c: RegexCache< uint64 >) (node: RegexNode< _ >) =
        if c.IsTrueStar node then
            ValueSome()
        else
            ValueNone


    [<return: Struct>]
    let inline (|IsFalse|_|) (c: RegexCache< uint64 >) (node: RegexNode< uint64 >) =
        if c.IsFalse node then ValueSome() else ValueNone


    [<return: Struct>]
    let inline (|IsTrue|_|) (c: RegexCache< uint64 >) (node: RegexNode< uint64 >) =
        if c.IsTrue node then ValueSome() else ValueNone

    [<return: Struct>]
    let inline (|IsAnchor|_|) (_: RegexCache< uint64 >) (node: RegexNode< uint64 > list) =
        match node with
        | [ LookAround _ ] -> ValueSome()
        | _ -> ValueNone


    [<return: Struct>]
    let inline (|IsValidPredicate|_|)
        (cache: RegexCache< uint64 >, location: Location)
        (node: RegexNode< uint64 >)
        =
        let mterm = cache.MintermForLocation(location)

        match node with
        | Singleton pred -> ValueSome(cache.Solver.isElemOfSet (pred, mterm))
        | _ -> ValueNone


    let mkNot (cache: RegexCache<uint64>, derivative: RegexNode<uint64>) : RegexNode<uint64> =


        let singletonInfo () =
            let flags =
                RegexNodeFlags.IsAlwaysNullable
                ||| RegexNodeFlags.CanBeNullable

            { Flags = flags; Startset = cache.Solver.Full;  }

        // ~(Derx(R))
        match derivative with
        // optional rewrite, needs testing
        // ~(a) = epsilon | [^a] | a.+
        // | [ Singleton tset ] ->
        //     let ornode =
        //         Or(
        //             ofSeq [| derivative @ cache.TruePlusList
        //                      []
        //                      [ Singleton(cache.Solver.Not(tset)) ] |],
        //             singletonInfo ()
        //         )
        //     [ ornode ]

        | IsFalse cache -> cache.TrueStar // ~(⊥) -> ⊤*
        | IsTrueStar cache -> cache.False // ~(⊤*) -> ⊥
        | Epsilon ->
            cache.Builder.mkConcat2 (
                cache.True , cache.TrueStar
            ) // ~(ε) -> ⊤+
        // all non-epsilon zero minimum width nodes resolve to false
        // e.g. ~(_{0,_}) -> ⊥  (negation of any loop with lower bound 0 is false)
        // or containing always nullable nodes is also false
        | Concat(info=regexNodeInfo) when regexNodeInfo.IsAlwaysNullable -> cache.False
        | _ ->
            let mutable flags = Info.Flags.inferNode derivative

            if flags.HasFlag(RegexNodeFlags.IsAlwaysNullable) then
                Info.removeFlag &flags RegexNodeFlags.CanBeNullable
                Info.removeFlag &flags RegexNodeFlags.IsAlwaysNullable
            else if not (flags.HasFlag(RegexNodeFlags.CanBeNullable)) then
                Info.addFlag &flags RegexNodeFlags.CanBeNullable
                Info.addFlag &flags RegexNodeFlags.IsAlwaysNullable

            let startset = Info.Startset.inferStartset (cache.Solver) (derivative)
            let info = cache.CreateInfo(flags, startset)

            // let info = Info.ofFlagsAndStartset (flags, startset)
            Not(derivative, info)


    // todo : not needed
    [<return: Struct>]
    let inline (|ReturnsInitialDerivative|_|)
        (c: RegexCache<uint64>)
        (loc: Location)
        (loc_pred: uint64)
        (node: RegexNode<uint64>)
        : unit voption =

        // inline
        let inline notMatchInfo
            (
                info: RegexNodeInfo<uint64>,
                loc_pred: uint64
            ) =
            if not (info.Flags.HasFlag(Flag.CanSkip)) then ValueNone else
            if c.Solver.isElemOfSet (info.Startset, loc_pred) then
                ValueNone
            else if info.Flags.HasFlag(RegexNodeFlags.CanBeNullable) then
                ValueNone
            else
                ValueSome()

        match node with
        | IsTrueStar c -> ValueSome()
        | Epsilon -> ValueNone
        | Or(info = info)  ->
            notMatchInfo (info, loc_pred)
        | And(info = info)  -> notMatchInfo (info, loc_pred)
        | Not(info = info)  -> notMatchInfo (info, loc_pred)
        | Loop(info = info)  -> notMatchInfo (info, loc_pred)
        | Singleton pred  -> ValueNone
        | Concat(info = info) -> notMatchInfo (info, loc_pred)
        | LookAround _ -> ValueNone