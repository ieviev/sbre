namespace Sbre

open System
open System.Runtime.CompilerServices
open System.Text.RuntimeRegexCopy.Symbolic

[<Sealed>]
type UInt16Solver(minterms:BDD[], solver:CharSetSolver) =
    let _minterms = minterms
    let _classifier : MintermClassifier = MintermClassifier(minterms, solver)
    let _full =
        if minterms.Length = 16 then UInt16.MaxValue else
            UInt16.MaxValue >>> (16 - minterms.Length)

    interface ISolver<uint16> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.And(set1, set2) = set1 &&& set2

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ConvertFromBDD(set, solver) =
            let partition = _minterms
            let mutable result : uint16 = 0us
            for i = 0 to partition.Length - 1 do
                if not(solver.IsEmpty(solver.And(partition[i],set))) then
                    result <- result ||| uint16 (1 <<< i)
            result
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ConvertToBDD(set, solver) =
            let partition = _minterms
            let mutable result = BDD.False
            if (set <> LanguagePrimitives.GenericZero) then
                for i = 0 to partition.Length - 1 do
                    // include the i'th minterm in the union if the i'th bit is set
                    if ((set &&& uint16 (1 <<< i)) <> LanguagePrimitives.GenericZero) then
                        result <- solver.Or(result, partition[i]);
            result

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.GetMinterms() =
            let minterms = Array.zeroCreate _minterms.Length
            for i = 0 to minterms.Length  - 1 do
                minterms[i] <- uint16 (1 <<< i)
            minterms
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.IsEmpty(set) = set = LanguagePrimitives.GenericZero
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.IsFull(set) = set = _full
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Not(set) = _full &&& (~~~set)
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Or(set1, set2) = set1 ||| set2
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Or(sets) =
            let mutable result = 0us
            for s in sets do
                result <- result ||| s
            result
#if DEBUG
        member this.PrettyPrint(set, solver) =
            solver.PrettyPrint((this :> ISolver<_>).ConvertToBDD(set, solver))
#endif
        member this.Empty = LanguagePrimitives.GenericZero
        member this.Full = _full

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.And(set1, set2) = set1 &&& set2

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ConvertFromBDD(set, solver:CharSetSolver) =
            let partition = _minterms
            let mutable result : uint16 = 0us
            for i = 0 to partition.Length - 1 do
                if not(solver.IsEmpty(solver.And(partition[i],set))) then
                    result <- result ||| uint16 (1 <<< i)
            result
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ConvertToBDD(set:uint16, solver:CharSetSolver) =
            let partition = _minterms
            let mutable result = BDD.False
            if (set <> LanguagePrimitives.GenericZero) then
                for i = 0 to partition.Length - 1 do
                    // include the i'th minterm in the union if the i'th bit is set
                    if ((set &&& uint16 (1 <<< i)) <> LanguagePrimitives.GenericZero) then
                        result <- solver.Or(result, partition[i]);
            result

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.GetMinterms() =
            let minterms = Array.zeroCreate _minterms.Length
            for i = 0 to minterms.Length  - 1 do
                minterms[i] <- uint16 (1 <<< i)
            minterms
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.IsEmpty(set) = set = 0us
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.IsFull(set) = set = _full
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Not(set) = _full &&& (~~~set)
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Or(set1, set2) = set1 ||| set2
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Or(sets) =
            let mutable result = 0us
            for s in sets do
                result <- result ||| s
            result
#if DEBUG
        member this.PrettyPrint(set, solver:CharSetSolver) =
            solver.PrettyPrint((this :> ISolver<_>).ConvertToBDD(set, solver))
#endif
        member this.Empty = 0us
        member this.Full = _full
    member this.Classifier = _classifier


