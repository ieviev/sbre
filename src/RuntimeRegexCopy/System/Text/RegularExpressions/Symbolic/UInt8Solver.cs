// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;
using System.Runtime.CompilerServices;

namespace System.Text.RuntimeRegexCopy.Symbolic
{
    /// <summary>Provides an <see cref = "ISolver{Int8}"/> over bit vectors up to 8 bits in length.</summary>
    public sealed class UInt8Solver : ISolver<byte>
    {
        private readonly BDD[] _minterms;
        public readonly MintermClassifier _classifier;
        public UInt8Solver(BDD[] minterms, CharSetSolver solver)
        {
            Debug.Assert(minterms.Length <= 8);
            _minterms = minterms;
            _classifier = new MintermClassifier(minterms, solver);
            Full = (byte)((byte)minterms.Length == 8 ? byte.MaxValue : byte.MaxValue >> (8 - (byte)minterms.Length));
        }

        public byte Empty => 0;
        public byte Full { get; }

        public bool IsFull(byte set) => (set == Full);
        public bool IsEmpty(byte set) => (set == 0);
        public List<byte> GenerateMinterms(HashSet<byte> constraints) => MintermGenerator<byte>.GenerateMinterms(this, constraints);
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public byte And(byte set1, byte set2) => unchecked((byte)(set1 & set2));

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public byte Not(byte set) => unchecked ((byte)(Full & ~set)); //NOTE: must filter off unused bits

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public byte Or(ReadOnlySpan<byte> sets)
        {
            byte result = 0;
            foreach (var p in sets)
            {
                result |= p;
                if (result == Full)
                {
                    // Short circuit the evaluation once all bits are set, as nothing can change after this point.
                    break;
                }
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public byte Or(byte set1, byte set2) => unchecked((byte) (set1 | set2));
        /// <summary>
        /// Assumes that set is a union of some minterms (or empty).
        /// If null then 0 is returned.
        /// </summary>
        public byte ConvertFromBDD(BDD set, CharSetSolver solver)
        {
            BDD[] partition = _minterms;
            byte result = 0;
            for (int i = 0; i < partition.Length; i++)
            {
                // Set the i'th bit if the i'th minterm is in the set.
                if (!solver.IsEmpty(solver.And(partition[i], set)))
                {
                    result |= (byte)(1 << i);
                }
            }

            return result;
        }

        /// <summary>
        /// Return an array of bitvectors representing each of the minterms.
        /// </summary>
        public byte[] GetMinterms()
        {
            byte[] minterms = new byte[_minterms.Length];
            for (var i = 0; i < minterms.Length; i++)
            {
                minterms[i] = (byte)(1 << i);
            }

            return minterms;
        }
#if DEBUG
        /// <summary>Pretty print the bitvector bv as the character set it represents.</summary>
        public string PrettyPrint(byte bv, CharSetSolver solver) => 
            solver.PrettyPrint(ConvertToBDD(bv, solver));
#endif
        public BDD ConvertToBDD(byte set, CharSetSolver solver)
        {
            BDD[] partition = _minterms;

            // the result will be the union of all minterms in the set
            BDD result = BDD.False;
            if (set != 0)
            {
                for (int i = 0; i < partition.Length; i++)
                {
                    // include the i'th minterm in the union if the i'th bit is set
                    if ((set & (1 << i)) != 0)
                    {
                        result = solver.Or(result, partition[i]);
                    }
                }
            }

            return result;
        }

    }
}