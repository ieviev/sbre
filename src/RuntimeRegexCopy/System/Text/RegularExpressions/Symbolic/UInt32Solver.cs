// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace System.Text.RuntimeRegexCopy.Symbolic
{
    /// <summary>Provides an <see cref = "ISolver{Int8}"/> over bit vectors up to 8 bits in length.</summary>
    public sealed class UInt32Solver : ISolver<uint>
    {
        private readonly BDD[] _minterms;
        public readonly MintermClassifier _classifier;
        public UInt32Solver(BDD[] minterms, CharSetSolver solver)
        {
            Debug.Assert(minterms.Length <= 32);
            _minterms = minterms;
            _classifier = new MintermClassifier(minterms, solver);
            Full = (uint)minterms.Length == 32 ? uint.MaxValue : (uint.MaxValue >> (32 - minterms.Length));
        }

        public uint Empty => 0;
        public uint Full { get; }

        public bool IsFull(uint set) => (set == Full);
        public bool IsEmpty(uint set) => (set == 0);
        public List<uint> GenerateMinterms(HashSet<uint> constraints) => MintermGenerator<uint>.GenerateMinterms(this, constraints);
        public uint And(uint set1, uint set2) => set1 & set2;

        public uint Not(uint set) => Full & ~set; //NOTE: must filter off unused bits

        public uint Or(ReadOnlySpan<uint> sets)
        {
            uint result = 0;
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

        public uint Or(uint set1, uint set2) => set1 | set2;
        /// <summary>
        /// Assumes that set is a union of some minterms (or empty).
        /// If null then 0 is returned.
        /// </summary>
        public uint ConvertFromBDD(BDD set, CharSetSolver solver)
        {
            BDD[] partition = _minterms;
            uint result = 0;
            for (var i = 0; i < partition.Length; i++)
            {
                // Set the i'th bit if the i'th minterm is in the set.
                if (!solver.IsEmpty(solver.And(partition[i], set)))
                {
                    result |= (uint)(1 << i);
                }
            }

            return result;
        }

        /// <summary>
        /// Return an array of bitvectors representing each of the minterms.
        /// </summary>
        public uint[] GetMinterms()
        {
            uint[] minterms = new uint[_minterms.Length];
            for (var i = 0; i < minterms.Length; i++)
            {
                minterms[i] = (uint)(1 << i);
            }

            return minterms;
        }
#if DEBUG
        /// <summary>Pretty print the bitvector bv as the character set it represents.</summary>
        public string PrettyPrint(uint bv, CharSetSolver solver) => 
            solver.PrettyPrint(ConvertToBDD(bv, solver));
#endif
        public BDD ConvertToBDD(uint set, CharSetSolver solver)
        {
            BDD[] partition = _minterms;

            // the result will be the union of all minterms in the set
            BDD result = BDD.False;
            if (set == 0) return result;
            for (int i = 0; i < partition.Length; i++)
            {
                // include the i'th minterm in the union if the i'th bit is set
                if ((set & (1 << i)) != 0)
                {
                    result = solver.Or(result, partition[i]);
                }
            }

            return result;
        }

    }
}