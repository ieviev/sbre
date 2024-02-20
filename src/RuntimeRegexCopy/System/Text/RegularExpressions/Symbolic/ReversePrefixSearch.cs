// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;

namespace System.Text.RuntimeRegexCopy.Symbolic;

public static class ReversePrefixSearch
{

    private static readonly ushort[] s_basicLatin = new ushort[256]
    {
      (ushort) 0,
      (ushort) 1,
      (ushort) 2,
      (ushort) 3,
      (ushort) 4,
      (ushort) 5,
      (ushort) 6,
      (ushort) 7,
      (ushort) 8,
      (ushort) 9,
      (ushort) 10,
      (ushort) 11,
      (ushort) 12,
      (ushort) 13,
      (ushort) 14,
      (ushort) 15,
      (ushort) 16,
      (ushort) 17,
      (ushort) 18,
      (ushort) 19,
      (ushort) 20,
      (ushort) 21,
      (ushort) 22,
      (ushort) 23,
      (ushort) 24,
      (ushort) 25,
      (ushort) 26,
      (ushort) 27,
      (ushort) 28,
      (ushort) 29,
      (ushort) 30,
      (ushort) 31,
      (ushort) 32,
      (ushort) 33,
      (ushort) 34,
      (ushort) 35,
      (ushort) 36,
      (ushort) 37,
      (ushort) 38,
      (ushort) 39,
      (ushort) 40,
      (ushort) 41,
      (ushort) 42,
      (ushort) 43,
      (ushort) 44,
      (ushort) 45,
      (ushort) 46,
      (ushort) 47,
      (ushort) 48,
      (ushort) 49,
      (ushort) 50,
      (ushort) 51,
      (ushort) 52,
      (ushort) 53,
      (ushort) 54,
      (ushort) 55,
      (ushort) 56,
      (ushort) 57,
      (ushort) 58,
      (ushort) 59,
      (ushort) 60,
      (ushort) 61,
      (ushort) 62,
      (ushort) 63,
      (ushort) 64,
      (ushort) 65,
      (ushort) 66,
      (ushort) 67,
      (ushort) 68,
      (ushort) 69,
      (ushort) 70,
      (ushort) 71,
      (ushort) 72,
      (ushort) 73,
      (ushort) 74,
      (ushort) 75,
      (ushort) 76,
      (ushort) 77,
      (ushort) 78,
      (ushort) 79,
      (ushort) 80,
      (ushort) 81,
      (ushort) 82,
      (ushort) 83,
      (ushort) 84,
      (ushort) 85,
      (ushort) 86,
      (ushort) 87,
      (ushort) 88,
      (ushort) 89,
      (ushort) 90,
      (ushort) 91,
      (ushort) 92,
      (ushort) 93,
      (ushort) 94,
      (ushort) 95,
      (ushort) 96,
      (ushort) 65,
      (ushort) 66,
      (ushort) 67,
      (ushort) 68,
      (ushort) 69,
      (ushort) 70,
      (ushort) 71,
      (ushort) 72,
      (ushort) 73,
      (ushort) 74,
      (ushort) 75,
      (ushort) 76,
      (ushort) 77,
      (ushort) 78,
      (ushort) 79,
      (ushort) 80,
      (ushort) 81,
      (ushort) 82,
      (ushort) 83,
      (ushort) 84,
      (ushort) 85,
      (ushort) 86,
      (ushort) 87,
      (ushort) 88,
      (ushort) 89,
      (ushort) 90,
      (ushort) 123,
      (ushort) 124,
      (ushort) 125,
      (ushort) 126,
      (ushort) sbyte.MaxValue,
      (ushort) 128,
      (ushort) 129,
      (ushort) 130,
      (ushort) 131,
      (ushort) 132,
      (ushort) 133,
      (ushort) 134,
      (ushort) 135,
      (ushort) 136,
      (ushort) 137,
      (ushort) 138,
      (ushort) 139,
      (ushort) 140,
      (ushort) 141,
      (ushort) 142,
      (ushort) 143,
      (ushort) 144,
      (ushort) 145,
      (ushort) 146,
      (ushort) 147,
      (ushort) 148,
      (ushort) 149,
      (ushort) 150,
      (ushort) 151,
      (ushort) 152,
      (ushort) 153,
      (ushort) 154,
      (ushort) 155,
      (ushort) 156,
      (ushort) 157,
      (ushort) 158,
      (ushort) 159,
      (ushort) 160,
      (ushort) 161,
      (ushort) 162,
      (ushort) 163,
      (ushort) 164,
      (ushort) 165,
      (ushort) 166,
      (ushort) 167,
      (ushort) 168,
      (ushort) 169,
      (ushort) 170,
      (ushort) 171,
      (ushort) 172,
      (ushort) 173,
      (ushort) 174,
      (ushort) 175,
      (ushort) 176,
      (ushort) 177,
      (ushort) 178,
      (ushort) 179,
      (ushort) 180,
      (ushort) 924,
      (ushort) 182,
      (ushort) 183,
      (ushort) 184,
      (ushort) 185,
      (ushort) 186,
      (ushort) 187,
      (ushort) 188,
      (ushort) 189,
      (ushort) 190,
      (ushort) 191,
      (ushort) 192,
      (ushort) 193,
      (ushort) 194,
      (ushort) 195,
      (ushort) 196,
      (ushort) 197,
      (ushort) 198,
      (ushort) 199,
      (ushort) 200,
      (ushort) 201,
      (ushort) 202,
      (ushort) 203,
      (ushort) 204,
      (ushort) 205,
      (ushort) 206,
      (ushort) 207,
      (ushort) 208,
      (ushort) 209,
      (ushort) 210,
      (ushort) 211,
      (ushort) 212,
      (ushort) 213,
      (ushort) 214,
      (ushort) 215,
      (ushort) 216,
      (ushort) 217,
      (ushort) 218,
      (ushort) 219,
      (ushort) 220,
      (ushort) 221,
      (ushort) 222,
      (ushort) 223,
      (ushort) 192,
      (ushort) 193,
      (ushort) 194,
      (ushort) 195,
      (ushort) 196,
      (ushort) 197,
      (ushort) 198,
      (ushort) 199,
      (ushort) 200,
      (ushort) 201,
      (ushort) 202,
      (ushort) 203,
      (ushort) 204,
      (ushort) 205,
      (ushort) 206,
      (ushort) 207,
      (ushort) 208,
      (ushort) 209,
      (ushort) 210,
      (ushort) 211,
      (ushort) 212,
      (ushort) 213,
      (ushort) 214,
      (ushort) 247,
      (ushort) 216,
      (ushort) 217,
      (ushort) 218,
      (ushort) 219,
      (ushort) 220,
      (ushort) 221,
      (ushort) 222,
      (ushort) 376
    };

 

    
    // internal static int IndexOfOrdinalIgnoreCase(
    //     ReadOnlySpan<char> source,
    //     ReadOnlySpan<char> value)
    // {
    //     if (value.Length == 0)
    //         return 0;
    //     if (value.Length > source.Length)
    //         return -1;
    //
    //     ref var local1 = ref MemoryMarshal.GetReference<char>(value);
    //     var c1 = local1;
    //
    //     if (!char.IsAscii(c1))
    //         return source.IndexOf(value, StringComparison.OrdinalIgnoreCase);
    //     var num1 = value.Length - 1;
    //     var length = source.Length - num1;
    //     ref var local2 = ref MemoryMarshal.GetReference<char>(source);
    //     var c2 = char.MinValue;
    //     var ch1 = char.MinValue;
    //     var elementOffset = IntPtr.Zero;
    //     var flag = false;
    //     if (Vector128.IsHardwareAccelerated && num1 != 0 && length >= Vector128<ushort>.Count)
    //     {
    //         c2 = Unsafe.Add<char>(ref local1, num1);
    //         if (char.IsAscii(c2))
    //         {
    //             var ch2 = (char)((uint)c1 | 32U);
    //             var ch3 = (char)((uint)c2 | 32U);
    //             IntPtr num2;
    //             char c3;
    //             for (num2 = (IntPtr)num1; (int)ch3 == (int)ch2 && num2 > new IntPtr(1); ch3 = (char)((uint)c3 | 32U))
    //             {
    //                 c3 = Unsafe.Add<char>(ref local1, num2 - new IntPtr(1));
    //                 if (char.IsAscii(c3))
    //                     --num2;
    //                 else
    //                     break;
    //             }
    //
    //             if (Vector256.IsHardwareAccelerated && length - Vector256<ushort>.Count >= 0)
    //             {
    //                 var left1 = Vector256.Create((char)ch2);
    //                 var left2 = Vector256.Create((char)ch3);
    //                 var num3 = (IntPtr)length - (IntPtr)Vector256<ushort>.Count;
    //                 uint num4;
    //                 while (true)
    //                 {
    //                     do
    //                     {
    //                         var vector256 = Vector256.Equals<char>(left2,
    //                             Vector256.BitwiseOr<char>(
    //                                 Vector256.LoadUnsafe(ref local2, (UIntPtr)(elementOffset + num2)),
    //                                 Vector256.Create((char)32)));
    //                         var vector = (Vector256.Equals<char>(left1,
    //                             Vector256.BitwiseOr<char>(Vector256.LoadUnsafe(ref local2, (UIntPtr)elementOffset),
    //                                 Vector256.Create((char)32))) & vector256).AsByte<char>();
    //                         if (vector != Vector256<byte>.Zero)
    //                             goto label_30;
    //                         label_26:
    //                         elementOffset += (IntPtr)Vector256<ushort>.Count;
    //                         if (elementOffset == (IntPtr)length)
    //                             return -1;
    //                         continue;
    //                         label_30:
    //                         var num5 = vector.ExtractMostSignificantBits<byte>();
    //                         label_31:
    //                         num4 = uint.TrailingZeroCount(num5) / 2U;
    //                         if (!Ordinal.EqualsIgnoreCase(
    //                                 ref Unsafe.Add<char>(ref local2, elementOffset + (IntPtr)num4), ref local1,
    //                                 value.Length))
    //                         {
    //                             num5 = BitOperations.ResetLowestSetBit(BitOperations.ResetLowestSetBit(num5));
    //                             if (num5 != 0U)
    //                                 goto label_31;
    //                             else
    //                                 goto label_26;
    //                         }
    //                         else
    //                         {
    //                             goto label_32;
    //                         }
    //                     } while (elementOffset <= num3);
    //
    //                     elementOffset = num3;
    //                 }
    //
    //                 label_32:
    //                 return (int)(elementOffset + (IntPtr)num4);
    //             }
    //
    //             var left3 = Vector128.Create((char)ch2);
    //             var left4 = Vector128.Create((char)ch3);
    //             var num6 = (IntPtr)length - (IntPtr)Vector128<char>.Count;
    //             uint num7;
    //             while (true)
    //             {
    //                 do
    //                 {
    //                     var vector128 = Vector128.Equals<char>(left4,
    //                         Vector128.BitwiseOr<char>(
    //                             Vector128.LoadUnsafe(ref local2, (UIntPtr)(elementOffset + num2)),
    //                             Vector128.Create((char)32)));
    //                     var vector = (Vector128.Equals<char>(left3,
    //                         Vector128.BitwiseOr<char>(Vector128.LoadUnsafe(ref local2, (UIntPtr)elementOffset),
    //                             Vector128.Create((char)32))) & vector128).AsByte<char>();
    //                     if (vector != Vector128<byte>.Zero)
    //                         goto label_40;
    //                     label_36:
    //                     elementOffset += (IntPtr)Vector128<ushort>.Count;
    //                     if (elementOffset == (IntPtr)length)
    //                         return -1;
    //                     continue;
    //                     label_40:
    //                     var num8 = vector.ExtractMostSignificantBits<byte>();
    //                     label_41:
    //                     num7 = uint.TrailingZeroCount(num8) / 2U;
    //                     
    //                     if (!Ordinal.EqualsIgnoreCase(ref Unsafe.Add<char>(ref local2, elementOffset + (IntPtr)num7),
    //                             ref local1, value.Length))
    //                     {
    //                         num8 = BitOperations.ResetLowestSetBit(BitOperations.ResetLowestSetBit(num8));
    //                         if (num8 != 0U)
    //                             goto label_41;
    //                         else
    //                             goto label_36;
    //                     }
    //                     else
    //                     {
    //                         goto label_42;
    //                     }
    //                 } while (elementOffset <= num6);
    //
    //                 elementOffset = num6;
    //             }
    //
    //             label_42:
    //             return (int)(elementOffset + (IntPtr)num7);
    //         }
    //     }
    //
    //     if (char.IsAsciiLetter(c1))
    //     {
    //         c2 = (char)((uint)c1 & 4294967263U);
    //         ch1 = (char)((uint)c1 | 32U);
    //         flag = true;
    //     }
    //
    //     do
    //     {
    //         int num9 = flag
    //             ? PackedSpanHelpers.PackedIndexOfIsSupported
    //                 ? PackedSpanHelpers.IndexOfAny(ref Unsafe.Add<char>(ref local2, elementOffset), c2, ch1, length)
    //                 : SpanHelpers.IndexOfAnyChar(ref Unsafe.Add<char>(ref local2, elementOffset), c2, ch1, length)
    //             : SpanHelpers.IndexOfChar(ref Unsafe.Add<char>(ref local2, elementOffset), c1, length);
    //         if (num9 >= 0)
    //         {
    //             var num10 = length - num9;
    //             if (num10 > 0)
    //             {
    //                 var num11 = elementOffset + (IntPtr)num9;
    //                 if (num1 == 0 || Ordinal.EqualsIgnoreCase(
    //                         ref Unsafe.Add<char>(ref local2, (UIntPtr)(num11 + new IntPtr(1))),
    //                         ref Unsafe.Add<char>(ref local1, 1), num1))
    //                     return (int)num11;
    //                 length = num10 - 1;
    //                 elementOffset = num11 + 1;
    //             }
    //             else
    //             {
    //                 break;
    //             }
    //         }
    //         else
    //         {
    //             break;
    //         }
    //     } while (length > 0);
    //
    //     return -1;
    // }
}