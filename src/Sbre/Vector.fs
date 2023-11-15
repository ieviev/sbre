module Sbre.Vector

open System.Numerics

// unused
let inline isValidPrefix (regex:Vector<uint64>) (input:Vector<uint64>) =
    not (Vector.EqualsAny(Vector.BitwiseAnd(regex,input),Vector<uint64>.Zero))