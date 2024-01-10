[<Xunit.Collection("Sequential")>]
module Sbre.Test._05_IdentityTests

#if DEBUG

open Sbre
open Sbre.Algorithm
open Sbre.Pat
open Sbre.Types
open Xunit






//
//
// [<Fact>]
// let ``identity derivative concat 1`` () =
//     let m = Regex(@"(⊤*t|)neW⊤*")
//
//     let deriv = _04_DerivativeTests.getDerivative (m,"test")
//
//     let req = refEq m.UInt64Matcher.RawPattern deriv
//     Assert.True(req)
//
// [<Fact>]
// let ``identity derivative concat 2`` () =
//     let m = Regex(@"⊤*Twain")
//
//     let deriv = _04_DerivativeTests.getDerivative (m,"wain")
//
//     let isEqual = obj.ReferenceEquals(m.UInt64Matcher.RawPattern, deriv)
//     Assert.True(isEqual)
//
// [<Fact>]
// let ``identity derivative concat 3`` () =
//     let m = Regex(@".*1")
//
//     let deriv = _04_DerivativeTests.getDerivative (m,"a")
//
//     let req = refEq m.UInt64Matcher.RawPattern deriv
//     Assert.True(req)



[<Fact>]
let ``identity derivative 2`` () =
    let m = Regex(@"((⊤*t|)neW⊤*&⊤*erohsa⊤*&⊤*lirpA⊤*&⊤*yadsruhT⊤*)")

    let deriv = _04_DerivativeTests.getDerivative (m,"test")

    let req = refEq m.TSetMatcher.RawPattern deriv
    Assert.True(req)


// [<Fact>]
// let ``identity loop 1`` () =
//     let m = Matcher(@".*b|a")
//
//     let deriv = getDerivative (m,"aaab")
//
//     let req = refEq m.RawPattern deriv
//     Assert.True(req, $"{deriv.ToStringHelper()}")


[<Fact>]
let ``identity and 1`` () =
    let m = Regex(@"((nglish⊤*|⊤*English⊤*)&~(⊤*\n\n⊤*)\n&⊤*King⊤*&⊤*Paris⊤*)")

    let deriv = _04_DerivativeTests.getDerivativeT<uint64> (m,"English")

    let req = refEq (m.TSetMatcher.RawPattern) deriv
    Assert.True(req)


[<Fact>]
let ``identity singleton 1`` () =
    let m = Regex(@".*b|a")

    let deriv = _04_DerivativeTests.getDerivativeT<TSet> (m,"aaab")

    let l1 =
        match m.TSetMatcher.RawPattern with
        | Or(nodes,_) ->
            let conc = nodes |> Seq.find (function | Concat(_) -> true | _ -> false)
            let loop =
                match conc with
                | Concat(regexNode, tail, regexNodeInfo) ->
                    regexNode
                | _ -> failwith "debug"
            loop
        | _ -> failwith "debug"

    let l2 =
        match m.TSetMatcher.ReversePattern with
        | Or(nodes,_) ->
            let conc = nodes |> Seq.find (function | Concat(_) -> true | _ -> false)
            let loop =
                match conc with
                | Concat(regexNode, tail, regexNodeInfo) ->
                    tail
                | _ -> failwith "debug"
            loop
        | _ -> failwith "debug"

    let req = refEq l1 l2
    Assert.True(req)



// [<Fact>]
// let ``identity of \n 1`` () =
//     // let m = Matcher(@"\n\n~(⊤*\n\n⊤*)\n")
//     let m = Regex(@"\n\na")
//
//     let pattern = m.UInt64Matcher.RawPattern
//
//
//     let debug = 1
//     let firstChar, secondChar =
//         match pattern with
//         | Types.RegexNode.Concat(head,RegexNode.Concat(head2,_,_),_) ->
//             head, head2
//         | _ -> failwith "AAAAAAAAAAAa"
//
//
//     let equal1 = obj.Equals(firstChar, secondChar)
//     let equal2 = obj.ReferenceEquals(firstChar, secondChar)
//     Assert.True(equal2)
//
//
//
// [<Fact>]
// let ``identity of and 1`` () =
//     let m = Regex(@"(~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*)")
//
//     let derivative = _04_DerivativeTests.getDerivative (m,"asd")
//
//     let equal1 = obj.Equals(m.UInt64Matcher.RawPattern, derivative)
//     let equal2 = obj.ReferenceEquals(m.UInt64Matcher.RawPattern, derivative)
//     Assert.True(equal2)
//





#endif