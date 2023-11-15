[<Xunit.Collection("Sequential")>]
module Sbre.Test._05_IdentityTests

#if DEBUG

open Sbre
open Sbre.Algorithm
open Sbre.Pat
open Sbre.Types
open Xunit

let getDerivative (matcher: Regex, input: string) =
    let cache = matcher.Cache
    let node = matcher.RawPattern
    let location = (Location.create input 0)
    // let matchCache = RegexMatchCache(cache,node)
    createDerivative (cache, location, cache.MintermForLocation(location), node)


let get2ndDerivative (pattern: string, input: string) =
    let matcher = Regex(pattern)
    let cache = matcher.Cache
    let node = matcher.ImplicitPattern
    let location = (Location.create input 0)
    let location1 = (Location.create input 1)

    let der1 =
        createDerivative (cache, location, cache.MintermForLocation(location), node)

    let der2 =
        createDerivative (cache, location1, cache.MintermForLocation(location1), der1)

    der2


let getNodeDerivative (m:Regex, node: RegexNode<_>, input: string) =
    let cache = m.Cache
    let location = (Location.create input 0)
    let der1 =
        createDerivative (cache, location, cache.MintermForLocation(location), node)

    der1



[<Fact>]
let ``identity derivative concat 1`` () =
    let m = Regex(@"(⊤*t|)neW⊤*")

    let deriv = getDerivative (m,"test")

    let req = refEq m.RawPattern deriv
    Assert.True(req)

[<Fact>]
let ``identity derivative concat 2`` () =
    let m = Regex(@"⊤*Twain")

    let deriv = getDerivative (m,"wain")

    let isEqual = obj.ReferenceEquals(m.RawPattern, deriv)
    Assert.True(isEqual)

[<Fact>]
let ``identity derivative concat 3`` () =
    let m = Regex(@".*1")

    let deriv = getDerivative (m,"a")

    let req = refEq m.RawPattern deriv
    Assert.True(req)


[<Fact>]
let ``identity derivative 1`` () =
    let m = Regex(@"(nglish⊤*|⊤*English⊤*)")
    // ⊥|(nglish⊤*|⊤*English⊤*)
    let deriv = getDerivative (m,"Eng")

    let req = refEq m.RawPattern deriv
    Assert.True(req)


[<Fact>]
let ``identity derivative 2`` () =
    let m = Regex(@"((⊤*t|)neW⊤*&⊤*erohsa⊤*&⊤*lirpA⊤*&⊤*yadsruhT⊤*)")

    let deriv = getDerivative (m,"test")

    let req = refEq m.RawPattern deriv
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

    let deriv = getDerivative (m,"English")

    let req = refEq m.RawPattern deriv
    Assert.True(req)


[<Fact>]
let ``identity singleton 1`` () =
    let m = Regex(@".*b|a")

    let deriv = getDerivative (m,"aaab")

    let l1 =
        match m.RawPattern with
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
        match m.ReversePattern with
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
    Assert.True(req, $"{l2.ToStringHelper()}")



[<Fact>]
let ``identity of \n 1`` () =
    // let m = Matcher(@"\n\n~(⊤*\n\n⊤*)\n")
    let m = Regex(@"\n\na")

    let pattern = m.RawPattern


    let debug = 1
    let firstChar, secondChar =
        match pattern with
        | Types.RegexNode.Concat(head,RegexNode.Concat(head2,_,_),_) ->
            head, head2
        | _ -> failwith "AAAAAAAAAAAa"


    let equal1 = obj.Equals(firstChar, secondChar)
    let equal2 = obj.ReferenceEquals(firstChar, secondChar)
    Assert.True(equal2)



[<Fact>]
let ``identity of and 1`` () =
    let m = Regex(@"(~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*)")

    let derivative = getDerivative (m,"asd")

    let equal1 = obj.Equals(m.RawPattern, derivative)
    let equal2 = obj.ReferenceEquals(m.RawPattern, derivative)
    Assert.True(equal2)


[<Fact>]
let ``identity of and 2`` () =
    let m = Regex(@"(~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*)")

    let pattern = m.RawPattern
    let deriv = getNodeDerivative (m,m.RawPattern,"\n")
    let deriv2 = getNodeDerivative (m,deriv,"a")

    let equal1 = obj.Equals(pattern, deriv2)
    let equal2 = obj.ReferenceEquals(pattern, deriv2)
    Assert.True(equal2)


[<Fact>]
let ``identity of and 3`` () =
    let m = Regex(@"(~(⊤*\n\n⊤*)\n&⊤*English⊤*&⊤*King⊤*)")

    let pattern = m.RawPattern
    let deriv = getNodeDerivative (m,m.RawPattern,"\n")
    let deriv2 = getNodeDerivative (m,deriv,"L")
    let deriv3 = getNodeDerivative (m,deriv2,"LL")
    let deriv4 = getNodeDerivative (m,deriv3,"L")

    let equal1 = obj.Equals(pattern, deriv4)
    let equal2 = obj.ReferenceEquals(pattern, deriv4)
    Assert.True(equal2)


[<Fact>]
let ``identity of and 4`` () =
    let m = Regex(@"((nglish⊤*|⊤*English⊤*)&~(⊤*\n\n⊤*)\n&⊤*King⊤*&⊤*Paris⊤*)")

    let pattern = m.RawPattern
    let deriv = getNodeDerivative (m,m.RawPattern,"E")
    let equal1 = obj.Equals(pattern, deriv)
    let equal2 = obj.ReferenceEquals(pattern, deriv)
    Assert.True(equal2)





#endif