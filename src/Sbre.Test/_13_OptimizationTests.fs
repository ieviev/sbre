[<Xunit.Collection("Sequential")>]
module Sbre.Test._13_OptimizationTests

open Sbre
open Sbre.Benchmarks.Jobs
open Sbre.CountingSet
open Sbre.Info
open Sbre.Optimizations
open Sbre.Types
open Xunit
open Common

#if DEBUG


[<Fact>]
let ``fixed length 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let prefixLen = Node.getFixedLength matcher.ReversePattern
    Assert.Equal(Some 5, prefixLen)

[<Fact>]
let ``fixed length 2``() =
    let regex = Regex("[a-q][^u-z]{13}x")
    let matcher = regex.TSetMatcher
    let prefixLen = Node.getFixedLength matcher.ReversePattern
    Assert.Equal(Some 15, prefixLen)

[<Fact>]
let ``fixed length 3``() =
    let regex = Regex("""\b1\b""")
    let matcher = regex.TSetMatcher
    let prefixLen = Node.getFixedLength matcher.ReversePattern
    Assert.Equal(Some 1, prefixLen)





[<Fact>]
let ``calc reverse prefix 1``() =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("n;i;a;w;T", prefixString)




[<Fact>]
let ``calc reverse prefix 2``() =
    let regex = Regex("⊤*A⊤*&⊤*B")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("B", prefixString)


[<Fact>]
let ``calc reverse prefix 3``() =
    let regex = Regex(@"⊤*Huck⊤*")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    // todo: should be kcuH
    Assert.Equal("k;c;u;H", prefixString)


[<Fact>]
let ``calc reverse prefix 4``() =
    let regex = Regex(@"~(⊤*\n\n⊤*)&⊤*Huck⊤*")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("k;c;u;H", prefixString)


[<Fact>]
let ``calc reverse prefix 5``() =
    let regex = Regex(@"~(.*11.*)&[az1]{8,}")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPrefixSets getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("[1az]", prefixString)


[<Fact>]
let ``calc potential start 1``() =
    let regex = Regex("Tom|Sawyer|Huckleberry|Finn")
    let matcher = regex.TSetMatcher
    let getflags = (fun node -> matcher.GetOrCreateState(node).Flags)
    let getder = (fun (mt,node) ->
        let loc = Pat.Location.getNonInitial()
        matcher.CreateDerivative(&loc, mt,node)
    )
    let prefix =
        Optimizations.calcPotentialMatchStart getder getflags matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("[mnry];[enor];[Tiry]", prefixString)


[<Fact>]
let ``apply prefix 1``() =
    let applied = Common.applyPrefix "Twain"
    assertNodeOneOf applied [
        @"(ε|⊤*niawT)"
        @"(⊤*niawT|ε)"
    ]

[<Fact>]
let ``initialOptimizations 01``() =
    let optimizations = getInitOptimizations "Twain"
    match optimizations with
    | Optimizations.InitialOptimizations.StringPrefix(prefix, transitionNode) ->
        Assert.True(prefix.Length = 5)
    | _ -> failwith "invalid optimization result"

[<Fact>]
let ``initialOptimizations 02``() =
    let optimizations = getInitOptimizations "Tom|Sawyer|Huckleberry|Finn"
    match optimizations with
    | Optimizations.InitialOptimizations.PotentialStartPrefix(prefix) ->
        Assert.True(prefix.Length = 3)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 03``() =

    let optimizations = getInitOptimizations "..g"
    match optimizations with
    // | Optimizations.InitialOptimizations.ReverseStringPrefix(prefix,_) ->
    //     Assert.Equal(1,prefix.Length)
    | Optimizations.InitialOptimizations.SetsPrefix(prefix,_) ->
        Assert.Equal(3,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 04``() =
    let optimizations = getInitOptimizations "[a-z]shing"
    match optimizations with
    | Optimizations.InitialOptimizations.StringPrefix(prefix,_) ->
        Assert.Equal(5,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 05``() =
    let optimizations = getInitOptimizations ".*t.*hat.*&.*a.*nd.*&.*t.*he.*&.*w.*as.*"
    match optimizations with
    | Optimizations.InitialOptimizations.PotentialStartPrefix(prefix) ->
        Assert.Equal(2,prefix.Length)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 06``() =
    assertPotentialPrefix "Huck[a-zA-Z]+|Saw[a-zA-Z]+" "[A-Za-z];[kw];[ac];[Su]"


[<Fact>]
let ``initialOptimizations 07``() =
    assertPotentialPrefix "Tom|Sawyer|Huckleberry|Finn" "[mnry];[enor];[Tiry]"

[<Fact>]
let ``initialOptimizations 08``() =
    assertPotentialPrefix "\s([A-Za-z]awyer|[A-Za-z]inn)\s" "\s;[nr];[en];[iy];[A-Za-z];\s"


[<Fact>]
let ``initialOptimizations 09``() =
    // TODO: use this after optimizations done
    assertSetsPrefix @"\b\w+nn\b" @"\W;n;n;[0-9A-Z_a-z\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0300-\u0374\u0376\u0377\u037A-\u037D\u037F\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u0483-\u0487\u048A-\u052F\u0531-\u0556\u0559\u0560-\u0588\u0591-\u05BD\u05BF\u05C1\u05C2\u05C4\u05C5\u05C7\u05D0-\u05EA\u05EF-\u05F2\u0610-\u061A\u0620-\u0669\u066E-\u06D3\u06D5-\u06DC\u06DF-\u06E8\u06EA-\u06FC\u06FF\u0710-\u074A\u074D-\u07B1\u07C0-\u07F5\u07FA\u07FD\u0800-\u082D\u0840-\u085B\u0860-\u086A\u0870-\u0887\u0889-\u088E\u0898-\u08E1\u08E3-\u0902\u0904-\u093A\u093C\u093D\u0941-\u0948\u094D\u0950-\u0963\u0966-\u096F\u0971-\u0981\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BC\u09BD\u09C1-\u09C4\u09CD\u09CE\u09DC\u09DD\u09DF-\u09E3\u09E6-\u09F1\u09FC\u09FE\u0A01\u0A02\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A3C\u0A41\u0A42\u0A47\u0A48\u0A4B-\u0A4D\u0A51\u0A59-\u0A5C\u0A5E\u0A66-\u0A75\u0A81\u0A82\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABC\u0ABD\u0AC1-\u0AC5\u0AC7\u0AC8\u0ACD\u0AD0\u0AE0-\u0AE3\u0AE6-\u0AEF\u0AF9-\u0AFF\u0B01\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3C\u0B3D\u0B3F\u0B41-\u0B44\u0B4D\u0B55\u0B56\u0B5C\u0B5D\u0B5F-\u0B63\u0B66-\u0B6F\u0B71\u0B82\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BC0\u0BCD\u0BD0\u0BE6-\u0BEF\u0C00\u0C04-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C39\u0C3C-\u0C40\u0C46-\u0C48\u0C4A-\u0C4D\u0C55\u0C56\u0C58-\u0C5A\u0C5D\u0C60-\u0C63\u0C66-\u0C6F\u0C80\u0C81\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBC\u0CBD\u0CBF\u0CC6\u0CCC\u0CCD\u0CDD\u0CDE\u0CE0-\u0CE3\u0CE6-\u0CEF\u0CF1\u0CF2\u0D00\u0D01\u0D04-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3D\u0D41-\u0D44\u0D4D\u0D4E\u0D54-\u0D56\u0D5F-\u0D63\u0D66-\u0D6F\u0D7A-\u0D7F\u0D81\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0DCA\u0DD2-\u0DD4\u0DD6\u0DE6-\u0DEF\u0E01-\u0E3A\u0E40-\u0E4E\u0E50-\u0E59\u0E81\u0E82\u0E84\u0E86-\u0E8A\u0E8C-\u0EA3\u0EA5\u0EA7-\u0EBD\u0EC0-\u0EC4\u0EC6\u0EC8-\u0ECD\u0ED0-\u0ED9\u0EDC-\u0EDF\u0F00\u0F18\u0F19\u0F20-\u0F29\u0F35\u0F37\u0F39\u0F40-\u0F47\u0F49-\u0F6C\u0F71-\u0F7E\u0F80-\u0F84\u0F86-\u0F97\u0F99-\u0FBC\u0FC6\u1000-\u102A\u102D-\u1030\u1032-\u1037\u1039\u103A\u103D-\u1049\u1050-\u1055\u1058-\u1061\u1065\u1066\u106E-\u1082\u1085\u1086\u108D\u108E\u1090-\u1099\u109D\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u135D-\u135F\u1380-\u138F\u13A0-\u13F5\u13F8-\u13FD\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u16F1-\u16F8\u1700-\u1714\u171F-\u1733\u1740-\u1753\u1760-\u176C\u176E-\u1770\u1772\u1773\u1780-\u17B5\u17B7-\u17BD\u17C6\u17C9-\u17D3\u17D7\u17DC\u17DD\u17E0-\u17E9\u180B-\u180D\u180F-\u1819\u1820-\u1878\u1880-\u18AA\u18B0-\u18F5\u1900-\u191E\u1920-\u1922\u1927\u1928\u1932\u1939-\u193B\u1946-\u196D\u1970-\u1974\u1980-\u19AB\u19B0-\u19C9\u19D0-\u19D9\u1A00-\u1A18\u1A1B\u1A20-\u1A54\u1A56\u1A58-\u1A5E\u1A60\u1A62\u1A65-\u1A6C\u1A73-\u1A7C\u1A7F-\u1A89\u1A90-\u1A99\u1AA7\u1AB0-\u1ABD\u1ABF-\u1ACE\u1B00-\u1B03\u1B05-\u1B34\u1B36-\u1B3A\u1B3C\u1B42\u1B45-\u1B4C\u1B50-\u1B59\u1B6B-\u1B73\u1B80\u1B81\u1B83-\u1BA0\u1BA2-\u1BA5\u1BA8\u1BA9\u1BAB-\u1BE6\u1BE8\u1BE9\u1BED\u1BEF-\u1BF1\u1C00-\u1C23\u1C2C-\u1C33\u1C36\u1C37\u1C40-\u1C49\u1C4D-\u1C7D\u1C80-\u1C88\u1C90-\u1CBA\u1CBD-\u1CBF\u1CD0-\u1CD2\u1CD4-\u1CE0\u1CE2-\u1CF6\u1CF8-\u1CFA\u1D00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u203F\u2040\u2054\u2071\u207F\u2090-\u209C\u20D0-\u20DC\u20E1\u20E5-\u20F0\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2CE4\u2CEB-\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D7F-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2DE0-\u2DFF\u2E2F\u3005\u3006\u302A-\u302D\u3031-\u3035\u303B\u303C\u3041-\u3096\u3099\u309A\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312F\u3131-\u318E\u31A0-\u31BF\u31F0-\u31FF\u3400-\u4DBF\u4E00-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA62B\uA640-\uA66F\uA674-\uA67D\uA67F-\uA6E5\uA6F0\uA6F1\uA717-\uA71F\uA722-\uA788\uA78B-\uA7CA\uA7D0\uA7D1\uA7D3\uA7D5-\uA7D9\uA7F2-\uA822\uA825\uA826\uA82C\uA840-\uA873\uA882-\uA8B3\uA8C4\uA8C5\uA8D0-\uA8D9\uA8E0-\uA8F7\uA8FB\uA8FD-\uA92D\uA930-\uA951\uA960-\uA97C\uA980-\uA982\uA984-\uA9B3\uA9B6-\uA9B9\uA9BC\uA9BD\uA9CF-\uA9D9\uA9E0-\uA9FE\uAA00-\uAA2E\uAA31\uAA32\uAA35\uAA36\uAA40-\uAA4C\uAA50-\uAA59\uAA60-\uAA76\uAA7A\uAA7C\uAA7E-\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAEC\uAAED\uAAF2-\uAAF4\uAAF6\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uAB30-\uAB5A\uAB5C-\uAB69\uAB70-\uABE2\uABE5\uABE8\uABED\uABF0-\uABF9\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE00-\uFE0F\uFE20-\uFE2F\uFE33\uFE34\uFE4D-\uFE4F\uFE70-\uFE74\uFE76-\uFEFC\uFF10-\uFF19\uFF21-\uFF3A\uFF3F\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]"
    ()

[<Fact>]
let ``initialOptimizations 10``() =
    // TODO: use this after optimizations done
    assertSetsPrefix @"(?<=\W)hello(?=\W)" @"\W;o;l;l;e;h;\W"
    // assertPotentialPrefix @"(?<=\W)hello(?=\W)" @"\W;o;l;l;e;h;\W"



// [<Fact>]
// let ``activeOptimizations 1``() =
//     let regex = Regex("""["'][^"']{0,30}[?!\.]["']""")
//     let matcher = regex.TSetMatcher
//     let c = matcher.Cache
//     let der1 = Algorithm.createStartsetDerivative(c, c.CharToMinterm('"'), matcher.ReverseTrueStarredPattern)
//     let der2 = Algorithm.createStartsetDerivative(c, c.CharToMinterm('.'), der1)
//     let optimizations =
//         Optimizations.tryGetLimitedSkip
//             (fun node -> matcher.GetOrCreateState(node).Id)
//             (fun node -> matcher.GetOrCreateState(node).Startset)
//             matcher.Cache matcher.ReverseTrueStarredPattern der2
//     match optimizations with
//     | Some (Optimizations.ActiveBranchOptimizations.LimitedSkip(distance=_)) ->
//         ()
//         // let prefixString = Optimizations.printPrefixSets matcher.Cache (prefix.ToArray() |> Seq.toList)
//         // Assert.Equal("\s;[nr];[en];[iy];[A-Za-z];\s", prefixString)
//     | _ -> failwith "invalid optimization result"
//




#endif