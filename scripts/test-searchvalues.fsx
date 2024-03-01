#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open Microsoft.FSharp.Core
open Sbre
open Sbre.Optimizations

let regex = Regex(@"\w+nn\W")
let cache = regex.TSetMatcher.Cache
let prefix = regex.InitialReversePrefix
let prefixSets = // [6UL; 4UL; 4UL; 1UL]
    match prefix with
    | InitialOptimizations.SetsPotentialStart(prefixMem) -> 
        Array.toList (prefixMem.ToArray()) |> List.rev
    | InitialOptimizations.SetsPrefix(prefixMem, _) ->
        Array.toList (prefixMem.ToArray()) |> List.rev
    | _ -> failwith "?"



cache.PrettyPrintMinterm 6UL                  // [0-9A-Z_a-z\u00AA...] -> '\w'
cache.MintermSearchValues(6UL).Contains('n')  // true
cache.MintermSearchValues(6UL).Contains('a')  // false ??
(cache.MintermChars 6UL).ToString()           // 'n'   ??

cache.PrettyPrintMinterm 4UL                  // 'n'
(cache.MintermChars 4UL).ToString()           // 'n'

cache.PrettyPrintMinterm 1UL                  // \W
cache.MintermSearchValues(1UL).Contains('a')  // false
cache.MintermSearchValues(1UL).Contains(' ')  // false ??
cache.MintermSearchValues(1UL).Contains('n')  // true  ??
(cache.MintermChars 1UL).ToString()           // 'n'   ??