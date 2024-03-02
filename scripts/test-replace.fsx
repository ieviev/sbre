#I "../src/Sbre.Test/bin/Debug/net8.0"
#r "RuntimeRegexCopy.dll"
#r "Sbre.dll"

open Sbre
open Sbre.Types
open Sbre.Optimizations

let text =
    """type Context1 =
    get, set
    get, set

type Context2 =
    get, set
    get, set
    get, set
"""

Sbre
    .Regex(
        join
            "&"
            [
                "get"
            ]
    )
    .Replace(text, "hello")
