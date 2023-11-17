module libsbre

open System.Runtime.InteropServices
open System


[<UnmanagedCallersOnly(EntryPoint = "print_matches")>]
let print_matches (pPattern: nativeint) (pInput: nativeint) : int =
    try
        let pattern = Marshal.PtrToStringUTF8(pPattern)
        let input = Marshal.PtrToStringUTF8(pInput)
        stdout.WriteLine $"called fn"
        stdout.WriteLine $"pattern: {pattern}"
        stdout.WriteLine $"input: {input}"
        let matchArray = Sbre.Regex(pattern).Matches(input) |> Seq.toArray
        stdout.WriteLine $"matches: %A{matchArray}"
        0
    with
    | _ -> -1


[<UnmanagedCallersOnly(EntryPoint = "matches")>]
let matches (pPattern: nativeint) (pInput: nativeint) : nativeint =
    try
        let pattern = Marshal.PtrToStringUTF8(pPattern)
        let input = Marshal.PtrToStringUTF8(pInput)
        let matches = Sbre.Regex(pattern).Matches(input)
        let resultstr = 
            matches
            |> Seq.map (fun v -> v.Value)
            |> String.concat "\n"
        let pResultStr = Marshal.StringToCoTaskMemUTF8(resultstr);
        pResultStr
    with
    | _ -> -1

[<UnmanagedCallersOnly(EntryPoint = "add")>]
let Add (a: int) (b: int) : int =
    a + b + b

[<UnmanagedCallersOnly(EntryPoint = "write_line")>]
let WriteLine (pString: nativeint) : int =
    try
        let str = Marshal.PtrToStringUTF8(pString)
        Console.WriteLine("TEST")
        Console.WriteLine(str)
        0
    with
    | _ -> -1


[<UnmanagedCallersOnly(EntryPoint = "write_hello")>]
let WriteHello () : int =
    try
        Console.WriteLine("hello")
        0
    with
    | _ -> -1