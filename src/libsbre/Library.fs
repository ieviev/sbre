module libsbre

open System.Runtime.InteropServices
open System

[<UnmanagedCallersOnly(EntryPoint = "print_matches")>]
let print_matches (pStrPattern: nativeint) (pStrInput: nativeint) : int =
    try
        let pattern = Marshal.PtrToStringUTF8(pStrPattern)
        let input = Marshal.PtrToStringUTF8(pStrInput)
        let matchArray = Sbre.Regex(pattern).Matches(input) |> Seq.toArray
        for m in matchArray do
            stdout.WriteLine $"%A{m.Index}\t{m.Index + m.Length}\t{m.Value}"
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
        Marshal.StringToCoTaskMemUTF8(resultstr);
    with
    | _ -> -1

