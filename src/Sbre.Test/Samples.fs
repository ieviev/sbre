module Sbre.Test.Samples


/// len 6+, 1+ upper, 1+ lower, 1+ number, 1+ special
let strongPassword1 = """(?=^.{6,}$)((?=.*\w)(?=.*[A-Z])(?=.*[a-z])(?=.*[0-9])(?=.*[|!"$%&\/\(\)\?\^\'\\\+\-\*]))^.*"""

/// len 8-18 , 1+ digit, 1+lower, 1+upper, 1+special
let strongPassword2 = """^(?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?=.*[!@#$^&*()_-]).{8,18}$"""

let emailClassic = """^([a-zA-Z0-9._%-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6})*$"""


// https://github.com/mariomka/regex-benchmark
let mariomkaURI = """[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?"""
let mariomkaEmail = """[\w\.+-]+@[\w\.-]+\.[\w\.-]+"""
let mariomkaIPv4 = """(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])"""


let hexColor = """\B#(?:[a-fA-F0–9]{6}|[a-fA-F0–9]{3})\b"""


let htmlTag = """<([a-z]+)[^<]*(?:>(.*?)<\/\1>|\s+\/>)"""


let trimSpaces = """^[\s]*(.*?)[\s]*$"""


let MarkTwainText =
    __SOURCE_DIRECTORY__ + "/data/mtwain-fixed-newlines.txt"
    |> System.IO.File.ReadAllText

let measureTimeMs fn =
    let watch = System.Diagnostics.Stopwatch.StartNew();
    let result = fn()
    watch.Stop()
    watch.ElapsedMilliseconds

let getRuntimeMatcher(pattern:string) =
    System.Text.RegularExpressions.Regex(pattern)

