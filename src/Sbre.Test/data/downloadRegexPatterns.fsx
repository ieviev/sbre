#r "nuget: FSharp.Data"
#r "nuget: FSharpPlus"

open System.IO
open System.Text.Json
open FSharpPlus
open FSharp.Data
open System
open System.Text
open System.Web

let pageUrl i = $"https://regexlib.com/Search.aspx?k=&c=-1&m=2&ps=20&p={i}"

let allpages = 
    [| 1..56 |]
    |> Array.map pageUrl
    |> Array.map Http.AsyncRequestString
    |> Async.Parallel
    |> Async.RunSynchronously

let htmlDocuments = allpages |> Seq.map HtmlDocument.Parse |> Seq.toList

let getEntries (doc:HtmlDocument) = doc.CssSelect(".searchResultsTable")

// for some strange reason you have to decode the emails 
let decodeEmail (input:string) =
    let bytes = input |> Convert.FromHexString
    bytes[1..] |> Array.map ((^^^) bytes[0] >> char) |> String

let decodeInner split (tdElement:HtmlNode) = 
    let divNode = tdElement.Elements().[0]
    divNode.Elements()
    |> List.map (fun f -> 
        match f.HasName "a" with 
        | true -> f |> (HtmlNode.attributeValue "data-cfemail" >> decodeEmail) 
        | false -> f.InnerText()
    )
    |> String.concat ""
    |> (fun f -> if split then f |> String.split ["|"] else [f])
    |> Seq.map String.trimWhiteSpaces
    |> Seq.toList

let readRegexEntry (htmlNode:HtmlNode) =
    let data = htmlNode.CssSelect("td")
    {|
        title = decodeInner false data[0] |> String.concat ""
        pattern = decodeInner false (data[1]) |> String.concat ""
        description = decodeInner false data[2] |> String.concat ""
        matches = decodeInner true data[3]
        nonMatches = decodeInner true data[4]  
      |}
      
let readEntries (entries:HtmlNode list) = entries |> List.map readRegexEntry

let regexPatterns = htmlDocuments |> List.collect (getEntries >> readEntries) 

regexPatterns
|> JsonSerializer.Serialize
|> (fun o -> File.WriteAllText("regexlibpatterns.json",o))
