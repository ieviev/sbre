#r "nuget: FSharp.Data"

let [<Literal>] SampleFile = "/home/ian/f/ttu/iti0303-regexp-text-extraction/ExtractText/data/training-samples/Bibtex_Author.json" 
type Provider = FSharp.Data.JsonProvider<SampleFile>
let ctx = Provider.GetSample() 


let samples = 
    ctx.Examples
    |> Seq.map (fun v -> v.String)
    |> join "\n\n"

File.writeTo (__SOURCE_DIRECTORY__ + "/bibtexAuthors.txt") samples
