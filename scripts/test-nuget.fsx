#r "nuget: Sbre, 0.0.21"
let r = Sbre.Regex("(ab)+", false)
let matches1 = 
    r.Matches("__abab__ab__")
    |> Seq.toArray
