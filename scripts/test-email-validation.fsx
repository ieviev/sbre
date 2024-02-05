open System.IO

Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)

let dotnet_emails =
    "/home/ian/f/ieviev/sbre/scripts/samples/email-dotnet-defacto.txt"
    |> File.ReadLines
    |> Seq.toList

let validDotnetEmails =
    dotnet_emails
    |> Seq.skip 1
    |> Seq.takeWhile (String.startsWith "--------- Invalid" >> not)
    |> Seq.where (String.isNullOrWhitespace >> not)
    |> Seq.toList

let invalidDotnetEmails =
    dotnet_emails
    |> Seq.skipWhile (String.startsWith "--------- Invalid" >> not)
    |> Seq.skip 1
    |> Seq.where (String.isNullOrWhitespace >> not)
    |> Seq.toList





