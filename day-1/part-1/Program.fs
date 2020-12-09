// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let input = readLines "input.txt" |> Seq.map int

    let remaindersMap = input |> Seq.map (fun x -> 2020 - x, x) |> Map.ofSeq

    let result = Seq.filter (fun x -> remaindersMap.ContainsKey(x)) input

    result 
        |> Seq.map (fun x -> x * remaindersMap.[x]) 
        |> Seq.iter (printfn "%d")
    0 // return an integer exit code