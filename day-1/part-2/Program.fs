// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

let cartesian xs ys = 
    xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))

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
    let remaindersMap = cartesian input input 
                        |> Seq.map (fun nums -> match nums with (a, b) -> 2020 - a - b, nums)
                        |> Map.ofSeq
                        |> Map.filter (fun x _ -> x > 0)

    // remaindersMap |> Seq.map string |> Seq.iter (printf "%s ")

    // let remaindersMap = input |> Seq.map (fun x -> 2020 - x, x) |> Map.ofSeq

    let result = input
                  |> Seq.filter (fun x -> remaindersMap.ContainsKey(x))
                  |> Seq.map (fun x -> match remaindersMap.[x] with (a, b) -> x * a * b)

    result |> Seq.iter (printfn "%A")

    // result 
    //     |> Seq.map (fun x -> x * remaindersMap.[x]) 
    //     |> Seq.iter (printfn "%d")
    0 // return an integer exit code