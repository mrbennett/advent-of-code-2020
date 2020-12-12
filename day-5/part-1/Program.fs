// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let rec binarySpacePartition (lower:int) (upper:int) isUpper instructions =
    match instructions with
    | [] -> lower
    | current :: rest -> if isUpper current
                         then binarySpacePartition ((lower + upper) / 2) upper isUpper rest
                         else binarySpacePartition lower ((lower + upper) / 2) isUpper rest

[<EntryPoint>]
let main argv =
    let input = readLines "input.txt"
                |> List.ofSeq
                |> List.map (Seq.toList
                             >> (fun x -> (List.take 7 x, List.skip 7 x)))
                |> List.map (fun (row, seat) -> (
                                                   binarySpacePartition 0 128 (fun x -> x = 'B') row
                                                 , binarySpacePartition 0 8 (fun x -> x = 'R') seat)
                                                )
                |> List.map (fun (row, seat) -> row * 8 + seat)
                // |> List.max
                // |> printfn "%A"
                |> List.sort
                |> List.pairwise
                |> List.filter (fun (x, y) -> y - x <> 1)
                |> List.iter (printfn "%A")

    // input |> List.iter (printfn "%A") 
    0 // return an integer exit code

// binarySpacePartition 0 128 isUpper [FBFBBFF]
// binarySpacePartition 0 128 isUpper [BFBBFF]
// binarySpacePartition 0 128 isUpper [FBBFF]
// binarySpacePartition 0 128 isUpper [BBFF]
// binarySpacePartition 0 128 isUpper [BFF]
// binarySpacePartition 0 128 isUpper [FF]
// binarySpacePartition 0 128 isUpper [F]
// binarySpacePartition 0 128 isUpper []