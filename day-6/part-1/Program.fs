// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let inputFold input inputLine = 
    match inputLine with
    | "" -> List.empty :: input
    | line -> match input with
              | [] -> List.empty
              | headInput :: restInput -> (line :: headInput) :: restInput

let multiLineInput input 
    = List.fold inputFold [ List.empty ] input

[<EntryPoint>]
let main argv =
    let input = readLines "input.txt"
                |> Seq.toList
                |> multiLineInput
                |> List.map ((fun x -> List.collect Seq.toList x)
                                >> Set.ofList
                                >> Set.count)
                |> List.sum 
                |> printfn "%d"
    // input 
    //       |> List.rev
    //       |> List.map (List.collect (fun x -> ["a";"b"]))
    //       |> List.map (fun passport -> (validatePassport passport, passport, List.length passport))
    //     //   |> List.iter (printfn "%A")
    //       |> List.filter (fun x -> match x with | (valid, _, _) -> valid)
          
    //       |> List.length
    //       |> printfn "%A"


    0 // return an integer exit code