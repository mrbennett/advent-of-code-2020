// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let twoTupleFromArray (items:array<'a>) = (items.[0], items.[1])

let passportFold passports passportLine = 
    match passportLine with
    | "" -> List.empty :: passports
    | line -> match passports with
              | [] -> List.empty
              | headPassport :: restPassports -> (line :: headPassport) :: restPassports

let validatePassport passportLine = 
    let requiredFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid";]
    let validateFields = fun (requiredField:string) -> passportLine |> List.exists (fun (passportField:string) -> passportField.Contains(requiredField))
    let testedFields = requiredFields |> List.map validateFields
                                      |> List.filter not
    List.isEmpty testedFields

[<EntryPoint>]
let main argv =
    let input = readLines "input.txt"
                |> Seq.toList
                |> List.rev
                // |> List.iter (printfn "%A")

    input |> List.fold passportFold [ List.empty ]
          |> List.map (List.collect (fun passDetails -> passDetails.Split [|' '|] |> List.ofArray))
          |> List.map (fun passport -> (validatePassport passport, passport, List.length passport))
        //   |> List.iter (printfn "%A")
          |> List.filter (fun x -> match x with | (valid, _, _) -> valid)
          
          |> List.length
          |> printfn "%A"


    0 // return an integer exit code