// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

[<StructuredFormatDisplay("{First}-{Second} {Letter}: {Password}")>]
type PasswordPolicy = {
    First: int
    Second: int
    Letter: char
    Password: string
}

let firstAndSecondFromFirstAndSecond (minMax:string) =
    let splitMinMax = minMax.Split [|'-'|]
    (int splitMinMax.[0], int splitMinMax.[1])

let passwordPolicyFromTuple (line:string * string * string) =
    match line with
    | (minMax, letter, password) -> 
        let minAndMax = firstAndSecondFromFirstAndSecond minMax
        let actualLetter = letter.[0]
        match minAndMax with
        | (min, max) -> { First=min; Second=max; Letter=actualLetter; Password=password}

let validatePassword (passwordPolicy:PasswordPolicy) =
    passwordPolicy.Password.[passwordPolicy.First - 1].Equals passwordPolicy.Letter
    <> passwordPolicy.Password.[passwordPolicy.Second - 1].Equals passwordPolicy.Letter

[<EntryPoint>]
let main argv =
    let answer = readLines "input.txt" 
                |> Seq.map (fun line -> line.Split [|' '|])
                |> Seq.map (fun parts -> (parts.[0], parts.[1], parts.[2]))
                |> Seq.map passwordPolicyFromTuple
                |> Seq.filter validatePassword
                |> Seq.length
                // |> Seq.iter (printfn "%A")

    printfn "%A" answer
    0 // return an integer exit code