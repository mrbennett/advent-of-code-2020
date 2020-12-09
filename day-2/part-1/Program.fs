// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

[<StructuredFormatDisplay("{Min}-{Max} {Letter}: {Password}")>]
type PasswordPolicy = {
    Min: int
    Max: int
    Letter: char
    Password: string
}

let minAndMaxFromMinMax (minMax:string) =
    let splitMinMax = minMax.Split [|'-'|]
    (int splitMinMax.[0], int splitMinMax.[1])

let passwordPolicyFromTuple (line:string * string * string) =
    match line with
    | (minMax, letter, password) -> 
        let minAndMax = minAndMaxFromMinMax minMax
        let actualLetter = letter.[0]
        match minAndMax with
        | (min, max) -> { Min=min; Max=max; Letter=actualLetter; Password=password}

let validatePassword (passwordPolicy:PasswordPolicy) =
    let letterCount = passwordPolicy.Password
                      |> Seq.filter (fun x -> x.Equals passwordPolicy.Letter)
                      |> Seq.length
    letterCount >= passwordPolicy.Min && letterCount <= passwordPolicy.Max

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