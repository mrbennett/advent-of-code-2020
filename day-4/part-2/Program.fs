// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let twoTupleFromArray (items:array<'a>) = (items.[0], items.[1])

let yearBetween (min) (max) (year:string) =
    year.Length = 4 && int year >= min && int year <= max

    // let result = year.Length = 4 && int year >= min && int year <= max
    // printfn "year: %A between: %d-%d <%A>" year min max result
    // result

let heightValidate height =
    let result = match height with
                    | ParseRegex "^(\d*)cm$" [Integer n] -> printfn "cm: %d" n; n >= 150 && n <= 193
                    | ParseRegex "^(\d*)in$" [Integer n] -> printfn "in: %d" n; n >= 59 && n <= 76
                    | _ -> false
    printfn "height :%A <%A>" height result
    result

let fieldValidation  = 
    [
        ("byr", yearBetween 1920 2002);
        ("iyr", yearBetween 2010 2020);
        ("eyr", yearBetween 2020 2030 );
        ("hgt", heightValidate);
        ("hcl", fun (x:string) -> Regex.IsMatch(x, "#[0-9a-f]{6}"));
        ("ecl", fun (x:string) -> ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> List.contains x);
        ("pid", fun (x:string) -> Regex.IsMatch(x, "^[0-9]{9}$"));
        ("cid", fun (x:string) -> true);
    ] 
    |> Map.ofList

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

let twoElementListToTuple (list:List<'a>) =
    (list.[0], list.[1])

let passportLineToMap (passportLine:List<string>) =
    passportLine |> List.map (fun x -> x.Split[|':'|] |> List.ofArray |> twoElementListToTuple)
                 |> Map.ofList

let validatePassportFields (passport:Map<string, string>) =
    passport |> Map.filter ((fun k v -> fieldValidation.[k] v |> not))
             |> Map.isEmpty

[<EntryPoint>]
let main argv =
    let input = readLines "input.txt"
                |> Seq.toList
                |> List.rev
                // |> List.iter (printfn "%A")

    input |> List.fold passportFold [ List.empty ]
          |> List.map (List.collect (fun passDetails -> passDetails.Split [|' '|] |> List.ofArray)
                        >> fun passport -> (validatePassport passport, passport))
        //   |> List.iter (printfn "%A")
          |> List.filter (fun x -> match x with | (valid, _) -> valid)
          |> List.map ((fun (_, pass) -> pass)
                        >> passportLineToMap)
          |> List.filter validatePassportFields
        //   |> List.iter (printfn "%A")
          
          |> List.length
          |> printfn "%A"


    0 // return an integer exit code