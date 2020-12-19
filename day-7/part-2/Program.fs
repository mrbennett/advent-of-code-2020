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

let tupleOfList list = 
    match list with
    | [x; y] -> (x, y)
    | _ -> invalidArg "list" "list should only have two elements"

let regexParseRule rule =
    match rule with
    | ParseRegex "(\d) ([a-z]* [a-z]*) bags?\.?" a -> match (tupleOfList a) with (a, b) -> (int a, b)
    | ParseRegex "no other bags\." a -> (0, "no other bags")
    | _ -> invalidArg "rule" "rule cannot be split into parts"


let splitContainsRules (rules:'a * string) =
    match rules with
    | (a, b) -> (
                    a,
                    b.Split ", "
                        |> Array.map regexParseRule
                        |> List.ofArray
                )

let rec countChildBags (bagType:string) (bagMap:Map<string, (int * string) list>) =
    if bagMap.ContainsKey(bagType)
    then List.sumBy(fun (x, _) -> x) bagMap.[bagType] + List.sumBy (fun (x, y) -> x * (countChildBags y bagMap)) bagMap.[bagType]
    else 0

[<EntryPoint>]
let main argv =
    let input = readLines "input.txt"
                |> List.ofSeq
    
    input
        |> List.map ((fun x -> x.Split " bags contain ")
                    >> List.ofArray
                    >> tupleOfList
                    >> splitContainsRules)
        |> Map.ofList
        |> countChildBags "shiny gold"
        |> printfn "%A"
    
    0 // return an integer exit code