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

let listFold (bagType:string) (rules:Map<string, string list>) (rule:(int * string)) =
    match rule with
    | (number, bag) -> if rules.ContainsKey bag 
                        then Map.change bag (fun x -> x |> Option.map (fun y -> bagType :: y)) rules
                        else Map.add bag [bagType] rules

let mapFold (rules:Map<string, string list>) (rule:string * (int * string) list) =
    match rule with 
    | (currentBag, containRules) -> containRules |> List.fold (listFold currentBag) rules

let rec ancestorsOf bagType (bagMap:Map<string, string list>) =
    if bagMap.ContainsKey bagType
    then (bagMap.[bagType] @ List.collect (fun x -> ancestorsOf x bagMap) bagMap.[bagType])
    else []

let uniqueAncestorsOf bagType bagMap =
    ancestorsOf bagType bagMap
        |> Set.ofList

[<EntryPoint>]
let main argv =
    let input = readLines "input.txt"
                |> List.ofSeq
    
    input
        |> List.map ((fun x -> x.Split " bags contain ")
                    >> List.ofArray
                    >> tupleOfList
                    >> splitContainsRules)
        |> List.fold mapFold Map.empty
        |> uniqueAncestorsOf "shiny gold"
        |> Set.count
        |> printfn "%A"
    0 // return an integer exit code