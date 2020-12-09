open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let rec transpose = function
| [] -> failwith "cannot transpose a 0-by-n matrix"
| []::xs -> [] 
| xs -> List.map List.head xs :: transpose (List.map List.tail xs)

//Takes a list of strips and generates an infinite list of those repeating
let infiniteList strips =
    let width = List.length strips
    Seq.initInfinite (fun index ->
        let remainder = index % width
        strips.[remainder])

let treesForSlope hStep vStep slopeMap =
    let treeChar = '#'
    let courseLength = slopeMap |> Seq.item 0 |> List.length

    seq { 0 .. vStep .. courseLength-1 }
        |> Seq.map (fun n -> (slopeMap |> Seq.item (n / vStep * hStep)).[n])
        |> Seq.filter(fun x -> x.Equals(treeChar))
        |> Seq.length

[<EntryPoint>]
let main argv =
    let strips = readLines "input.txt"
                |> Seq.map Seq.toList
                |> List.ofSeq
                |> transpose

    let infiniteTobogganTrail = infiniteList strips

    // Right 1, down 1.
    treesForSlope 1 1 infiniteTobogganTrail
    // Right 3, down 1. (This is the slope you already checked.)
     * treesForSlope 3 1 infiniteTobogganTrail
    // Right 5, down 1.
     * treesForSlope 5 1 infiniteTobogganTrail
    // Right 7, down 1.
     * treesForSlope 7 1 infiniteTobogganTrail
    // Right 1, down 2.
     * treesForSlope 1 2 infiniteTobogganTrail
     |> printfn "%d"
    0 // return an integer exit code