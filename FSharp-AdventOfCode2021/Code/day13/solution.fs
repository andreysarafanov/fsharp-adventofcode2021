module Code.day13.solution

open System

type Fold =
    | Horizontal of int
    | Vertical of int
    
let parseInput (input: string) =
    let parsePoint (line: string) =
        let parts = line.Split(",")
        parts.[0] |> int, parts.[1] |> int
    let parseFold (line: string) =
        let parts = line.Split("=")
        let v = parts.[1] |> int
        match parts.[0].[parts.[0].Length - 1] with
        | 'x' -> Horizontal v
        | 'y' -> Vertical v
        | _ -> raise <| Exception "unknown axis"
        
    let parts = input.Split("\n\n")
    let points = parts.[0].Split("\n") |> Array.map parsePoint |> Set.ofArray
    let folds = parts.[1].Split("\n") |> Array.map parseFold
    points, folds
    
let applyFold (points: Set<int*int>) (fold: Fold) =
    let foldPoint set (x,y) =
        let newPoint =
            match fold with
            | Horizontal xLine when x <= xLine -> x, y
            | Horizontal xLine -> xLine + xLine - x, y
            | Vertical yLine when y <= yLine -> x, y
            | Vertical yLine -> x, yLine + yLine - y
        Set.add newPoint set
    points |> Set.toSeq |> Seq.fold foldPoint Set.empty
    
let printPoints (points: Set<int*int>) =
    let getBorders op part =
        points |> Set.toSeq |> op part |> part
    let left = getBorders Seq.minBy fst
    let right = getBorders Seq.maxBy fst
    let top = getBorders Seq.minBy snd
    let bottom = getBorders Seq.maxBy snd
    printfn $"left=%d{left}, right=%d{right}, top=%d{top}, bottom=%d{bottom}"
    
    
    
    for y in top .. bottom do
        for x in left .. right do
            match points |> Set.contains (x,y) with
            | true -> printf "#"
            | false -> printf " "
        printfn ""

let part1 input =
    let points, folds = parseInput input
    let newPoints = applyFold points folds.[0]
    newPoints.Count
    
let part2 input =
    let points, folds = parseInput input
    let newPoints = Array.fold applyFold points folds
    printPoints newPoints
    newPoints.Count