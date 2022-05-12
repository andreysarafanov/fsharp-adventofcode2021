module Code.day12.solution
open System
open System.Collections.Generic


type Graph = Map<string, string list>

let parseLine (line: string) =
    line.Split("-")
    |> fun arr -> [|arr.[0], arr.[1]; arr.[1], arr.[0]|]
    
let parseInput (input: string) : Graph =
    let ways = input.Split([|"\n"; "\r\n"|], StringSplitOptions.None) |> Array.collect parseLine
    // printfn  "got ways"
    let folder (map: Map<string, string list>) (from, destination) =
        match map |> Map.tryFind from with
        | Some ways -> map.Add(from, destination :: ways)
        | None -> map.Add(from, [ destination ])

    let map = ways |> Array.fold folder Map.empty
    // printfn  "got map"

    let sortedMap = map |> Map.map (fun _ v -> List.sort v)
    // printfn  "got sorted map"
    sortedMap

    
let coundValidPaths graph calcIsBadLoop =
    let rec loop (path: string list) (lastChildMaybe: string option) (count: int) =
        let lastCave = List.head path
        let isFinal = lastCave = "end"
        let isBadLoop = calcIsBadLoop isFinal path
        // printfn  "LOOP========================"
        // printf "path:["
//        path |> List.iter (fun x -> printf $"%s{x} ")
        // printfn  $"], lastChild: %A{lastChildMaybe}, count:%d{count}"
        let tryGetNextChild unit =
            let availableChildren = graph |> Map.tryFind lastCave
            match availableChildren with
            | None -> None
            | Some children ->
                match lastChildMaybe with
                | None -> Some (List.head children)
                | Some lastChild ->
                    let index = List.findIndex (fun c -> c = lastChild) children
                    match index with
                    | i when i < children.Length - 1 -> Some children.[i + 1]
                    | _ -> None
        let newPath, newLastChild, newCount =
            match isFinal, isBadLoop with
            | true, _ ->
                path.Tail, (Some lastCave), (count + 1)
            | false, true -> path.Tail, (Some lastCave), count
            | false, false ->
                let nextChild = tryGetNextChild()
                match nextChild with
                | None -> path.Tail, (Some lastCave), count
                | Some child -> (child :: path), None, count
        match newPath.Length with
        | 0 -> count
        | _ -> loop newPath newLastChild newCount
     
    loop ["start"] None 0
    
let isBadLoopPart1 (isFinal: bool) (path: string list) =
    let lastCave = List.head path
    (not isFinal) && Char.IsLower lastCave.[0] && (List.exists (fun c -> c = lastCave) path.Tail)
    
let isBadLoopPart2 (isFinal: bool) (path: string list) =
    match isFinal with
    | true -> false
    | false ->
        let folder ((set, copies): Set<string>*int) (v: string) =
            let isStartOrEnd = v = "start" || v = "end"
            let isCaveSmall = not isStartOrEnd && Char.IsLower v.[0] 
            let exists = set |> Set.contains v
            match exists, isStartOrEnd, isCaveSmall with
            | false, _, _  -> Set.add v set, copies
            | true, true, _ -> Set.add v set, copies + 10 
            | true, false, true -> Set.add v set, copies + 1
            | true, false, false -> Set.add v set, copies
        let (_, copies) =
            path |> List.fold folder (Set.empty, 0)
        copies > 1
    
let printGraph (graph: Graph) =
    let printNode node paths =
        printf "%s -> [" node
        paths |> List.iter (fun x -> printf $"%s{x} ")
        printfn  "]"
    graph |> Map.iter printNode
    
let part1 input =
    let graph = parseInput input
    coundValidPaths graph isBadLoopPart1
    
let part2 input =
    let graph = parseInput input
    coundValidPaths graph isBadLoopPart2