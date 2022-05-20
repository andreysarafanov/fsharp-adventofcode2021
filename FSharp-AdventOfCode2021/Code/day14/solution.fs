module Code.day14.solution

open Newtonsoft.Json

type LinkedLetter = {
    mutable Next: LinkedLetter option
    Char: char
}

let parseInput (input: string) =
    let parts = input.Split("\n\n")
    let initialString = parts.[0]
    let addRule (allRules: Map<char*char, char>) (rule: string) =
        let result = allRules |> Map.add (rule.[0], rule.[1]) rule.[6]
        result
    
    let initialLetter = { Next = None; Char = initialString.[0] }
    let addCharToLinkedList (lastLinkedLetter: LinkedLetter) (c: char) =
        let newLetter = { Next = None; Char = c }
        lastLinkedLetter.Next <- Some newLetter
        newLetter
    initialString.ToCharArray() |> Array.skip 1 |> Array.fold addCharToLinkedList initialLetter |> ignore

    let rules = parts.[1].Split("\n") |> Array.fold addRule Map.empty
    initialLetter, rules

let applyRules (startingLetter: LinkedLetter) (rules: Map<char*char, char>) =
    let rec loop (currentLetter: LinkedLetter) =
        match currentLetter.Next with
        | None -> None
        | Some nextLetter ->
            let letterToInsert = rules |> Map.tryFind (currentLetter.Char, nextLetter.Char)
            match letterToInsert with
            | None -> loop nextLetter
            | Some newChar ->
                let newLetter = {Char = newChar; Next = Some nextLetter}
                currentLetter.Next <- Some newLetter
                loop nextLetter
    loop startingLetter
    
let printLetters (initialLetter: LinkedLetter) =
    let rec loop (currentLetter: LinkedLetter) =
        printf $"%c{currentLetter.Char}"
        match currentLetter.Next with
        | None -> None
        | Some nextLetter ->
            loop nextLetter
    printf "'"
    loop initialLetter |> ignore
    printf "'"
    printfn ""
    
let rec buildCountByLetter (current: LinkedLetter option) (map: Map<char, int>) =
    match current with
    | None -> map
    | Some letter ->
        let newMap =
            match map |> Map.tryFind letter.Char with
            | Some v -> map |> Map.add letter.Char (v + 1)
            | None -> map |> Map.add letter.Char 1
        buildCountByLetter letter.Next newMap
        
let getMinAndMaxDiff map =
    let max = map |> Map.toSeq |> Seq.maxBy snd |> snd
    let min = map |> Map.toSeq |> Seq.minBy snd |> snd
    max - min

let part1 input =
    let initialLetter, rules = parseInput input
    printLetters initialLetter
    [for _ in 0..11 -> applyRules initialLetter rules] |> ignore
    
    let map = buildCountByLetter (Some initialLetter) Map.empty
    printfn $"%s{JsonConvert.SerializeObject(map)}"
    getMinAndMaxDiff map
    
let part2 input =
    let initialLetter, rules = parseInput input
    printLetters initialLetter
    [for _ in 0..10 do
        applyRules initialLetter rules
        let map = buildCountByLetter (Some initialLetter) Map.empty
//        printLetters initialLetter |> ignore
        printfn $"%s{JsonConvert.SerializeObject(map)} - %d{getMinAndMaxDiff map}" 
        ] |> ignore
//    
//    let map = buildCountByLetter (Some initialLetter) Map.empty
//    printfn $"%s{JsonConvert.SerializeObject(map)}"
//    let max = map |> Map.toSeq |> Seq.maxBy snd |> snd
//    let min = map |> Map.toSeq |> Seq.minBy snd |> snd
//    max - min