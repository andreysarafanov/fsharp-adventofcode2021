module Code.day6.solution_1

open Code.day3

type Fish(age: int) =
    let age = age
    member this.PassOneDay =
        let newFish = if age = 0 then Some (Fish 8) else None
        let thisFish = if age = 0 then Fish 6 else Fish (age - 1)
        thisFish, newFish
        
let parseInput  (input: string) =
    input.Split(",") |> Array.map int |> Array.map Fish
    
let passOneDay (fish: Fish[]) =
    let newFish = fish |> Array.map (fun f -> f.PassOneDay)
    let oldFIsh = newFish |> Array.map fst
    let newbornFish = newFish |> Array.map snd |> Array.choose id
    Array.append oldFIsh newbornFish
    
let part1 input =
    let initialFish = parseInput input
    let days = 256
    let rec loop day (fish: Fish[]) =
//        printfn $"day=%d{day}, fish count = %d{fish.Length}"
        if day < days
        then loop (day + 1) (passOneDay fish)
        else fish.Length
    loop 0 initialFish