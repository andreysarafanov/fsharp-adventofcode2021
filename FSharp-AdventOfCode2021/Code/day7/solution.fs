module Code.day7.solution

open System

let parseInput (input: string) =
    input.Split(",") |> Array.map int
    
let part1 input =
    let positions = parseInput input
    let min, max = positions |> Array.min, positions |> Array.max
    let sumOfDistances (x: int) = positions |> Array.sumBy (fun p -> Math.Abs(x - p))
    [for x in min..max do yield x]
    |> List.map sumOfDistances
    |> List.min

let fuelCostOfDistance dist = dist * (dist + 1) / 2
    
let part2 input =
    let positions = parseInput input
    let min, max = positions |> Array.min, positions |> Array.max
    let sumOfDistances (x: int) = positions |> Array.sumBy (fun p -> fuelCostOfDistance (Math.Abs(x - p)))
    [for x in min..max do yield x]
    |> List.map sumOfDistances
    |> List.min