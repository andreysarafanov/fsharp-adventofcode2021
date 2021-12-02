module Code.day1.solution

open System

let parseInput (input: string) =
    input.Split(Environment.NewLine) |> Array.map int

let part1 input =
    let numbers = parseInput input
    let rec loop i counter =
        let endOfArray = i = numbers.Length
        match endOfArray with
        | true -> counter
        | false when numbers.[i] > numbers.[i-1] -> loop (i+1) (counter+1)
        | false -> loop (i+1) counter
    loop 1 0
    
let part2 input =
    let numbers = parseInput input
    let rec loop i counter =
        let endOfArray = i = numbers.Length
        match endOfArray with
        | true -> counter
        | false when numbers.[i] + numbers.[i-1] + numbers.[i-2] > numbers.[i-1] + numbers.[i-2] + numbers.[i-3]
             -> loop (i+1) (counter+1)
        | false -> loop (i+1) counter
    loop 3 0