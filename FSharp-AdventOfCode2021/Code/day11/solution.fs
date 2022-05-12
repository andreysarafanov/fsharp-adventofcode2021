module Code.day11.solution

type Octopus =
    | Normal of int
    | UnhandledFlash
    | HandledFlash
with
    static member Increase this =
        match this with
        | Normal s when s = 9 -> UnhandledFlash
        | Normal s -> Normal (s + 1)
        | _ -> this
end

type Field(octopuses: Octopus[]) =
    let octopuses = octopuses
    member this.IncreaseAll = Field (octopuses |> Array.map Octopus.Increase)
    member this.HasUnhandledFlashes = octopuses |> Array.exists (fun oct -> oct = UnhandledFlash)
    member this.Item
        with get(line, col) = octopuses.[line * 10 + col]
    
    member this.CountFlashesAndRemoveThem =
        let count = octopuses |> Array.filter (fun oct -> oct = HandledFlash) |> Array.length
        let removeFlashes octopus =
            match octopus with
            | HandledFlash -> Normal 0
            | _ -> octopus
        Field (octopuses |> Array.map removeFlashes), count
        
    member this.HandleAllFlashes =
        let allNeighbors (x, y) =
             let allNeighbors =
                    [(x - 1, y - 1); (x, y - 1);  (x + 1, y - 1);
                    (x - 1, y); (x + 1, y);
                    (x - 1, y + 1); (x, y + 1);  (x + 1, y + 1)]
             allNeighbors |> List.filter (fun (x, y) -> x >= 0 && x <=9 && y >= 0 && y <= 9)
        let countNearbyUnhandledFlashes (x, y) =
            allNeighbors (x, y) |> List.filter (fun (x, y) -> this[x, y] = UnhandledFlash) |> List.length
            
        let handle (x, y) =
            match this[x, y] with
            | UnhandledFlash -> HandledFlash
            | Normal value ->
                let increaseBy = countNearbyUnhandledFlashes(x, y)
                let newValue = value + increaseBy
                if newValue > 9 then UnhandledFlash else Normal newValue
            | HandledFlash -> HandledFlash
        
        Field [|for x in 0..99 -> handle(x / 10, x % 10)|]

let nextStep (initialField: Field) =
    let fieldAfterIncreaseByOne = initialField.IncreaseAll
    let rec loop (field: Field) =
        if field.HasUnhandledFlashes
        then loop field.HandleAllFlashes
        else
            field.CountFlashesAndRemoveThem
    loop fieldAfterIncreaseByOne

let parseInput (input: string) =
    let lines = input.Split("\n")
    let parseOctopus (c: char) = Normal (int c - int '0')
    Field [|for x in 0..99 -> parseOctopus(lines.[x / 10].[x % 10])|]
let part1 input =
    let initialField = parseInput input
    let rec loop step field sum =
        if step > 100 then sum
        else
            let newField, increaseBy = nextStep field
            loop (step + 1) newField (sum + increaseBy)
    loop 1 initialField 0
    
let part2 input =
    let initialField = parseInput input
    let rec loop step field sum =
        let newField, increaseBy = nextStep field
        if increaseBy = 100
        then step
        else loop (step + 1) newField (sum + increaseBy)
    loop 1 initialField 0