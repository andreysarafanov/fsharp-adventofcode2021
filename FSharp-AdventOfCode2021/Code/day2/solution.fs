module Code.day2.solution

open System

type Direction = Down | Up | Forward
type Command = {
    Direction: Direction
    Distance: int
}
type Point = {X: int; Y:int}
type PointWithAim = {Point: Point; Aim: int}

let applyCommand point command =
    match command.Direction with
    | Down -> { point with Y = point.Y + command.Distance }
    | Up -> { point with Y = point.Y - command.Distance }
    | Forward -> { point with X = point.X + command.Distance }
    
let applyCommandWithAim point command =
    match command.Direction with
    | Down -> { point with Aim = point.Aim + command.Distance }
    | Up -> { point with Aim = point.Aim - command.Distance }
    | Forward ->
        let newX = point.Point.X + command.Distance
        let newY = point.Point.Y + (command.Distance * point.Aim)
        { point with Point = {X = newX; Y = newY} }

let parseInput (input: string) =
    let parseCommand (line: string) =
        let parts = line.Split(" ")
        let dir, dist = parts.[0], (int parts.[1])
        match dir with
        | "forward" -> {Direction = Forward; Distance = dist}
        | "up" -> {Direction = Up; Distance = dist}
        | "down" -> {Direction = Down; Distance = dist}
        | _ -> raise <| Exception $"Unknown direction {dir}"
    input.Split("\n") |> Array.map parseCommand

let part1 input =
    let commands = parseInput input
    Array.fold applyCommand {X = 0; Y = 0} commands
    
let part2 input =
    let commands = parseInput input
    Array.fold applyCommandWithAim {Aim = 0; Point = {X = 0; Y = 0}} commands
    