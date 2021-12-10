module Code.day10.solution

open System
open System.Linq
open System.Collections.Generic

type BracketType = | Square | Round | Angle | Curly
type Bracket = { Type: BracketType; IsOpening: bool }
let getIllegalScoreOfBracket (bracket: Bracket) =
    match bracket with
    | {IsOpening = true; Type = _} -> 0
    | {Type = Round; IsOpening = false} -> 3
    | {Type = Square; IsOpening = false} -> 57
    | {Type = Curly; IsOpening = false} -> 1197
    | {Type = Angle; IsOpening = false} -> 25137

let parseBracket (bracket: char) =
    match bracket with
    | '(' -> {Type = Round; IsOpening = true} | ')' -> {Type = Round; IsOpening = false}
    | '[' -> {Type = Square; IsOpening = true} | ']' -> {Type = Square; IsOpening = false}
    | '<' -> {Type = Angle; IsOpening = true} | '>' -> {Type = Angle; IsOpening = false}
    | '{' -> {Type = Curly; IsOpening = true} | '}' -> {Type = Curly; IsOpening = false}
    | c -> raise <| Exception $"Not a bracket '${c}'"

let getIllegalScoreOfLine (line: string) =
    let rec loop (stack: Stack<Bracket>) i =
        if i = line.Length then None
        else
            let bracket = parseBracket (line.[i])
            if bracket.IsOpening
            then
                stack.Push bracket
                loop stack (i + 1)
            else
                if bracket.Type = stack.Pop().Type
                then loop stack (i + 1)
                else Some (getIllegalScoreOfBracket bracket)
    loop (new Stack<Bracket>()) 0

let getBracketCompletionCost bracket =
    match bracket.Type with
    | Round -> 1L
    | Square -> 2L
    | Curly -> 3L
    | Angle -> 4L

let getCompletionScore (bracketsLeft: List<Bracket>) =
    let rec loop i sum =
        if i = bracketsLeft.Count then sum
        else
            let bracketCost = getBracketCompletionCost bracketsLeft.[i]
            loop (i + 1) (sum * 5L + bracketCost)
    loop 0 0L
    
let getCompletionScoreOfLine (line: string) =
    let rec loop (stack: Stack<Bracket>) i =
        if i = line.Length then Some (getCompletionScore (stack.ToList()))
        else
            let bracket = parseBracket (line.[i])
            if bracket.IsOpening
            then
                stack.Push bracket
                loop stack (i + 1)
            else
                if bracket.Type = stack.Pop().Type
                then loop stack (i + 1)
                else None
    loop (new Stack<Bracket>()) 0
    
let part1 (input: string) =
    let lines = input.Split("\n") 
    lines |> Array.map getIllegalScoreOfLine |> Array.choose id |> Array.sum
    
let part2 (input: string) =
    let lines = input.Split("\n") 
    lines |> Array.map getCompletionScoreOfLine |> Array.choose id |> Array.sort |> fun arr -> arr.[(arr.Length - 1) / 2]