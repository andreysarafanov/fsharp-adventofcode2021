module Code.day4.solution

open System

type Board(boardId: int, initial: int list list, marked: Set<int>) =
    let boardId = boardId
    let values = initial
    let marked = marked

    member board.Mark value =
        let newBoard =
            Board(boardId, values, marked.Add(value))

        newBoard
    member board.isBoardWon =
        let horIsMarked =
            [ for y in 0 .. 4 do
                  yield (board.IsMarkedHorizontalLine y) ]
            |> List.exists id

        let vertIsMarked =
            [ for x in 0 .. 4 do
                  yield (board.IsMarkedVerticalLine x) ]
            |> List.exists id
        horIsMarked || vertIsMarked

    member board.GetWonScore =
        if board.isBoardWon then
            Some board.SumOfUnmarked
        else
            None

    member board.SumOfUnmarked =
        [ for y in 0 .. 4 do
              for x in 0 .. 4 do
                  yield (values.[y].[x], marked.Contains(values.[y].[x])) ]
        |> List.filter (fun (_, snd) -> not snd)
        |> List.sumBy fst

    member board.IsMarkedVerticalLine x =
        [ for y in 0 .. 4 do
              yield (marked.Contains values.[y].[x]) ]
        |> List.forall id

    member board.IsMarkedHorizontalLine y =
        [ for x in 0 .. 4 do
              yield (marked.Contains values.[y].[x]) ]
        |> List.forall id

    static member ParseBoard boardId (text: string) =
        let lineToNumbers (line: string) =
            line.Split(
                " ",
                StringSplitOptions.TrimEntries
                ||| StringSplitOptions.RemoveEmptyEntries
            )
            |> Array.toList
            |> List.map int

        let values =
            text.Split("\n")
            |> Array.map lineToNumbers
            |> Array.toList

        Board(boardId, values, Set.empty)

let parseInput (input: string) =
    let blocks = input.Split("\n\n") |> Array.toList

    let numbers =
        blocks.[0].Split(",")
        |> Array.toList
        |> List.map int

    let boards =
        blocks.Tail |> List.mapi Board.ParseBoard

    boards, numbers

let part1 input =
    let initialBoards, numbers = parseInput input

    let rec loop (boards: Board list) i =
        let value = numbers.[i]
        printfn $"value=%d{value}"

        let newBoards =
            boards |> List.map (fun b -> b.Mark value)

        let wonScore =
            newBoards |> List.tryPick (fun b -> b.GetWonScore)

        if wonScore.IsSome then
            printfn $"score=%d{wonScore.Value}"

        match wonScore with
        | None -> loop newBoards (i + 1)
        | Some score -> value * score

    loop initialBoards 0
    
let part2 input =
    let initialBoards, numbers = parseInput input

    let rec loop (boards: Board list) i =
        let value = numbers.[i]
        printfn $"value=%d{value}"

        let newBoards =
            boards |> List.map (fun b -> b.Mark value)
        let wonBoards = newBoards |> List.filter (fun b -> b.isBoardWon)
        let notWonBoards = newBoards |> List.filter (fun b -> not b.isBoardWon)

        if notWonBoards.IsEmpty
        then wonBoards |> List.last |> fun board ->(value * board.GetWonScore.Value)
        else loop notWonBoards (i+1)

    loop initialBoards 0
