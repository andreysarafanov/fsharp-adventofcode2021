module Code.day9.solution

type HeightMap(text: string) =
    let lines = text.Split("\n")
    member this.Height = lines.Length
    member this.Width = lines.[0].Length
    member this.GetAt (line: int, col: int) = (int lines.[line].[col]) - (int '0')

    member this.getNeighbors (line, col) =
        let neighbors =
            [| line - 1, col
               line + 1, col
               line, col - 1
               line, col + 1 |]

        neighbors
        |> Array.filter
            (fun (line, col) ->
                line >= 0
                && line < this.Height
                && col >= 0
                && col < this.Width)
    
    member this.getBasinSize line col =
        let rec loop (points: Set<int*int>) =
            let getNotIncludedPoint (line, col) =
                this.getNeighbors (line, col)
                |> Array.tryFind (fun point -> (this.GetAt point) <> 9 && not (points.Contains(point)))
                
            let notIncludedPoint = points |> Set.toSeq |> Seq.tryPick getNotIncludedPoint
            match notIncludedPoint with
            | None -> points.Count
            | Some point -> loop (points.Add point)
        loop (Set.empty.Add((line, col)))
        
    member this.IsLowPoint line col =
//        printfn $"{line} {col}"
        let height = this.GetAt (line, col)

        let neighbors =
            this.getNeighbors (line, col)
            |> Array.map this.GetAt

        neighbors
            |> Array.forall (fun value -> value > height)

    member this.GetLowPointRiskLevel line col =
        if (this.IsLowPoint line col) then
            let height = this.GetAt (line, col)
            Some(height + 1)
        else
            None



let part1 input =
    let map = HeightMap input

    seq {
        for i in 0 .. (map.Height - 1) do
            for j in 0 .. (map.Width - 1) -> map.GetLowPointRiskLevel i j
    }
    |> Seq.choose id
    |> Seq.sum
    
let part2 input =
    let map = HeightMap input

    seq {
        for i in 0 .. (map.Height - 1) do
            for j in 0 .. (map.Width - 1) -> (map.IsLowPoint i j, (i, j))
    }
    |> Seq.filter fst
    |> Seq.map snd
    |> Seq.map (fun (line, col) -> map.getBasinSize line col)
    |> Seq.toArray
    |> Array.sortDescending
    |> Array.take 3
    |> fun arr -> arr.[0] * arr.[1] * arr.[2]
