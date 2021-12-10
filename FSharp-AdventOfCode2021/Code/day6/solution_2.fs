module Code.day6.solution_2

type FishSchool(countByTimeToChild: Map<int,int64>) =
    let countByTimeToChild = countByTimeToChild
    member this.TotalCount = countByTimeToChild |> Map.toSeq |> Seq.map snd |> Seq.sum
    member this.GetAt day =
        match countByTimeToChild.TryFind day with
        | Some v -> v
        | None -> 0L
    member this.Print = printfn $"%A{countByTimeToChild}" 
    member this.NextDay =
        let rec loop day (school: Map<int,int64>) =
            if day = 9 then school
            elif day = 8 then loop (day + 1) (school.Add(8, (this.GetAt 0)))
            elif day = 6 then loop (day + 1) (school.Add(6, (this.GetAt 7) + (this.GetAt 0)))
            else loop (day + 1) (school.Add(day, this.GetAt (day + 1)))
        let newMap = loop 0 Map.empty
        FishSchool newMap
    static member Parse(input: string) =
        let folder (map: Map<int,int64>) (value: int) =
            match map.TryFind value with
            | Some count -> map.Add(value, count + 1L)
            | None -> map.Add(value, 1L)
        let map = input.Split(",") |> Array.map int |> Array.fold folder Map.empty
        FishSchool map
    
let part1 input =
    let initialFishSchool = FishSchool.Parse input
    let days = 256
    let rec loop day (fishSchool: FishSchool) =
//        printfn $"day=%d{day}, fish count = %A{fishSchool.TotalCount}"
        fishSchool.Print
        if day < days
        then loop (day + 1) (fishSchool.NextDay)
        else fishSchool.TotalCount
    loop 0 initialFishSchool