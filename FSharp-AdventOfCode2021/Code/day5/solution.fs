module Code.day5.solution

type Point = {
    X: int; Y: int
}
with
static member Parse (str: string) =
    let parts = str.Split(",")
    { X = int parts.[0]; Y = int parts.[1] }
end

type HorizontalLine = {Y: int; XLeft: int; XRight: int}
type VerticalLine = {X: int; YTop: int; YBottom: int}
type DiagonalLine = {X1: int; X2: int; Y1: int; Y2: int}
type Line =
    | Horizontal of HorizontalLine
    | Vertical of VerticalLine
    | Diagonal of DiagonalLine

let parseInput (input: string) countDiagonal =
    let lines = input.Split("\n")
    let parseLine (line: string) =
        let parts = line.Split(" -> ")
        let point1, point2 = Point.Parse parts.[0], Point.Parse parts.[1]
        let x1, y1, x2, y2 = point1.X, point1.Y, point2.X, point2.Y
        if x1 = x2 then
            let top, bot = if y1 < y2 then y1, y2 else y2, y1
            Some (Vertical {X = x1; YTop = top; YBottom = bot})
        elif y1 = y2 then
            let left, right = if x1 < x2 then x1, x2 else x2, x1
            Some (Horizontal {Y = y1; XLeft = left; XRight = right})
        elif countDiagonal then
            Some (Diagonal {X1 = x1; Y1 = y1; X2 = x2; Y2 = y2})
        else None
    lines |> Array.map parseLine |> Array.choose id |> Array.toList

let addPointToMap (map: Map<Point, int>) (point: Point) =
    match map.TryFind point with
    | Some value -> map.Add(point, value + 1)
    | None -> map.Add(point, 1)
let addLineToMap (map: Map<Point, int>) (line: Line) =
    match line with
    | Horizontal hor ->
        [for x in (hor.XLeft)..(hor.XRight) do yield {X = x; Y = hor.Y}]
        |> List.fold addPointToMap map
    | Vertical vert ->
        [for y in (vert.YTop)..(vert.YBottom) do yield {X = vert.X; Y = y}]
        |> List.fold addPointToMap map
    | Diagonal diag ->
        let {X1 = x1; Y1 = y1; X2 = x2; Y2 = y2} = diag
        let xLeft, yLeft, xRight, yRight = if x1 < x2 then x1, y1, x2, y2 else x2, y2, x1, y1
        let yDirection = if yLeft < yRight then 1 else -1
        [for diff in 0..(xRight - xLeft) do yield {X = xLeft + diff; Y = yLeft + diff * yDirection}]
        |> List.fold addPointToMap map

let part1 input =
    let lines = parseInput input false
    let map = lines |> List.fold addLineToMap Map.empty
    map |> Map.toList |> List.map snd |> List.filter (fun value -> value >= 2) |> fun lst -> lst.Length
    
let part2 input =
    let lines = parseInput input true
    let map = lines |> List.fold addLineToMap Map.empty
    map |> Map.toList |> List.map snd |> List.filter (fun value -> value >= 2) |> fun lst -> lst.Length