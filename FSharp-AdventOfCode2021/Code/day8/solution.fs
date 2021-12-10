module Code.day8.solution

open System

let part1 (input: string) =
    let countRequiredSymbolsForLine (line: string) =
        line.Split(" | ").[1].Split(" ")
        |> Array.filter
            (fun s ->
                s.Length = 2
                || s.Length = 3
                || s.Length = 4
                || s.Length = 7)
        |> Array.length

    input.Split("\n")
    |> Array.sumBy countRequiredSymbolsForLine

let permutations =
    Map [ (0b1110111, '0')
          (0b0010010, '1')
          (0b1011101, '2')
          (0b1011011, '3')
          (0b0111010, '4')
          (0b1101011, '5')
          (0b1101111, '6')
          (0b1010010, '7')
          (0b1111111, '8')
          (0b1111011, '9')]
let allowedPermutations = permutations |> Map.toList |> List.map fst |> Set.ofList

let getSumByWordWithMap (map: Map<char, int>) (word: char []) = word |> Array.sumBy (fun c -> map.[c])
let areMapAndWordAllowed (map: Map<char, int>) (word: char []) =
    let sum = getSumByWordWithMap map word
    allowedPermutations |> Set.contains sum

let getSymbolByWordAndMap (map: Map<char, int>) (word: char []) =
    let sum = getSumByWordWithMap map word
    permutations.[sum]

let getAllMaps maxDepth =
    let allPowersOf2 =
        [ for pwr in 0 .. maxDepth do
              yield pown 2 pwr ]

    let rec loop (depth: int) (maps: Map<char, int> seq) =
        if depth > maxDepth then maps
        else
            let nextChar = char ((int 'a') + depth)

            let addNextLevelToMap (map: Map<char, int>) =
                let allPowersLeft =
                    allPowersOf2
                    |> List.except (map |> Map.toList |> List.map snd)

                allPowersLeft
                |> List.map (fun pwr -> map.Add(nextChar, pwr))

            let newMaps = maps |> Seq.collect addNextLevelToMap
            loop (depth + 1) newMaps

    loop 0 (seq { Map.empty })
    
let getCorrectMapForLine (leftPart: string) =
    let words = leftPart.Split(" ") |> Array.map (fun word -> word.ToCharArray())
    let mapsSeq = getAllMaps 6
    mapsSeq |> Seq.find (fun map -> words |> Array.forall (areMapAndWordAllowed map))
    
let getOutputForLine (rightPart: string) (map: Map<char, int>) =
    let words = rightPart.Split(" ") |> Array.map (fun word -> word.ToCharArray())
    let chars = words |> Array.map (getSymbolByWordAndMap map)
    int (String.Join("", chars))
    
let getResultByLine (line: string) =
    let parts = line.Split(" | ")
    let map = getCorrectMapForLine parts.[0]
//    printfn $"right = %A{map}"
//    printfn $"map = %A{map}"
    getOutputForLine parts.[1] map

let part2 (input: string) =
    input.Split("\n") |> Array.sumBy getResultByLine