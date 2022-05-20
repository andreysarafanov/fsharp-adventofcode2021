module Code.day14.solution_2

open Newtonsoft.Json

type PairsMap = Map<char*char, int64>
type Rules = Map<char*char, char>

let parseInput (input: string) =
    let parts = input.Split("\n\n")
    let initialString = parts.[0]
    let addRule (allRules: Rules) (rule: string) =
        let result = allRules |> Map.add (rule.[0], rule.[1]) rule.[6]
        result
    
    let rules = parts.[1].Split("\n") |> Array.fold addRule Map.empty
    
    let addToPairsMap (map: PairsMap) (i: int) =
        let pair = initialString.[i], initialString.[i+1]
        match map |> Map.tryFind pair with
        | Some v -> map |> Map.add pair (v+1L)
        | None -> map |> Map.add pair 1
        
    let pairsMap = [|0..initialString.Length-2|] |> Array.fold addToPairsMap Map.empty
    
    initialString, pairsMap, rules
    
let applyRules (rules: Rules) (pairsMap: PairsMap) =
    let addPair key valueToAdd map =
        match map |> Map.tryFind key with
        | Some value -> map |> Map.add key (value + valueToAdd)
        | None -> map |> Map.add key valueToAdd

    pairsMap |> Map.toSeq |> Seq.fold (
        fun map (k, v) ->
            let firstLetter = fst k
            let secondLetter = snd k
            let newLetter = rules |> Map.find k
            map |> addPair (firstLetter, newLetter) v |> addPair (newLetter, secondLetter) v
        )
        Map.empty

let pairsMapToLettersMap (pairsMap: PairsMap) (initialString: string) =    
    let addLetterToLetterMap letter count letterMap =
        match letterMap |> Map.tryFind letter with
        | Some v -> letterMap |> Map.add letter (v + count)
        | None -> letterMap |> Map.add letter count
        
    let addPairToLetterMap letterMap (pair, count) =
        letterMap |> addLetterToLetterMap (fst pair) count |> addLetterToLetterMap (snd pair) count
        
    let increaseCharByOne c map =
        let newCount = (map |> Map.find c) + 1L
        map |> Map.add c newCount        
    
    pairsMap
    |> Map.toSeq
    |> Seq.fold addPairToLetterMap Map.empty
    |> increaseCharByOne initialString.[0]
    |> increaseCharByOne initialString.[initialString.Length - 1]
    |> Map.map (fun k v -> v / 2L)
    
let getMinAndMaxDiff map =
    let max = map |> Map.toSeq |> Seq.maxBy snd |> snd
    let min = map |> Map.toSeq |> Seq.minBy snd |> snd
    max - min
    
let part1 input =
    let initialString, initialPairs, rules = parseInput input
    let foldRules pairs _ =
//        let lettersMap = pairsMapToLettersMap pairs initialString
//        printfn $"%s{JsonConvert.SerializeObject(pairs)}"
        applyRules rules pairs
    let pairsMap = [|0..9|] |> Array.fold foldRules initialPairs
    let lettersMap = pairsMapToLettersMap pairsMap initialString
    
    getMinAndMaxDiff lettersMap
    
let part2 input =
    let initialString, initialPairs, rules = parseInput input
    let foldRules pairs _ =
//        let lettersMap = pairsMapToLettersMap pairs initialString
//        printfn $"%s{JsonConvert.SerializeObject(pairs)}"
        applyRules rules pairs
    let pairsMap = [|0..39|] |> Array.fold foldRules initialPairs
    let lettersMap = pairsMapToLettersMap pairsMap initialString
    
    getMinAndMaxDiff lettersMap