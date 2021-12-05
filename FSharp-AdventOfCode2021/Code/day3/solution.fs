module Code.day3.solution

type Counter = { Ones: int; Zeroes: int }

type BitCriteria =
    | MostCommon
    | LeastCommon

let parseInput (input: string) = input.Split("\n")

let getCounterForBit (lines: string list) i =
    let addToCounter counter (line: string) =
        if (line.[i] = '1') then
            { counter with Ones = counter.Ones + 1 }
        else
            { counter with
                  Zeroes = counter.Zeroes + 1 }

    List.fold addToCounter { Ones = 0; Zeroes = 0 } lines

let addToCounters (counters: Counter list) (line: string) =
    let addToCounter idx counter =
        if (line.[idx] = '1') then
            { counter with Ones = counter.Ones + 1 }
        else
            { counter with
                  Zeroes = counter.Zeroes + 1 }

    counters |> List.mapi addToCounter

let buildRate (counters: Counter list) bitCriteria =
    let rec loop index sum =
        if index = counters.Length then
            sum
        else
            let counter = counters.[index]

            let shouldAddOne =
                counter.Ones < counter.Zeroes
                && bitCriteria = LeastCommon
                || counter.Ones > counter.Zeroes
                   && bitCriteria = MostCommon

            if shouldAddOne then
                loop (index + 1) (sum * 2 + 1)
            else
                loop (index + 1) (sum * 2)

    loop 0 0

let buildGammaRate (counters: Counter list) = buildRate counters MostCommon

let buildEpsilonRate (counters: Counter list) = buildRate counters LeastCommon

let isLineValid i symbol (line: string) = line.[i] = symbol

let filterLinesByBit (lines: string list) i bitCriteria =
    let counter = getCounterForBit lines i

    let shouldFilterOnes =
        counter.Ones < counter.Zeroes
        && bitCriteria = LeastCommon
        || counter.Ones >= counter.Zeroes
           && bitCriteria = MostCommon

    let requiredSymbol = if shouldFilterOnes then '1' else '0'

    lines
    |> List.filter (isLineValid i requiredSymbol)

let lastLineByCriteria (lines: string list) bitCriteria =
    let rec loop i (linesLeft: string list) =
//        printfn $"i=%d{i}, linesLeft=%A{linesLeft}"
        if linesLeft.Length = 1 then
            linesLeft.[0]
        else
            loop (i + 1) (filterLinesByBit linesLeft i bitCriteria)

    loop 0 lines

let part1 input =
    let lines = parseInput input |> Array.toList

    let finalCounters =
        [ for i in 0 .. (lines.[0].Length - 1) do
              yield (getCounterForBit lines i) ]

    let gammaRate, epsilonRate =
        buildGammaRate finalCounters, buildEpsilonRate finalCounters

    gammaRate * epsilonRate

let binStringToInt (str: string) =
    let mutable n = 0
    for c in str do
        n <- n * 2 + (int c - int '0')
    n

let part2 input =
    let lines = parseInput input |> Array.toList

    let rating1, rating2 =
        lastLineByCriteria lines MostCommon, lastLineByCriteria lines LeastCommon
    printfn $"rating1 = %s{rating1}, rating2 = %s{rating2}"
    (binStringToInt rating1) * (binStringToInt rating2)
