open Code
open System.Diagnostics

type TimedOperation<'T> = {MillisecondsTaken:int64; Result:'T}

let timeOperation func = 
    let timer = Stopwatch()
    timer.Start()
    let returnValue = func()
    timer.Stop()
    { MillisecondsTaken=timer.ElapsedMilliseconds; Result=returnValue }

[<EntryPoint>]
let main argv =
    let operation unit =
        day9.solution.part2 day9.input.input
    let {Result = result; MillisecondsTaken = ms} = timeOperation operation
    printfn $"Total time taken: %d{ms}"
    printfn $"%A{result}"
    0