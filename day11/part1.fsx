open System.Collections.Generic

let blinkOneStone (num: int64) =
    match num with
    | n when n.ToString().Length % 2 = 0 ->
        let str = n.ToString()
        let midpoint = str.Length / 2
        [|
            int64 (str.Substring(0, midpoint))
            int64 (str.Substring(midpoint))
         |]
    | 0L -> [|1L|]
    | 1L -> [|2024L|]
    | 999L -> [|2021976L|]
    | n -> [|n * 2024L|]

let blink iterations (initialSequence: seq<int64>) =
    let cache = Dictionary<int64, int64 array>()

    let rec computeBlinkOne num =
        if cache.ContainsKey(num) then
            cache.[num]
        else
            let result = blinkOneStone num
            cache.[num] <- result
            result

    let rec findCycle (current: seq<int64>) (seen: Dictionary<string, int>) iterations =
        if iterations = 0 then current
        else
            let transformed =
                current |> Seq.collect (fun n -> computeBlinkOne n |> Array.toSeq)

            let hash = String.concat "," (transformed |> Seq.map string)

            if seen.ContainsKey(hash) then
                let prevIteration = seen.[hash]
                let cycleLength = seen.Count - prevIteration
                let remainingIterations = iterations % cycleLength
                findCycle transformed seen remainingIterations
            else
                seen.[hash] <- seen.Count
                findCycle transformed seen (iterations - 1)

    findCycle initialSequence (Dictionary<string, int>()) iterations

let parseMap (input: string) =
    input.Split(' ')
    |> Seq.map int64

let realInput = System.IO.File.ReadAllText "input"

let result =
    parseMap realInput
    |> blink 25
    |> Seq.length

printfn "Result: %d" result
