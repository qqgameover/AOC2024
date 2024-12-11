open System.Collections.Generic

type Lazy<'a> = System.Lazy<'a>

let parseMap (input: string) =
    input.Split(' ')
    |> Seq.map int64

let memoize f =
    let cache = Dictionary<(int64 * int), Lazy<bigint>>()
    fun x ->
        match cache.TryGetValue x with
        | true, value -> value.Value
        | false, _ ->
            let lazyValue = lazy (f x)
            cache.[x] <- lazyValue
            lazyValue.Value

let blink stone =
    match string stone with
    | "0" -> [ 1L ]
    | ds when ds.Length % 2 = 0 ->
        [ ds[.. ds.Length / 2 - 1]; ds[ds.Length / 2 ..] ]
        |> List.map int64
    | _ -> [ stone * 2024L ]

let rec blinks =
    fun (stone, times) ->
        match times with
        | 0 -> bigint 1
        | n ->
            blink stone
            |> Seq.map (fun st ->
                lazy (blinks (st, n - 1))
            )
            |> Seq.map (fun lazyVal -> lazyVal.Value)
            |> Seq.sum
    |> memoize

let input =
    System.IO.File.ReadAllText "input"
    |> parseMap
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let part2 = input |> Seq.sumBy (fun st -> blinks (st, 75))
stopWatch.Stop()

printfn "%f" stopWatch.Elapsed.TotalMilliseconds
printfn "Part 2: %A" part2
