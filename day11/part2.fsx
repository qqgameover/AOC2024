open System.Collections.Generic

let parseMap (input: string) =
    input.Split(' ')
    |> Seq.map int64

let memoize f =
    let cache = Dictionary<(int64 * int), bigint>()
    fun x ->
        match cache.TryGetValue x with
        | true, value -> value
        | false, _ ->
            let value = f x
            cache.[x] <- value
            value

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
        | n -> blink stone |> Seq.sumBy (fun st -> blinks (st, n - 1))
    |> memoize

let input =
    System.IO.File.ReadAllText "input"
    |> parseMap

let part2 = input |> Seq.sumBy (fun st -> blinks (st, 75))

printfn "Part 2: %A" part2
