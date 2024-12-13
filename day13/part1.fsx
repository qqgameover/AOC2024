open System.IO

//Part 2 i guess
let N = 10000000000000L

let solve (x1: int) (y1: int) (x2: int) (y2: int) (t1: int64) (t2: int64) =
    let a = float(t2 * int64 x2 - t1 * int64 y2) / float(x2 * y1 - x1 * y2)
    let b = float(t2 * int64 x1 - t1 * int64 y1) / float(x1 * y2 - x2 * y1)

    if (a <> floor a || b <> floor b) || ((a > 100.0 || b > 100.0)) then
        0L
    else
        int64(a * 3.0 + b)


let solve2 (x1: int) (y1: int) (x2: int) (y2: int) (t1: int64) (t2: int64) =
    let t1 = t1 + N
    let t2 = t2 + N

    let a = float(t2 * int64 x2 - t1 * int64 y2) / float(x2 * y1 - x1 * y2)
    let b = float(t2 * int64 x1 - t1 * int64 y1) / float(x1 * y2 - x2 * y1)

    if (a <> floor a || b <> floor b)  then
        0L
    else
        int64(a * 3.0 + b)

let solvePuzzles (filename: string) =
    let input = File.ReadAllText(filename)
    let blocks = input.Split("\n\n")

    let parseBlock (block: string) =
        let nums =
            block.Split('\n')
            |> Array.collect (fun line ->
                System.Text.RegularExpressions.Regex.Matches(line, @"\d+")
                |> Seq.map (fun m -> int m.Value)
                |> Array.ofSeq
            )

        match nums with
        | [|x1; y1; x2; y2; t1; t2|] ->
            (solve x1 y1 x2 y2 (int64 t1) (int64 t2),
             solve2 x1 y1 x2 y2 (int64 t1) (int64 t2))
        | _ -> failwith "Invalid input block"

    blocks
    |> Array.map parseBlock
    |> Array.fold (fun (p1, p2) (part1Sol, part2Sol) -> (p1 + part1Sol, p2 + part2Sol)) (0L, 0L)

let p1, p2 = solvePuzzles "input"
printfn "Part 1: %d" p1
printfn "Part 2: %d" p2
