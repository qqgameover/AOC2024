open System.Text.RegularExpressions
open System.IO

let findAllMul (input: string) =
    let pattern = @"mul\((\d+),(\d+)\)"
    let matches = Regex.Matches(input, pattern)

    matches
    |> Seq.cast<Match>
    |> Seq.sumBy (fun m ->
        let x = int m.Groups.[1].Value
        let y = int m.Groups.[2].Value
        x * y
    )

let findSomeMul (input: string) =
    let pattern = @"do\(\)|don't\(\)|mul\(\d+,\d+\)"
    let matches = Regex.Matches(input, pattern)

    matches
    |> Seq.cast<Match>
    |> Seq.fold (fun (isProcessing, sum) m ->
        match m.Value with
        | "do()" -> (true, sum)
        | "don't()" -> (false, sum)
        | _ ->
            if isProcessing && m.Value.StartsWith("mul") then
                let parts = Regex.Match(m.Value, @"mul\((\d+),(\d+)\)")
                let x = int parts.Groups.[1].Value
                let y = int parts.Groups.[2].Value
                (true, sum + x * y)
            else
                (false, sum)
    ) (true, 0)
    |> snd

let input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let count = findSomeMul (File.ReadAllText("input"))
printfn "Count of mul(x,y): %d" count
