let input = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

let (<|>) (x: int64) (y: int64) =
    int64 (string x + string y)
let operators = [(+); (*)]
let coolerOperators = [(<|>); (+); (*)]

let gen n ops =
    let rec gen' rem =
        match rem with
        | 0 -> [[]]
        | _ ->
            [ for op in ops do
                for tail in gen' (rem - 1) do
                    yield op :: tail ]
    gen' n

let eval (numbers: int64 list) (ops: (int64 -> int64 -> int64) list) =
    let rec eval' numbers ops =
        match numbers, ops with
        | [n], [] -> n
        | n1 :: n2 :: tail, op :: opsTail -> eval' ((op n1 n2) :: tail) opsTail
        | _ -> failwithf "Mismatched numbers (%A) and operators (%A)" numbers ops
    eval' numbers ops

let canAchieveTarget (numbers: int64 list) (target: int64) (operators: (int64 -> int64 -> int64) list) =
    gen (List.length numbers - 1) operators
    |> List.exists (fun ops -> eval numbers ops = target)

let parseLine (line: string) =
    match line.Split(':') |> Array.map (fun part -> part.Trim()) with
    | [| sum; numbers |] ->
        let parsedSum = int64 sum
        let parsedNumbers =
            numbers.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int64
            |> List.ofArray
        Some (parsedSum, parsedNumbers)
    | _ -> None

let parseInput (input: string) =
    input.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.choose parseLine

let results =
    parseInput (System.IO.File.ReadAllText "input")
    |> Array.map (fun (sum, numbers) ->
        let result = canAchieveTarget numbers sum coolerOperators
        (sum, numbers, result))

let answer =
    results
    |> Array.filter (fun (_, _, result) -> result)
    |> Array.sumBy (fun (sum, _, _) -> sum)
