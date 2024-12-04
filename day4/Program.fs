open System.IO

let directions = [
    (0, 1);(1,  0);(0, -1);(-1,  0)
    (1, 1);(1, -1);(-1, 1);(-1, -1)
]

let findWord (word: string) (grid: char[,]) =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid
    let wordLength = word.Length

    let isInBounds r c = r >= 0 && c >= 0 && r < rows && c < cols

    let isMatch r c (dr, dc) =
        [0 .. wordLength - 1]
        |> List.forall (fun i ->
            let nr, nc = r + dr * i, c + dc * i
            isInBounds nr nc && grid.[nr, nc] = word.[i]
        )

    seq {
        for r in 0 .. rows - 1 do
        for c in 0 .. cols - 1 do
        if grid.[r, c] = word.[0] then
            for direction in directions do
                if isMatch r c direction then yield 1
    }
    |> Seq.length



let readGridFromFile filePath =
    File.ReadAllLines(filePath)
    |> Array.map (fun line -> line.ToCharArray())
    |> array2D

let countWordInGrid filePath word =
    let grid = readGridFromFile filePath
    findWord word grid


let result = countWordInGrid "input" "XMAS"
printfn "The word 'XMAS' appears %d times in the grid." result

//Part 2

let input = System.IO.File.ReadAllLines("input") |> Seq.toList
let puzzle = input |> List.map List.ofSeq
let r = ((puzzle |> Seq.length) - 1)
let c = ((puzzle |> Seq.head) |> Seq.length) - 1
let lookup (puzzle: char list list ) (r,c) = puzzle[r][c]

let aLocations =
    seq {
        for row in 1..r-1 do
            for col in 1..c-1 do
                if lookup puzzle (row, col) = 'A' then
                    yield (row, col)
    }

let calcLoc (rl, cl) =
    let offset =
        [
            [(-1, -1);( 1, 1)]
            [( 1, -1);(-1, 1)]
        ]
    offset
    |> List.map (List.map (fun (ro, co) -> rl + ro, cl + co))

let lookups puzzle diagonals = diagonals |> List.map (List.map (lookup puzzle))

let makesPattern diagonals =
    [
        [['M';'S'];['M';'S']]
        [['M';'S'];['S';'M']]
        [['S';'M'];['M';'S']]
        [['S';'M'];['S';'M']]
    ]
    |> List.contains diagonals

let res =
    aLocations
    |> Seq.map calcLoc
    |> Seq.map (lookups puzzle)
    |> Seq.filter makesPattern
    |> Seq.length
    |> printfn "The pattern 'XMAS' appears %d times in the grid."

res
