open System.IO
let directions =
    [|
        (0, -1);
        (1, 0);
        (0, 1);
        (-1, 0);
    |]


let step (x, y) (dx, dy) = (x + dx, y + dy)

let parseInput inp =
    let grid = inp |> Array.map Array.ofSeq
    let guardPos, guardDir =
        grid
        |> Array.mapi (fun y row ->
            row
            |> Array.mapi (fun x cell ->
                match cell with
                | '^' -> Some ((x, y), 0)
                | '>' -> Some ((x, y), 1)
                | 'v' -> Some ((x, y), 2)
                | '<' -> Some ((x, y), 3)
                | _ -> None
            )
            |> Array.choose id
        )
        |> Array.collect id
        |> Array.head

    guardPos, guardDir, grid

let turnRight dir = (dir + 1) % directions.Length

let simulate (startPos: int * int) (startDir: int) (grid: char[][]) =
    let rows = grid.Length
    let cols = grid.[0].Length
    let inBounds (x, y) = x >= 0 && y >= 0 && x < cols && y < rows

    let rec patrol visited (x, y) dir =
        let (dx, dy) = directions.[dir]
        let front = step (x, y) (dx, dy)
        match inBounds front with
        | false ->
            visited
        | true ->
            match inBounds front, grid.[snd front].[fst front] with
            | false, _ ->
                visited
            | true, '#' ->
                let newDir = turnRight dir
                patrol visited (x, y) newDir
            | true, '.' ->
                patrol (Set.add front visited) front dir
            | true, ('^' | '>' | 'v' | '<') ->
                patrol (Set.add front visited) front dir
            | _, _ ->
                failwith "Unexpected cell state"
    patrol Set.empty startPos startDir

let inputArray = File.ReadAllLines "input"
let startPos, startDir, grid = parseInput inputArray
let visitedPositions = simulate startPos startDir grid

printfn "The guard visits %d distinct positions." (Set.count visitedPositions)

