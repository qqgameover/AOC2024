let input =
    System.IO.File.ReadAllLines "input"
    |> List.ofSeq

let grid = input
let maxRow = (grid |> Seq.length) - 1
let maxColumn = (grid[0] |> Seq.length) - 1

let obstacles =
    [ for (rowNumber, row) in grid |> Seq.indexed do
          for (columnNumber, cell) in row |> Seq.indexed do
              if cell = '#' then yield (rowNumber, columnNumber)
    ]
    |> Set.ofSeq

let guard =
    [ for (rowNumber, row) in grid |> Seq.indexed do
          for (columnNumber, cell) in row |> Seq.indexed do
              if cell = '^' then yield (rowNumber, columnNumber)
    ] |> List.head

let directions = [| (-1, 0); (0, 1); (1, 0); (0, -1) |]

let turnRight dir =
    let newIndex = (Array.findIndex ((=) dir) directions + 1) % 4
    directions.[newIndex]

let inBounds (r, c) =
    0 <= r && r <= maxRow && 0 <= c && c <= maxColumn

let walk obstacles (r, c, dir) =
    let (dr, dc) = dir
    let inFront = (r + dr, c + dc)

    if obstacles |> Set.contains inFront then
        (r, c, turnRight dir)
    else
        (r + dr, c + dc, dir)

let rec patrol obstacles (r, c, dir, visited, finishReason) =
    match (r, c) |> inBounds with
    | false -> (r, c, dir, visited, Some "OutOfBounds")
    | true ->
        let (nextR, nextC, nextDir) = walk obstacles (r, c, dir)
        match visited |> Set.contains (nextR, nextC, nextDir) with
        | true -> (r, c, dir, visited, Some "Loop")
        | false -> patrol obstacles (nextR, nextC, nextDir, Set.add (r, c, dir) visited, finishReason)

let init: int * int * (int * int) * Set<(int * int * (int * int))> * option<string> =
    (fst guard, snd guard, directions.[0], Set.empty, None)

let finalState = patrol obstacles init

let visited =
    finalState |> fun (_, _, _, visited, _) ->
        visited
        |> Seq.map (fun (r, c, _) -> (r, c))
        |> List.ofSeq
        |> List.distinct

let runs =
    visited
    |> List.map (fun loc -> obstacles |> Set.add loc)
    |> List.map (fun obstacles -> patrol obstacles init)
    |> List.filter (fun (_, _, _, _, finishReason) -> finishReason = Some "Loop")

runs |> Seq.length |> printfn "%d"
