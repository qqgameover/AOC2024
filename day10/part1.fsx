open System.IO

let parseMap (input: string) =
    input.Split('\n')
    |> Array.filter (fun line -> not (System.String.IsNullOrWhiteSpace(line)))
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun c -> int (string c)))

let isInBounds (map: int[][]) (x, y) =
    x >= 0 && y >= 0 && x < map.Length && y < map.[0].Length

let findTrailheads (map: int[][]) =
    [ for x in 0 .. map.Length - 1 do
        for y in 0 .. map.[x].Length - 1 do
            if map.[x].[y] = 0 then yield (x, y) ]

let rec dfs (map: int[][]) (visited: Set<(int * int)>) (x, y) =
    let directions = [ (0, 1); (1, 0); (0, -1); (-1, 0) ]

    directions
    |> List.fold (fun (reachableNines: int, visited: Set<(int * int)>) (dx, dy) ->
        let nx, ny = x + dx, y + dy
        match isInBounds map (nx, ny) && not (visited.Contains((nx, ny))) with
        | true ->
            let currentHeight = map.[x].[y]
            let nextHeight = map.[nx].[ny]
            match nextHeight = currentHeight + 1 with
            | true ->
                let newVisited = visited.Add((nx, ny))
                match nextHeight = 9 with
                | true -> (reachableNines + 1, newVisited)
                | false ->
                    let (r, v) = dfs map newVisited (nx, ny)
                    (reachableNines + r, v)
            | false -> (reachableNines, visited)
        | false -> (reachableNines, visited)
    ) (0, visited)

let calculateTrailheadScore (map: int[][]) (startX, startY) =
    fst (dfs map (Set.singleton (startX, startY)) (startX, startY))

let sumOfTrailheadScores (map: int[][]) =
    let trailheads = findTrailheads map
    trailheads |> List.sumBy (calculateTrailheadScore map)


let input = File.ReadAllText "input"

let map = parseMap input
let result = sumOfTrailheadScores map
printfn "Sum of trailhead scores: %d" result
