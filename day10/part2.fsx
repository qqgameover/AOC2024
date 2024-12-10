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

let rec dfsTrails (map: int[][]) (visited: Set<(int * int)>) (x, y) : int =
    let directions = [ (0, 1); (1, 0); (0, -1); (-1, 0) ]

    directions
    |> List.fold (fun acc (dx, dy) ->
        let nx, ny = x + dx, y + dy
        match isInBounds map (nx, ny) && not (visited.Contains((nx, ny))) with
        | true ->
            let currentHeight = map.[x].[y]
            let nextHeight = map.[nx].[ny]
            match nextHeight = currentHeight + 1 with
            | true ->
                let newVisited = visited.Add((nx, ny))
                match nextHeight = 9 with
                | true -> acc + 1
                | false -> acc + (dfsTrails map newVisited (nx, ny))
            | false -> acc
        | false -> acc
    ) 0

let calculateTrailheadRating (map: int[][]) (startX, startY) =
    dfsTrails map (Set.singleton (startX, startY)) (startX, startY)

let sumOfTrailheadRatings (map: int[][]) =
    let trailheads = findTrailheads map
    trailheads |> List.sumBy (calculateTrailheadRating map)

let input = File.ReadAllText "input"

let map = parseMap input
let result = sumOfTrailheadRatings map
printfn "Sum of trailhead ratings: %d" result
