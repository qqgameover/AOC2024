let readGrid (input: string) =
    input.Split('\n')
    |> Array.filter (fun line -> not (System.String.IsNullOrWhiteSpace(line)))
    |> Array.map (fun line -> line.ToCharArray())

let input = System.IO.File.ReadAllText("input") |> readGrid
let height = input.Length
let width = input[0].Length

let map =
    [ for r in 0 .. height - 1 do
          for c in 0 .. width - 1 do
              (r, c), input[r][c] ]
    |> Map.ofList

let rec flood visited (r, c) =
    if Set.contains (r, c) visited then
        visited, 0
    else
        let adjacentInRegion =
            [ (r - 1, c); (r + 1, c); (r, c - 1); (r, c + 1) ]
            |> List.filter (fun (r, c) -> r >= 0 && c >= 0 && r < height && c < width)
            |> List.filter (fun pt -> map[r, c] = map[pt])

        adjacentInRegion
        |> List.fold
            (fun (newVisited, perimeter) neighbor ->
                let newVisited, newPerimeter = flood newVisited neighbor
                newVisited, perimeter + newPerimeter)
            (Set.add (r, c) visited, 4 - (List.length adjacentInRegion))

let regions =
    map
    |> Map.toSeq
    |> Seq.fold
        (fun (visited, regions) (pt, _) ->
            match pt with
            | _ when Set.contains pt visited -> visited, regions
            | _ ->
                let newRegion, perimeter = flood Set.empty pt
                Set.union visited newRegion, (newRegion, perimeter) :: regions)
        (Set.empty, [])
    |> snd

let part1 =
    regions |> List.sumBy (fun (region, perimeter) -> Set.count region * perimeter)

type Direction =
    | Up
    | Right
    | Down
    | Left

let directions = [| Up; Right; Down; Left |]

let move dir (r, c) =
    match dir with
    | Up -> r - 1, c
    | Right -> r, c + 1
    | Down -> r + 1, c
    | Left -> r, c - 1

let rec fillSide region dir visited (r, c) =
    match dir with
    | Up
    | Down -> Some(move Right (r, c))
    | Left
    | Right -> Some(move Down (r, c))
    |> Option.filter (fun pt -> Set.contains pt region && not (Set.contains (move dir pt) region))
    |> Option.fold (fun newVisited neighbor -> fillSide region dir newVisited neighbor) (Set.add (r, c) visited)

let sidesFacing dir region =
    region
    |> Set.toSeq
    |> Seq.fold
        (fun (visited, size) pt ->
            match pt with
            | _ when Set.contains pt visited -> visited, size
            | _ when Set.contains (move dir pt) region -> Set.add pt visited, size
            | _ -> fillSide region dir visited pt, size + 1)
        (Set.empty, 0)
    |> snd


let sides region =
    directions |> Seq.sumBy (fun dir -> sidesFacing dir region)

let part2 =
    regions |> List.sumBy (fun (region, _) -> Set.count region * sides region)


printfn "Part 1: %d" part1
printfn "Part 2: %d" part2
