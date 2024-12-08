type Antenna = { x: int; y: int; symbol: char }

let parseGrid (grid: string[]) : Antenna list =
    grid
    |> Array.mapi (fun x row ->
        row
        |> Seq.mapi (fun y ch -> if ch <> '.' then Some { x = x; y = y; symbol = ch } else None)
        |> Seq.choose id
        |> Seq.toList
    )
    |> List.concat

let areSameFrequency (a1: Antenna) (a2: Antenna) : bool =
    a1.symbol = a2.symbol

let findAntinodes (grid: string[]) (a1: Antenna) (a2: Antenna) : (int * int) list =
    if areSameFrequency a1 a2 then
        let dx = a2.x - a1.x
        let dy = a2.y - a1.y

        let candidates = [
            (a1.x + 2 * dx, a1.y + 2 * dy)
            (a2.x - 2 * dx, a2.y - 2 * dy)
        ]

        candidates
        |> List.filter (fun (x, y) ->
            x >= 0 && x < grid.[0].Length &&
            y >= 0 && y < grid.Length
        )
    else
        []

let findAllAntinodes (grid: string[]) : (int * int) list =
    let antennas = parseGrid grid
    antennas
    |> List.collect (fun a1 ->
        antennas
        |> List.collect (fun a2 ->
            if a1 <> a2 then
                findAntinodes grid a1 a2
            else []
        )
    )
    |> List.distinctBy (fun (x, y) -> (x, y))

let countAntinodes (grid: string[]) : int =
    findAllAntinodes grid |> List.length

let input = "............
........0...
.....0......
.......0....
....0.......
......a.....
............
............
........a...
.........a..
............
............"

countAntinodes (System.IO.File.ReadAllLines("input")) |> printfn "%d"
