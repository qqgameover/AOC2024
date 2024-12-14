let stopWatch = System.Diagnostics.Stopwatch.StartNew()
open System.IO

type Position = int * int
type Robot = Position * Position

let parse (line: string) : Robot option =
    try
        let regex = System.Text.RegularExpressions.Regex(@"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
        let m = regex.Match(line)
        if m.Success then
            let x = int m.Groups.[1].Value
            let y = int m.Groups.[2].Value
            let vx = int m.Groups.[3].Value
            let vy = int m.Groups.[4].Value
            Some ((x, y), (vx, vy))
        else
            None
    with
    | _ -> None

let map = (101, 103)

let bounds (x, y) =
    let gridSizeX, gridSizeY = map
    let wrap a size = ((a % size) + size) % size
    (wrap x gridSizeX, wrap y gridSizeY)

let walk (t: int) ((p, v): Robot) : Position =
    let (px, py) = p
    let (vx, vy) = v
    let move (px, py) = (px + t * vx, py + t * vy)
    (move >> bounds) (px, py)

let average (xs: int list) =
    (xs |> List.sum) / List.length xs

let variance (xs: int list) =
    let avg = average xs
    xs |> Seq.fold (fun acc x -> acc + (x - avg) * (x - avg)) 0

let generateTimeline (robots: Robot list) maxTime =
    [ for t in 0 .. maxTime -> robots |> List.map (walk t) ]

let findMinVariance timeline =
    timeline
    |> List.mapi (fun t positions ->
        let xs, ys = positions |> List.unzip
        let totalVariance = variance xs + variance ys
        (t, positions, totalVariance))
    |> List.minBy (fun (_, _, v) -> v)

let printState (time: int) (robots: Position list) =
    let width, height = map
    let grid =
        [ for y in 0 .. height - 1 do
            [ for x in 0 .. width - 1 do
                if List.contains (x, y) robots then 'R' else '.' ] ]
    printfn "\nTime: %d" time
    grid |> List.iter (fun row -> printfn "%s" (System.String.Concat row))

let solvePart2 (robots: Robot list) =
    let maxTime = 9000
    let timeline = generateTimeline robots maxTime
    let time, state, _ = findMinVariance timeline
    printState time state

let robots =
    File.ReadAllLines "input"
    |> Array.map parse
    |> Array.choose id
    |> Array.toList

printfn "Part 2:"
solvePart2 robots
stopWatch.Stop()
printfn "Execution time: %f" stopWatch.Elapsed.TotalMilliseconds
