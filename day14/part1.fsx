open System.IO

type Position = int * int
type Robot = Position * Position
type Quadrant =
    | Q1
    | Q2
    | Q3
    | Q4

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
    let position = (px + t * vx, py + t * vy)
    bounds position



let quadrant ((width, height): Position) (x, y): Option<Quadrant> =
    match (x, y) with
    | _ when x < width / 2 && y < height / 2 -> Some Q1
    | _ when x < width / 2 && y > height / 2 -> Some Q2
    | _ when x > width / 2 && y < height / 2 -> Some Q3
    | _ when x > width / 2 && y > height / 2 -> Some Q4
    | _ -> None

let solve (robots: Robot list) =
    let width, height = map
    robots
    |> List.map (walk 100)
    |> List.countBy (quadrant (width, height))
    |> List.filter (fun (k, _) -> k.IsSome)
    |> List.map snd
    |> List.reduce (*)


let result =
    File.ReadAllLines "input"
    |> Array.map parse
    |> Array.choose id
    |> Array.toList
    |> solve
    |> printfn "Part 1: %A"
