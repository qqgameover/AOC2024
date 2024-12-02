open System.IO


let validRow nums =
    (nums |> Array.pairwise |> Array.forall (fun (a, b) -> b >= a) ||
     nums |> Array.pairwise |> Array.forall (fun (a, b) -> b <= a)) &&
    nums |> Array.pairwise |> Array.forall (fun (a, b) -> abs (a - b) <= 3 && a <> b)


let canBeMadeValid (nums: int array) =
    nums
    |> Seq.mapi (fun i _ ->
        Array.concat [nums.[..i-1]; nums.[i+1..]]
    )
    |> Seq.exists validRow

let matrix (input: string array) =
    input
    |> Array.filter (fun row ->
        row.Trim().Split(' ')
        |> Array.map int
        |> validRow)
    |> Array.length

let matrix' (input: string array) =
    input
    |> Array.filter (fun row ->
        let nums = row.Trim().Split(' ') |> Array.map int
        validRow nums || canBeMadeValid nums
    )
    |> Array.length



let input = File.ReadAllLines "input"
printfn "%A" (matrix' input)
