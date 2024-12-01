open System.IO


let splitIntoLists filePath =
    File.ReadLines(filePath)
    |> Seq.map (fun line ->
        let parts = line.Split("  ")
        (int parts.[0], int parts.[1])
    )
    |> Seq.toList

let calculateTotalDistance filePath =
    let list1, list2 =
        splitIntoLists filePath
        |> List.unzip

    let sorted1 = List.sort list1
    let sorted2 = List.sort list2

    List.map2 (fun x y -> abs (x - y)) sorted1 sorted2
    |> List.sum


let calcSimilarityScore filePath =
    let leftList, rightList =
        splitIntoLists filePath
        |> List.unzip

    let freqMap =
        rightList
        |> List.groupBy id
        |> List.map (fun (key, group) -> (key, List.length group))
        |> Map.ofList

    leftList
    |> List.sumBy (fun num ->
                   match Map.tryFind num freqMap with
                    | Some count -> num * count
                    | None -> 0)

calcSimilarityScore "input" |> printfn "%A"
