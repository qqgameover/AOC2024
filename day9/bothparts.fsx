open System

let parse (input: string) =
    input.ToCharArray()
    |> Array.map (fun ch -> int (System.Char.GetNumericValue(ch)))
    |> Array.toList

let insertNtimes value times l =
    if times > 0 then
        List.init times (fun _ -> value) @ l
    else
        l

let createDiskMap (input: int list) =
    let rec foldPairs input id acc =
        match input with
        | [] -> acc
        | x::ys::lst ->
            let validYs = max 0 ys
            let newAcc = insertNtimes id x acc
            let addedZeros = insertNtimes -1 validYs newAcc
            foldPairs lst (id + 1) addedZeros
        | [x] ->
            insertNtimes id x acc

    foldPairs input 0 [] |> List.toArray |> Array.rev

let moveFiles disk =
    let rec compactPass moved disk =
        let freeIdx = Array.tryFindIndex ((=) -1) disk
        let lastFileIdx = Array.tryFindIndexBack ((<>) -1) disk

        match freeIdx, lastFileIdx with
        | None, _
        | _, None -> disk
        | Some freeIdx, Some fileIdx when fileIdx <= freeIdx ->
            if moved then disk
            else compactPass moved disk
        | Some freeIdx, Some fileIdx ->
            let newDisk = Array.copy disk
            newDisk.[freeIdx] <- newDisk.[fileIdx]
            newDisk.[fileIdx] <- -1
            compactPass true newDisk

    compactPass false disk

//p2
//------------------------------

let identifyFiles (disk: int[]) =
    disk
    |> Array.mapi (fun idx block -> (idx, block))
    |> Array.groupBy snd
    |> Array.filter (fun (fileId, _) -> fileId >= 0)
    |> Array.map (fun (fileId, positions) ->
        let sortedPositions = positions |> Array.map fst |> Array.sort
        (fileId, sortedPositions.[0], sortedPositions.[sortedPositions.Length - 1]))
    |> Array.sortByDescending (fun (fileId, _, _) -> fileId)

let moveWholeFile (disk: int[]) (fileId: int) (startPos: int) (endPos: int) =
    let fileSize = endPos - startPos + 1

    printfn "Moving file %d from %d to %d" fileId startPos endPos
    let freeSpaceStart =
        disk
        |> Array.mapi (fun idx _ -> idx)
        |> Array.filter (fun idx ->
            idx + fileSize <= disk.Length &&
            Array.sub disk idx fileSize |> Array.forall ((=) -1)
        )
        |> Array.tryFind (fun freeIdx -> freeIdx < startPos)

    match freeSpaceStart with
    | None -> disk
    | Some freeIdx ->
        disk
        |> Array.mapi (fun idx block ->
            match block with
            | b when idx >= startPos && idx <= endPos -> -1
            | b when idx >= freeIdx && idx < freeIdx + fileSize -> fileId
            | _ -> block
        )

// Functional file movement across all files
let moveFilesByIdWhole (disk: int[]) =
    let fileInfo = identifyFiles disk

    fileInfo
    |> Array.fold (fun currentDisk (fileId, startPos, endPos) ->
        moveWholeFile currentDisk fileId startPos endPos
    ) disk

// Checksum calculation remains the same
let calculateChecksum disk =
    disk
    |> Array.mapi (fun index block ->
        if block >= 0 then int64 index * int64 block else 0L)
    |> Array.sum

let testinput = "2333133121414131402"

let result =
    parse (System.IO.File.ReadAllText "input")
    |> createDiskMap
    |> moveFilesByIdWhole
    |> calculateChecksum
    |> printfn "%A"
