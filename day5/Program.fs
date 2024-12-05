open System.IO

let example = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

let parseLine (line: string) =
    let parts = line.Split('|')
    (int parts.[0], int parts.[1])

let getInstructions (input: string) =
    input.Split('\n')
    |> Seq.filter(fun x -> x.Contains("|"))
    |> Seq.map parseLine

let getData (input: string) =
    input.Split('\n')
    |> Seq.filter(fun x -> x.Contains(",") && not (x.Contains("|")))
    |> Seq.map (fun x -> x.Split(','))
    |> Seq.map (fun x -> x |> Array.map int)
    |> Seq.map Array.toSeq

let buildRuleMap instructions =
    instructions
    |> Seq.fold (fun acc (a, b) ->
        let updated =
            Map.tryFind a acc
            |> Option.defaultValue Set.empty
            |> Set.add b
        Map.add a updated acc
    ) Map.empty

let isValidUpdate ruleMap update =
    let pages = Seq.toList update
    pages
    |> List.indexed
    |> List.forall (fun (i, page) ->
        let mustComeAfter = Map.tryFind page ruleMap |> Option.defaultValue Set.empty
        let earlierPages = pages |> List.take i
        earlierPages |> List.forall (fun p -> not (Set.contains p mustComeAfter))
    )

let findMiddlePage update =
    let pages = Seq.toList update
    pages.[pages.Length / 2]

let solve instructions data =
    let rules = buildRuleMap instructions
    data
    |> Seq.filter (fun update -> isValidUpdate rules update)
    |> Seq.map findMiddlePage
    |> Seq.sum

let input = System.IO.File.ReadAllText("input")
let instructions = getInstructions input
let data = getData input

let res = solve instructions data
printfn "%d" res

// ------------------- Part 2 ---------------------

let topologicalSort pages ruleMap =
    let graph =
        pages
        |> Seq.fold (fun acc page ->
            let mustComeAfter = Map.tryFind page ruleMap |> Option.defaultValue Set.empty
            Set.intersect mustComeAfter (Set.ofSeq pages)
            |> Set.fold (fun acc dep ->
                let existingDependencies =
                    match Map.tryFind dep acc with
                    | Some deps -> deps
                    | None -> []
                Map.add dep (page :: existingDependencies) acc
            ) acc
        ) Map.empty

    let visited = System.Collections.Generic.HashSet<int>()
    let result = System.Collections.Generic.Stack<int>()

    let rec visit node =
        if not (visited.Contains(node)) then
            visited.Add(node) |> ignore
            for neighbor in Map.tryFind node graph |> Option.defaultValue [] do
                visit neighbor
            result.Push(node)

    pages |> Seq.iter visit
    result |> Seq.toList

let correctInvalidUpdates ruleMap data =
    data
    |> Seq.filter (fun update -> not (isValidUpdate ruleMap update))
    |> Seq.map (fun update ->
        let pages = Seq.toList update
        topologicalSort pages ruleMap
    )

// Find the Middle Page and Sum
let sumMiddlePages updates =
    updates
    |> Seq.map (fun update ->
        let pages = Seq.toList update
        pages.[pages.Length / 2]
    )
    |> Seq.sum

let solvePart2 instructions data =
    let ruleMap = buildRuleMap instructions
    let invalidUpdates = correctInvalidUpdates ruleMap data
    sumMiddlePages invalidUpdates

let result = solvePart2 instructions data
printfn "%d" result
