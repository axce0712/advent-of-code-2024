open System.IO

type Rules = Map<int, int[]>

type PageNumbers = int[]

let parse (input: string) : Rules * PageNumbers[] =
    let rulesPart, pageNumbersPart =
        let parts = input.Split("\n\n")
        parts[0], parts[1]

    let rules =
        rulesPart.Split('\n')
        |> Seq.map (fun rule ->
            let parts = rule.Split('|')
            int parts.[0], int parts.[1])
        |> Seq.groupBy fst
        |> Seq.map (fun (key, values) -> key, values |> Seq.map snd |> Seq.toArray)
        |> Map.ofSeq

    let pageNumbers =
        pageNumbersPart.Split('\n')
        |> Seq.map (fun line -> line.Split(',') |> Seq.map int |> Seq.toArray)
        |> Seq.toArray

    rules, pageNumbers

let sortByRules (rules: Rules) (pageNumbers: PageNumbers) : PageNumbers =
    let copy = Array.copy pageNumbers

    copy
    |> Array.sortInPlaceWith (fun x y ->
        match Map.tryFind x rules, Map.tryFind y rules with
        | Some xs, _ when xs |> Array.contains y -> -1
        | _, Some ys when ys |> Array.contains x -> 1
        | _ -> 0)

    copy

let middlePageNumber (pageNumbers: PageNumbers) = pageNumbers[pageNumbers.Length / 2]

let solve (predicate: PageNumbers -> PageNumbers -> bool) (rules: Rules) (pageNumbers: PageNumbers[]) : int =
    pageNumbers
    |> Array.sumBy (fun xs ->
        let sorted = sortByRules rules xs
        if predicate xs sorted then middlePageNumber sorted else 0)

let partOne rules pageNumbers =
    solve (Array.forall2 (=)) rules pageNumbers

let partTwo rules pageNumbers =
    solve (fun xs ys -> not (Array.forall2 (=) xs ys)) rules pageNumbers

let input = File.ReadAllText("./input/day05.txt")
let rules, pageNumbers = parse input
partOne rules pageNumbers
partTwo rules pageNumbers
