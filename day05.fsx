open System
open System.Collections.Generic
open System.IO

let parse (lines: seq<string>) =
    let rules = Dictionary<int, List<int>>()
    let pageNumbers = List()
    let mutable isPageNumbersPart = false

    for line in lines do
        if not isPageNumbersPart then
            if line = "" then
                isPageNumbersPart <- true
            else
                let span: ReadOnlySpan<char> = line.AsSpan()
                let key = Int32.Parse(span.Slice(0, span.IndexOf('|')))
                let value = Int32.Parse(span.Slice(span.IndexOf('|') + 1))

                match rules.TryGetValue(key) with
                | true, values -> values.Add(value)
                | false, _ ->
                    let values = ResizeArray()
                    values.Add(value)
                    rules.Add(key, values)
        else
            let mutable span = line.AsSpan()
            let length = span.Count(',') + 1
            let values = Array.zeroCreate<int> length
            let mutable next = span.IndexOf(',')
            let mutable idx = 0
            while next >= 0 do
                let value = Int32.Parse(span.Slice(0, next))
                values[idx] <- value
                span <- span.Slice(next + 1)
                next <- span.IndexOf(',')
                idx <- idx + 1

            values[idx] <- Int32.Parse(span)
            pageNumbers.Add(values)

    rules, pageNumbers

type Rules = Dictionary<int, List<int>>

type PageNumbers = int[]

let sortByRules (rules: Rules) (pageNumbers: PageNumbers) : PageNumbers =
    let copy = Array.copy pageNumbers

    copy
    |> Array.sortInPlaceWith (fun x y ->
        match rules.TryGetValue(x) with
        | true, xs when xs.Contains(y) -> -1
        | _ ->
            match rules.TryGetValue(y) with
            | true, ys when ys.Contains(x) -> 1
            | _ -> 0)

    copy

let middlePageNumber (pageNumbers: PageNumbers) = pageNumbers[pageNumbers.Length / 2]

let solve (predicate: PageNumbers -> PageNumbers -> bool) (rules: Rules) (pageNumbers: List<PageNumbers>) : int =
    pageNumbers
    |> Seq.sumBy (fun xs ->
        let sorted = sortByRules rules xs
        if predicate xs sorted then middlePageNumber sorted else 0)

let partOne rules pageNumbers =
    solve (Array.forall2 (=)) rules pageNumbers

let partTwo rules pageNumbers =
    solve (Array.exists2 (<>)) rules pageNumbers

let rules, pageNumbers = File.ReadLines("./input/day05.txt") |> parse
partOne rules pageNumbers
partTwo rules pageNumbers