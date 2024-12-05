open System
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.NativeInterop

#nowarn "9"

type ReadOnlySpan<'T> with

    member this.Item
        with get (range: Range) =
            let length =
                if range.End.IsFromEnd then
                    this.Length - range.End.Value - range.Start.Value
                else
                    range.End.Value - range.Start.Value

            this.Slice(range.Start.Value, length)

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
                let ranges = Span<Range>(NativePtr.toVoidPtr (NativePtr.stackalloc<Range> 2), 2)
                MemoryExtensions.Split(span, ranges, '|') |> ignore
                let key = Int32.Parse(span[ranges[0]])
                let value = Int32.Parse(span[ranges[1]])

                match rules.TryGetValue(key) with
                | true, values -> values.Add(value)
                | false, _ ->
                    let values = ResizeArray()
                    values.Add(value)
                    rules.Add(key, values)
        else
            let span = line.AsSpan()
            let length = span.Count(',') + 1

            let ranges =
                Span<Range>(NativePtr.toVoidPtr (NativePtr.stackalloc<Range> length), length)

            MemoryExtensions.Split(span, ranges, ',') |> ignore
            let values = Array.zeroCreate<int> length

            for idx in 0 .. length - 1 do
                let value = Int32.Parse(span[ranges[idx]])
                values[idx] <- value

            pageNumbers.Add(values)

    let mappedRules =
        rules |> Seq.map (fun kvp -> kvp.Key, kvp.Value.ToArray()) |> Map.ofSeq

    mappedRules, pageNumbers.ToArray()

type Rules = Map<int, int[]>

type PageNumbers = int[]

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

let rules, pageNumbers = File.ReadLines("./input/day05.txt") |> parse
partOne rules pageNumbers
partTwo rules pageNumbers
