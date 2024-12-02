open System
open System.IO

type Level = int

type Report = Level list

type ReportResult = Safe | Unsafe

let parse (line: string) : Report =
    line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map int
    |> Seq.toList

let input = File.ReadLines("./input/day02.txt")

// let input =
//     @"7 6 4 2 1
// 1 2 7 8 9
// 9 7 6 2 1
// 1 3 2 4 5
// 8 6 4 4 1
// 1 3 6 7 9"
//     |> _.Split('\n')

let reports =
    input |> Seq.map parse

let resolveReport (report: Report) =
    let paired = Seq.pairwise report

    if paired |> Seq.forall (fun (a, b) -> abs (b - a) <= 3) then
        let isSafe =
            paired
            |> Seq.map (fun (a, b) -> b.CompareTo(a))
            |> Seq.pairwise
            |> Seq.forall (fun (d1, d2) -> d1 = d2)

        if isSafe then Safe else Unsafe
    else
        Unsafe

let partOne =
    reports
    |> Seq.map resolveReport
    |> Seq.filter _.IsSafe
    |> Seq.length