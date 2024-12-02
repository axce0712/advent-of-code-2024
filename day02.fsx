open System
open System.IO

type Level = int

type Report = Level list

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

let reports = input |> Seq.map parse

let isBetween min max value = min <= value && value <= max

let countIncrements (report: Report) =
    let rec imp acc direction x xs =
        match xs with
        | [] -> acc
        | y :: ys when abs (y - x) |> isBetween 1 3 && compare y x = direction -> imp (acc + 1) direction y ys
        | _ :: z :: zs when abs (z - x) |> isBetween 1 3 && compare z x = direction -> imp (acc + 1) direction z zs
        | _ -> acc

    match report with
    | x :: y :: z :: zs ->
        (imp 0 (compare y x) x (y :: z :: zs))
        |> max (imp 0 (compare z x) x (z :: zs))
        |> max (imp 0 (compare z y) y (z :: zs))
    | x :: [ y ] -> imp 0 (compare y x) x [ y ]
    | _ -> 0

let partOne =
    reports
    |> Seq.filter (fun report -> (report.Length - countIncrements report) = 1)
    |> Seq.length

let partTwo =
    reports
    |> Seq.filter (fun report -> (report.Length - countIncrements report) |> isBetween 1 2)
    |> Seq.length
