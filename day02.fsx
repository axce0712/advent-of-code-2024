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

let isSafeIncrement dir x y =
    abs (y - x) |> isBetween 1 3 && compare y x = dir

let countIncrements tolerateLevels report =
    let rec loop dir acc tolerateLevels x xs =
        if tolerateLevels < 0 then
            acc
        else
            match xs with
            | [] -> acc
            | y :: z :: zs when isSafeIncrement dir x y && isSafeIncrement dir x z ->
                max (loop dir (acc + 1) tolerateLevels y (z :: zs)) (loop dir (acc + 1) (tolerateLevels - 1) z zs)
            | y :: ys when isSafeIncrement dir x y -> loop dir (acc + 1) tolerateLevels y ys
            | _ :: z :: zs when isSafeIncrement dir x z -> loop dir (acc + 1) (tolerateLevels - 1) z zs
            | _ -> acc

    let imp dir tolerateLevels x xs = if dir = 0 then 0 else loop dir 0 tolerateLevels x xs

    match report with
    | x :: y :: z :: zs ->
        (imp (compare y x) tolerateLevels x (y :: z :: zs))
        |> max (imp (compare z x) (tolerateLevels - 1) x (z :: zs))
        |> max (imp (compare z y) (tolerateLevels - 1) y (z :: zs))
    | x :: [ y ] -> imp (compare y x) tolerateLevels x [ y ]
    | _ -> 0

let partOne =
    reports
    |> Seq.filter (fun report -> (report.Length - countIncrements 0 report) = 1)
    |> Seq.length

let partTwo =
    reports
    |> Seq.filter (fun report -> (report.Length - countIncrements 1 report) |> isBetween 1 2)
    |> Seq.length
