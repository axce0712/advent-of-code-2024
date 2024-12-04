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

let countIncrements toleranceLevels report =
    let rec loop dir acc toleranceLevels x xs =
        if toleranceLevels < 0 then
            acc
        else
            match xs with
            | [] -> acc
            | y :: ys ->
                match ys with
                | [] when isSafeIncrement dir x y -> acc + 1
                | [] -> acc
                | z :: zs ->
                    match isSafeIncrement dir x y, isSafeIncrement dir x z with
                    | true, true ->
                        max (loop dir (acc + 1) toleranceLevels y ys) (loop dir (acc + 1) (toleranceLevels - 1) z zs)
                    | true, false -> loop dir (acc + 1) toleranceLevels y ys
                    | false, true -> loop dir (acc + 1) (toleranceLevels - 1) z zs
                    | false, false -> acc

    let imp dir toleranceLevels x xs =
        if dir = 0 then
            0
        else
            loop dir 0 toleranceLevels x xs

    match report with
    | [] -> 0
    | x :: xs ->
        match xs with
        | [] -> 0
        | y :: ys ->
            match ys with
            | [] -> imp (compare y x) toleranceLevels x xs
            | z :: _ ->
                (imp (compare y x) toleranceLevels x xs)
                |> max (imp (compare z x) (toleranceLevels - 1) x ys)
                |> max (imp (compare z y) (toleranceLevels - 1) y ys)

let isSafe toleranceLevel (report: Report) : bool =
    (report.Length - countIncrements 1 report)
    |> isBetween 1 (toleranceLevel + 1)

let solve toleranceLevel reports =
    reports
    |> Seq.filter (isSafe toleranceLevel)
    |> Seq.length

let partOne = reports |> solve 0

let partTwo = reports |> solve 1
