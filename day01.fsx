open System
open System.IO

let input = File.ReadLines("./input/day01.txt")

let left, right =
    input
    |> Seq.fold
        (fun (leftSoFar, rightSoFar) line ->
            let parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            let newLeft = int parts[0] :: leftSoFar
            let newRight = int parts[1] :: rightSoFar
            newLeft, newRight)
        ([], [])

let partOne =
    Seq.zip (Seq.sort left) (Seq.sort right)
    |> Seq.sumBy (fun (l, r) -> max l r - min l r)

let countedRight =
    right
    |> Seq.countBy id
    |> Map.ofSeq

let partTwo =
    left
    |> Seq.sumBy (fun l -> l * (Map.tryFind l countedRight |> Option.defaultValue 0))