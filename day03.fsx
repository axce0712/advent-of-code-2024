open System.IO
open System.Text.RegularExpressions

let partOne input =
    Regex.Matches(input, @"(mul\((\d+)\,(\d+)\))")
    |> Seq.cast<Match>
    |> Seq.sumBy (_.Groups >> Seq.skip 2 >> Seq.map (_.Value >> int) >> Seq.reduce (*))

partOne "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let partTwo input =
    let mutable enabled = true
    let mutable acc = 0
    for entry in Regex.Matches(input, @"((mul)\((\d+)\,(\d+)\))|((don't)\(\))|((do)\(\))") do
        if entry.Groups[2].Value = "mul" then
            if enabled then
                let x = entry.Groups[3].Value |> int
                let y = entry.Groups[4].Value |> int
                acc <- acc + x * y
        else if entry.Groups[6].Value = "don't" then
            enabled <- false
        else if entry.Groups[8].Value = "do" then
            enabled <- true

        else
            failwithf "Unable to handle group %A" entry.Groups
    acc

partTwo "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let input = File.ReadAllText("./input/day03.txt")
partOne input
partTwo input