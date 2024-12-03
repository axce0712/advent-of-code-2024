open System.IO
open System.Text.RegularExpressions

let partOne input =
    Regex.Matches(input, @"(mul\((\d+)\,(\d+)\))")
    |> Seq.cast<Match>
    |> Seq.sumBy (_.Groups >> Seq.skip 2 >> Seq.map (_.Value >> int) >> Seq.reduce (*))

partOne "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let partTwo input =
    let (|Mul|Do|Dont|) (entry: Match) =
        let groups = entry.Groups |> Seq.skip 1 |> Seq.toArray
        if groups[1].Value = "mul" then
            Mul (int groups[2].Value, int groups.[3].Value)
        else if groups[5].Value = "don't" then
            Dont
        else if groups[7].Value = "do" then
            Do
        else
            failwithf "Unable to handle group %A" groups
    
    let mutable enabled = true
    let mutable acc = 0
    for entry in Regex.Matches(input, @"((mul)\((\d+)\,(\d+)\))|((don't)\(\))|((do)\(\))") do
        match entry with
        | Mul (x, y) ->
            if enabled then
                acc <- acc + x * y
        | Do ->
            enabled <- true
        | Dont ->
            enabled <- false

    acc

partTwo "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let input = File.ReadAllText("./input/day03.txt")
partOne input
partTwo input