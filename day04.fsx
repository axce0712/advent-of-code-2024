open System.IO

let parse (input: seq<string>) = array2D input

let isInRange (x, y) (input: 'a[,]) =
    x >= 0 && x < input.GetLength(1) && y >= 0 && y < input.GetLength(0)

let countOccurrences (word: string) (directions: list<int * int>) (x, y) (input: char[,]) : int =
    let rec hasWord (word: string) ((dx, dy): int * int) (wordPosition: int) (x, y) =
        if wordPosition = word.Length then
            true
        else if isInRange (x, y) input && input[y, x] = word[wordPosition] then
            hasWord word (dx, dy) (wordPosition + 1) (x + dx, y + dy)
        else
            false

    if input[y, x] = word[0] then
        let mutable count = 0

        for (dx, dy) in directions do
            if hasWord word (dx, dy) 1 (x + dx, y + dy) then
                count <- count + 1

        count
    else
        0

let search (word: string) (directions: list<int * int>) (input: char[,]) : int =
    match word.Length with
    | 0 -> 0
    | _ ->
        let mutable count = 0

        for y in 0 .. input.GetLength(0) - 1 do
            for x in 0 .. input.GetLength(1) - 1 do
                count <- count + countOccurrences word directions (x, y) input

        count

let solve (word: string) (directions: list<int * int>) (input: char[,]) : int = search word directions input

let partOne (input: char[,]) =
    solve "XMAS" [ (0, 1); (1, 0); (0, -1); (-1, 0); (1, 1); (-1, -1); (1, -1); (-1, 1) ] input

let partTwo (input: char[,]) =
    let mutable count = 0

    for y in 1 .. input.GetLength(0) - 2 do
        for x in 1 .. input.GetLength(1) - 2 do
            if input[y, x] = 'A' then
                match input[y - 1, x - 1], input[y + 1, x + 1], input[y - 1, x + 1], input[y + 1, x - 1] with
                | 'M', 'S', 'M', 'S'
                | 'M', 'S', 'S', 'M'
                | 'S', 'M', 'M', 'S'
                | 'S', 'M', 'S', 'M' -> count <- count + 1
                | _ -> ()

    count

// let input =
//     @"MMMSXXMASM
// MSAMXMSMSA
// AMXSXMAAMM
// MSAMASMSMX
// XMASAMXAMM
// XXAMMXXAMA
// SMSMSASXSS
// SAXAMASAAA
// MAMMMXMMMM
// MXMXAXMASX"
//     |> _.Split('\n')
//     |> parse

let input = File.ReadLines("./input/day04.txt") |> parse

partOne input
partTwo input
