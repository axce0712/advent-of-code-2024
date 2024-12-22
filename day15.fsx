open System
open System.Buffers
open System.IO

type Direction = Up | Down | Left | Right

let parse (input: string) =
    let span = input.AsSpan()
    let partSeparator = span.IndexOf("\n\n")
    let map = span.Slice(0, partSeparator).ToArray()
    let directionsPart = span.Slice(partSeparator + 2)
    let lineBreaks = directionsPart.Count('\n')
    let action = SpanAction<char, ReadOnlySpan<char>>(fun buffer arg ->
        let mutable idx = 0
        
        for i in 0 .. arg.Length - 1 do
            match arg[i] with
            | '\n' -> ()
            | chr ->
                buffer[idx] <- chr
                idx <- idx + 1)

    let directions = String.Create(directionsPart.Length - lineBreaks, directionsPart, action)
    map, directions

let getPosition lineWidth index =
    index % lineWidth, index / lineWidth

let add direction (x, y) =
    match direction with
    | Up -> x, y - 1
    | Down -> x, y + 1
    | Left -> x - 1, y
    | Right -> x + 1, y

let toIndex lineWidth (x, y) =
    y * lineWidth + x

let get lineWidth (map: char[]) position =
    map[toIndex lineWidth position]

let set lineWidth (map: char[]) position value =
    map[toIndex lineWidth position] <- value

let toDirection = function
    | '^' -> Up
    | '>' -> Right
    | 'v' -> Down
    | '<' -> Left
    | chr -> failwithf "Invalid direction: %c" chr

let getDirection (directions: string) index =
    directions[index] |> toDirection

let initialPosition lineWidth (map: char[]) =
    map
    |> Array.findIndex (fun c -> c = '@')
    |> getPosition lineWidth

let rec takeWhileBoxes lineWidth (map: char[]) direction position =
    let next = add direction position
    
    match get lineWidth map next with
    | 'O' -> takeWhileBoxes lineWidth map direction next
    | _ -> position

let move lineWidth (map: char[]) (directions: string) position =
    let rec imp lineWidth (map: char[]) (directions: string) directionIndex position =
        if directionIndex < directions.Length then
            let direction = getDirection directions directionIndex
            let next = add direction position
            let newPosition =
                match get lineWidth map next with
                | 'O' ->
                    let lastBoxPosition = takeWhileBoxes lineWidth map direction next
                    let after = add direction lastBoxPosition

                    match get lineWidth map after with
                    | '.' ->
                        set lineWidth map position '.'
                        set lineWidth map next '@'
                        set lineWidth map after 'O'
                        next
                    | _ -> position

                | '.' ->
                    set lineWidth map position '.'
                    set lineWidth map next '@'
                    next

                | _ -> position

            imp lineWidth map directions (directionIndex + 1) newPosition
        else
            position

    imp lineWidth map directions 0 position

let printMap lineWidth (map: char[]) =
    for y in 0 .. map.Length / lineWidth do
        for x in 0 .. lineWidth - 2 do
            printf "%c" (get lineWidth map (x, y))
        printfn ""

let gps (x, y) = y * 100 + x

let getLineWidth (map: char[]) =
    (map |> Array.findIndex (fun c -> c = '\n')) + 1

let partOne (map: char[]) directions =
    let lineWidth = getLineWidth map
    let initial = initialPosition lineWidth map
    let copy = Array.copy map
    move lineWidth copy directions initial |> ignore

    copy
    |> Seq.indexed
    |> Seq.sumBy (fun (idx, c) -> if c = 'O' then gps (getPosition lineWidth idx) else 0)

let expand (map: char[]) =
    [|
        for c in map do
            match c with
            | '#' -> yield! [| '#'; '#' |]
            | 'O' -> yield! [| '['; ']' |]
            | '.' -> yield! [| '.'; '.' |]
            | '@' -> yield! [| '@'; '.' |]
            | '\n' -> yield '\n'
            | chr -> failwithf "Invalid cell: %c" chr
    |]

let getBoxPosition lineWidth (map: char[]) position =
    match get lineWidth map position with
    | ']' -> add Left position
    | '[' -> position
    | _ -> failwithf "Invalid box position %A" position

let getNextPositionAfterBox direction boxPosition =
    match direction with
    | Up -> add Up boxPosition
    | Down -> add Down boxPosition
    | Left -> add Left boxPosition
    | Right -> boxPosition |> add Right |> add Right

let canMove lineWidth (map: char[]) direction position =
    let folder lineWidth map direction (canMove, xs) boxPosition =
        if canMove then
            let p1 = getNextPositionAfterBox direction boxPosition
            let p2 = add Right p1

            match direction, get lineWidth map p1, get lineWidth map p2 with
            | Up, '[', ']' -> true, p1 :: xs
            | Up, ']', '.' -> true, add Left p1 :: xs
            | Up, ']', '[' -> true, add Left p1 :: p2 :: xs
            | Up, '.', '[' -> true, p2 :: xs
            | Up, '.', '.' -> true, xs
            | Down, '[', ']' -> true, p1 :: xs
            | Down, ']', '.' -> true, add Left p1 :: xs
            | Down, ']', '[' -> true, add Left p1 :: p2 :: xs
            | Down, '.', '[' -> true, p2 :: xs
            | Down, '.', '.' -> true, xs
            | Left, ']', _ -> true, add Left p1 :: xs
            | Left, '.', _ -> true, xs
            | Right, '[', _ -> true, p1 :: xs
            | Right, '.', _ -> true, xs
            | _ -> false, []
        else
            false, []

    let rec imp lineWidth map stack direction boxPositions =
        let canMove, newBoxPositions =
            boxPositions
            |> List.fold (folder lineWidth map direction) (true, [])

        if canMove then
            match newBoxPositions with
            | [] -> true, stack |> List.collect id |> List.distinct
            | xs -> imp lineWidth map (xs :: stack) direction xs
        else
            false, []

    let next = add direction position

    match get lineWidth map next with
    | '#' -> false, []
    | '.' -> true, []
    | '[' | ']' ->
        let boxPosition = getBoxPosition lineWidth map next
        imp lineWidth map [[boxPosition]] direction [boxPosition]
    | chr -> failwithf "Invalid cell %c at position %A" chr next

let moveBox lineWidth (map: char[]) direction position =
    let boxPosition = getBoxPosition lineWidth map position
    let nextBoxPosition = add direction boxPosition

    match direction with
    | Up
    | Down ->
        set lineWidth map nextBoxPosition '['
        set lineWidth map (add Right nextBoxPosition) ']'
        set lineWidth map boxPosition '.'
        set lineWidth map (add Right boxPosition) '.'

    | Left ->
        set lineWidth map nextBoxPosition '['
        set lineWidth map (add Right nextBoxPosition) ']'
        set lineWidth map (nextBoxPosition |> add Right |> add Right) '.'

    | Right ->
        set lineWidth map (nextBoxPosition |> add Left) '.'
        set lineWidth map nextBoxPosition '['
        set lineWidth map (add Right nextBoxPosition) ']'

let move2 lineWidth (map: char[]) (directions: string) position =
    let mutable current = position
    for direction in directions |> Seq.map toDirection do
        let canMove, boxesToMove = canMove lineWidth map direction current

        if canMove then
            for boxPosition in boxesToMove do
                moveBox lineWidth map direction boxPosition

            let next = add direction current
            set lineWidth map current '.'
            set lineWidth map next '@'
            current <- next

let partTwo (map: char[]) directions =
    let expanded = expand map
    let lineWidth = getLineWidth expanded
    let initial = initialPosition lineWidth expanded
    move2 lineWidth expanded directions initial |> ignore

    expanded
    |> Seq.indexed
    |> Seq.sumBy (fun (idx, c) -> if c = '[' then gps (getPosition lineWidth idx) else 0)

// let map, directions =
//     @"##########
// #..O..O.O#
// #......O.#
// #.OO..O.O#
// #..O@..O.#
// #O#..O...#
// #O..O..O.#
// #.OO.O.OO#
// #....O...#
// ##########

// <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
// vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
// ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
// <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
// ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
// ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
// >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
// <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
// ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
// v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
//     |> parse

let input = File.ReadAllText("./input/day15.txt")
let map, directions = parse input
partOne map directions
partTwo map directions