open System.IO
open System.Collections.Generic

let parse (lines: seq<string>) =
    array2D lines

type Direction =
    | Up
    | Right
    | Down
    | Left

type Position = (struct (int * int))

let turn = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let move direction ((x, y): Position) : Position =
    match direction with
    | Up -> struct (x, y - 1)
    | Right -> struct (x + 1, y)
    | Down -> struct (x, y + 1)
    | Left -> struct (x - 1, y)

let toInt = function
    | Up -> 1
    | Right -> 2
    | Down -> 4
    | Left -> 8

let containsDirection direction value =
    let expected = toInt direction
    (value &&& toInt direction) = expected

let setDirection direction value =
    value ||| toInt direction

let get (map: char[,]) ((x, y): Position) : char =
    map[y, x]

let set (map: char[,]) ((x, y): Position) (value: char) =
    map[y, x] <- value

let findGuardPosition (map: char[,]) : Position =
    let rec imp (map: char[,]) ((struct (x, y): Position) as pos) =
        if get map pos = '^' then
            pos
        else if (x + 1) = map.GetLength(1) then
            imp map (0, y + 1)
        else
            imp map (x + 1, y)

    imp map (0, 0)

let isOutside (map: char[,]) ((x, y): Position) : bool =
    x < 0 || x >= map.GetLength(1) || y < 0 || y >= map.GetLength(0)

let patrol (map: char[,]) (direction: Direction) ((x, y): Position) : int =
    let markVisited (map: char[,]) pos = set map pos '*'

    let rec imp (map: char[,]) visitedSoFar direction (x, y) =
        let (struct (nx, ny) as next) = move direction (x, y)
        
        if isOutside map (nx, ny) then
            visitedSoFar
        else
            match get map next with
            | '#' -> imp map visitedSoFar (turn direction) (x, y)
            | '*' -> imp map visitedSoFar direction (nx, ny)
            | _ ->
                markVisited map (nx, ny)
                imp map (visitedSoFar + 1) direction (nx, ny)

    let copy = Array2D.copy map
    markVisited copy (x, y)
    imp copy 1 direction (x, y)

let partOne (map: char[,]) =
    let startPosition = findGuardPosition map
    patrol map Up startPosition

let countOptions (map: char[,]) (initialDirection: Direction) (pos: Position) =
    let visited = Dictionary<Position, int>()
    let loopVisited = Dictionary<Position, int>()
    let rec isLoop (direction: Direction) (pos: Position) =
        let rec imp (direction: Direction) (pos: Position) =
            let next = move direction pos

            if isOutside map next then
                false
            else if containsDirection direction (visited.GetValueOrDefault(next))
                || containsDirection direction (loopVisited.GetValueOrDefault(next)) then
                true
            else
                match get map next with
                | '#' ->
                    loopVisited[next] <- setDirection direction (loopVisited.GetValueOrDefault(next))
                    imp (turn direction) pos
                | _ ->
                    imp direction next

        let next = move direction pos

        if isOutside map next then
            false
        else
            match get map next with
            | '#' -> false
            | _ ->
                loopVisited.Clear()
                loopVisited[next] <- toInt direction
                imp (turn direction) pos

    let rec imp (optionsSoFar: int) (direction: Direction) (pos: Position) =
        let next = move direction pos

        if isOutside map next then
            optionsSoFar
        else
            match get map next with
            | '#' ->
                visited[next] <- setDirection direction (visited.GetValueOrDefault(next))
                imp optionsSoFar (turn direction) pos
            | _ ->
                let newOptions =
                    if isLoop direction pos then
                        optionsSoFar + 1
                    else
                        optionsSoFar

                imp newOptions direction next

    imp 0 initialDirection pos

let partTwo (map: char[,]) =
    let startPosition = findGuardPosition map
    countOptions (Array2D.copy map) Up startPosition

let input =
    @"....#.....
.........#
..........
..#.......
.......#..
.#..^.....
........#.
#.........
......#..."
    |> _.Split('\n')

// let map = parse input

// partTwo map

let map = File.ReadLines("./input/day06.txt") |> parse

#time
partTwo map
#time
