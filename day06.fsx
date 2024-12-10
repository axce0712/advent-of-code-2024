open System.IO
open System.Collections.Generic

let parse (lines: seq<string>) =
    array2D lines

type Direction =
    | Up
    | Right
    | Down
    | Left

type Directions = int

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

let toInt (value: Direction) : Directions =
    match value with
    | Up -> 1
    | Right -> 2
    | Down -> 4
    | Left -> 8

let getChar = function
    | Up -> '^'
    | Right -> '>'
    | Down -> 'v'
    | Left -> '<'

let contains (direction: Direction) (value: Directions) =
    let expected = toInt direction
    (value &&& toInt direction) = expected

let addDirection (direction: Direction) (value: Directions) : Directions =
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
    let rec imp (map: char[,]) visitedSoFar direction (x, y) =
        let (struct (nx, ny) as next) = move direction (x, y)
        
        if isOutside map (nx, ny) then
            visitedSoFar
        else
            match get map next with
            | '#' -> imp map visitedSoFar (turn direction) (x, y)
            | '.' ->
                set map (nx, ny) (getChar direction)
                imp map (visitedSoFar + 1) direction (nx, ny)
            | _ -> imp map visitedSoFar direction (nx, ny)

    let copy = Array2D.copy map
    imp copy 1 direction (x, y)

let partOne (map: char[,]) =
    let startPosition = findGuardPosition map
    patrol map Up startPosition

let printMap (map: char[,]) =
    for y in 0 .. map.GetLength(0) - 1 do
        for x in 0 .. map.GetLength(1) - 1 do
            printf "%c" (map[y, x])
        printfn ""

let partTwo (map: char[,]) =
    let map = Array2D.copy map
    let countLoops (initialDirection: Direction) (pos: Position) =
        // Keeps track of visited positions and the direction when searching for a loop
        let loopVisited = Dictionary<Position, Directions>()
        let rec isLoop (direction: Direction) (pos: Position) =
            // Track position and direction
            loopVisited[pos] <- addDirection direction (loopVisited.GetValueOrDefault(pos))

            // Get next position
            let next = move direction pos

            if isOutside map next then
                // We are outside the map and therefore no loop was found
                false
            else 
                match get map next with
                | '#' -> isLoop (turn direction) pos
                | _ ->
                    if contains direction (loopVisited.GetValueOrDefault(next)) then
                        // If we have visited this position with the same direction
                        // we have found a loop
                        true
                    else
                        // Otherwise continue searching
                        isLoop direction next

        let rec imp (loopsSoFar: int) (direction: Direction) (pos: Position) =
            // Remember always the position which has been visited
            let next = move direction pos

            if isOutside map next then
                // We are outside the map and therefore done
                loopsSoFar
            else
                match get map next with
                // Obstacle
                | '#' ->
                    // Stay on the same position and turn the direction
                    imp loopsSoFar (turn direction) pos

                // Unvisited
                | '.' ->
                    // Place new obstruction and try to find loop
                    set map next '#'

                    // Since we are reusing the same map clear it before
                    // searching for a loop
                    loopVisited.Clear()

                    let newLoops =
                        if isLoop direction pos then
                            loopsSoFar + 1
                        else
                            loopsSoFar

                    // Mark visited
                    set map next (getChar direction)
                    imp newLoops direction next

                // Visited
                | _ ->
                    // If position has already been visited, we cannot place an
                    // obstruction here
                    imp loopsSoFar direction next
                        
        imp 0 initialDirection pos

    let startPosition = findGuardPosition map
    countLoops Up startPosition

let input =
    @"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."
    |> _.Split('\n')

// let map = parse input

// partTwo map

let map = File.ReadLines("./input/day06.txt") |> parse

partOne map
partTwo map