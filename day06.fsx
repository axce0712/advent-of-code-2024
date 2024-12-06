open System.IO

let parse (lines: seq<string>) =
    array2D lines

let up = (0, -1)

let findGuardPosition (map: char[,]) =
    let rec imp (map: char[,]) (x, y) =
        if map[y, x] = '^' then
            (x, y)
        else if (x + 1) = map.GetLength(1) then
            imp map (0, y + 1)
        else
            imp map (x + 1, y)
    
    imp map (0, 0)

let isOutside (map: char[,]) (x, y) =
    x < 0 || x >= map.GetLength(1) || y < 0 || y >= map.GetLength(0)

let turn (dx, dy) =
    match dx, dy with
    | 0, -1 -> (1, 0) // up -> right
    | 1, 0 -> (0, 1) // right -> down
    | 0, 1 -> (-1, 0) // down -> left
    | -1, 0 -> (0, -1) // left -> up
    | _ -> failwithf "Invalid direction (%i, %i)" dx dy

let patrol (map: char[,]) (direction: int * int) (x, y) =
    let markVisited (map: char[,]) (x, y) =
        map[y, x] <- '*'
    
    let rec imp (map: char[,]) visitedSoFar (dx, dy) (x, y) =
        let (nx, ny) = (x + dx, y + dy)
        
        if isOutside map (nx, ny) then
            visitedSoFar
        else
            match map[ny, nx] with
            | '#' -> imp map visitedSoFar (turn (dx, dy)) (x, y)
            | '*' -> imp map visitedSoFar (dx, dy) (nx, ny)
            | _ ->
                markVisited map (nx, ny)
                imp map (visitedSoFar + 1) (dx, dy) (nx, ny)

    markVisited map (x, y)
    imp map 1 direction (x, y)

let partOne (map: char[,]) =
    let startPosition = findGuardPosition map
    patrol map up startPosition

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

let map = File.ReadLines("./input/day06.txt") |> parse

partOne map
