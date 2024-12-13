open System.Collections.Generic
open System.IO

type Position = int * int

let parse (lines: seq<string>) =
    lines
    |> Seq.map (fun line -> line |> Seq.map (fun c -> int c - int '0'))
    |> array2D

let findStartingPositions (map: int [,]) : list<Position> =
    [ for y in 0 .. Array2D.length1 map - 1 do
          for x in 0 .. Array2D.length2 map - 1 do
              if map.[y, x] = 0 then yield x, y ]

let directions = [| (0, 1); (0, -1); (1, 0); (-1, 0) |]

let isInRange (map: 'a [,]) ((x, y): Position) : bool =
    x >= 0
    && x < Array2D.length2 map
    && y >= 0
    && y < Array2D.length1 map

let isIncreasing (map: int [,]) ((x, y): Position) ((nx, ny): Position) : bool = (map[y, x] + 1) = map[ny, nx]

let isEnding (map: int [,]) ((x, y): Position) ((nx, ny): Position) : bool = map[y, x] = 8 && map[ny, nx] = 9

let countTrails (map: int [,]) ((x, y): Position) : int =
    let queue = Queue()
    let trails = HashSet()
    queue.Enqueue((x, y))

    while queue.Count > 0 do
        let x, y = queue.Dequeue()

        for dx, dy in directions do
            let nx, ny = x + dx, y + dy

            if isInRange map (nx, ny) then
                if isEnding map (x, y) (nx, ny) then
                    trails.Add((nx, ny)) |> ignore
                else if isIncreasing map (x, y) (nx, ny) then
                    queue.Enqueue((nx, ny))

    trails.Count

let solve (f: int [,] -> Position -> int) (map: int [,]) =
    findStartingPositions map
    |> Seq.sumBy (fun (sx, sy) -> f map (sx, sy))

let partOne (map: int [,]) = solve countTrails map

let countDistinctHikingTrails (map: int [,]) (sx, sy) =
    let queue = Queue()
    let trails = Dictionary<Position * Position, int>()
    queue.Enqueue((sx, sy))

    while queue.Count > 0 do
        let x, y = queue.Dequeue()

        for dx, dy in directions do
            let nx, ny = x + dx, y + dy

            if isInRange map (nx, ny) then
                if isEnding map (x, y) (nx, ny) then
                    let key = (sx, sy), (nx, ny)
                    trails[key] <- trails.GetValueOrDefault(key, 0) + 1
                else if isIncreasing map (x, y) (nx, ny) then
                    queue.Enqueue((nx, ny))

    Seq.sum trails.Values

let partTwo (map: int [,]) = solve countDistinctHikingTrails map

let map = File.ReadLines("./input/day10.txt") |> parse

partOne map
partTwo map
