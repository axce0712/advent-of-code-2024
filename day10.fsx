open System.Collections.Generic
open System.IO

let parse (lines: seq<string>) =
    lines
    |> Seq.map (fun line -> line |> Seq.map (fun c -> int c - int '0'))
    |> array2D

let findStartingPositions (map: int[,]) =
    [ for y in 0 .. Array2D.length1 map - 1 do
          for x in 0 .. Array2D.length2 map - 1 do
              if map.[y, x] = 0 then
                  yield x, y ]

let directions = [| (0, 1); (0, -1); (1, 0); (-1, 0) |]

let tryFindTrails (map: int[,]) (x, y)  =
    let visited = HashSet()
    let queue = Queue()
    let trails = HashSet()
    visited.Add((x, y)) |> ignore
    queue.Enqueue((x, y))

    while queue.Count > 0 do
        let x, y = queue.Dequeue()

        for dx, dy in directions do
            let nx, ny = x + dx, y + dy
            if
                visited.Contains((nx, ny)) |> not
                && nx >= 0
                && nx < Array2D.length2 map
                && ny >= 0
                && ny < Array2D.length1 map
            then
                if map[y, x] = 8 && map[ny, nx] = 9 then
                    printfn "%A %A" (x, y) (nx, ny)
                    printfn "done"
                    trails.Add((nx, ny)) |> ignore
                else if (map[y, x] + 1) = map[ny, nx] then
                    printfn "%A %A" (x, y) (nx, ny)
                    visited.Add((nx, ny)) |> ignore
                    queue.Enqueue((nx, ny))

    Set.ofSeq trails

let partOne (map: int[,]) =
    findStartingPositions map
    // |> List.map (fun (sx, sy) -> (sx, sy), tryFindTrails map (sx, sy))
    |> Seq.sumBy (fun (sx, sy) -> tryFindTrails map (sx, sy) |> Set.count)

let map = File.ReadLines("./input/day10.txt") |> parse

partOne map
