open System
open System.IO

type Robot =
    {
        StartingPosition: int *  int
        Velocity: int * int
    }

type Tile =
    {
        Width: int
        Height: int
    }

    member this.HorizontalLine = this.Height / 2

    member this.VerticalLine = this.Width / 2

type VerticalPosition =
    | Top
    | Center
    | Bottom

type HorizontalPosition =
    | Left
    | Middle
    | Right

let parse (lines: seq<string>) =
    let mapping (line: string) =
        let mutable span = line.AsSpan()
        span <- span.Slice(span.IndexOf("p=") + 2)
        let x = Int32.Parse(span.Slice(0, span.IndexOf(',')))
        span <- span.Slice(span.IndexOf(',') + 1)
        let y = Int32.Parse(span.Slice(0, span.IndexOf(' ')))
        span <- span.Slice(span.IndexOf("v=") + 2)
        let dx = Int32.Parse(span.Slice(0, span.IndexOf(',')))
        span <- span.Slice(span.IndexOf(',') + 1)
        let dy = Int32.Parse(span)

        { StartingPosition = (x, y); Velocity = (dx, dy) }

    lines
    |> Seq.map mapping
    |> Seq.toList

let steps seconds (tile: Tile) (robot: Robot) =
    let rec imp tile seconds (x, y) (dx, dy) =
        if seconds = 0 then
            (x % tile.Width + tile.Width) % tile.Width, (y % tile.Height + tile.Height) % tile.Height
        else
            imp tile (seconds - 1) (x + dx, y + dy) (dx, dy)

    let sx, sy = robot.StartingPosition
    let dx, dy = robot.Velocity
    imp tile seconds (sx, sy) (dx, dy)

let getHorizontalPos (tile: Tile) x =
    if x < tile.VerticalLine then
        Left
    else if x > tile.VerticalLine then
        Right
    else
        Middle

let getVerticalPos (tile: Tile) y =
    if y < tile.HorizontalLine then
        Top
    else if y > tile.HorizontalLine then
        Bottom
    else
        Center

let quadrant (tile: Tile) (x, y) =
    match getHorizontalPos tile x, getVerticalPos tile y with
    | Left, Top -> 1, 0, 0, 0
    | Right, Top -> 0, 1, 0, 0
    | Left, Bottom -> 0, 0, 1, 0
    | Right, Bottom -> 0, 0, 0, 1
    | _ -> 0, 0, 0, 0

let partOne tile robots =
    let (a, b, c, d) =
        robots
        |> Seq.map (steps 100 tile >> quadrant tile)
        |> Seq.reduce (fun (a, b, c, d) (x, y, z, w) -> (a + x, b + y, c + z, d + w))

    a * b * c * d

// let lines =
//     @"p=0,4 v=3,-3
//     p=6,3 v=-1,-3
//     p=10,3 v=-1,2
//     p=2,0 v=2,-1
//     p=0,0 v=1,3
//     p=3,0 v=-2,-2
//     p=7,6 v=-1,-3
//     p=3,0 v=-1,-2
//     p=9,3 v=2,3
//     p=7,3 v=-1,2
//     p=2,4 v=2,-3
//     p=9,5 v=-3,-3"
//     |> fun str -> str.Split('\n')

// let robots = parse lines

// partOne { Width = 11; Height = 7 } robots

let lines = File.ReadLines("./input/day14.txt")

let robots = parse lines

partOne { Width = 101; Height = 103 } robots

(*


01234567891
.....#.....
....#.#....
...#...#...
..#.....#..
.#.......#.
###########
.....#.....


*)