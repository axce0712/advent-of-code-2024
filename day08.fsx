open System.Collections.Generic
open System.IO

type Frequency = char

type Position = int * int

type City =
    {
        Antennas: Map<Frequency, list<Position>>
        Width: int
        Height: int
    }

module City =
    let isInRange (x, y) city =
        0 <= x && x < city.Width && 0 <= y && y < city.Height

type Antinode =
    {
        Position1: Position
        Position2: Position
        Locations: list<Position>
    }

let parse (lines: seq<string>) =
    let antennas = Dictionary<Frequency, list<Position>>()
    let mutable width = -1
    let mutable y = 0
    for line in lines do
        for x in 0 .. line.Length - 1 do
            match line[x] with
            | '.' -> ()
            | frequency -> antennas[frequency] <- (x, y) :: (antennas.GetValueOrDefault(frequency, []))

        if width = -1 then
            width <- line.Length
        else if width <> line.Length then
            failwith "Invalid input"

        y <- y + 1

    {
        Antennas = antennas |> Seq.map (|KeyValue|) |> Map.ofSeq
        Width = width
        Height = y
    }

let allCombinations list =
    let rec imp acc list =
        match list with
        | [] -> acc |> List.rev
        | x :: ys ->
            let combinations = [
                for y in ys do
                    (x, y)
            ]

            let mutable newAcc = acc
            for combination in combinations do
                newAcc <- combination :: newAcc

            imp newAcc ys

    imp [] list

let generateLocations position1 position2 city =
    let x1, y1 = position1
    let x2, y2 = position2
    let dx = x2 - x1
    let dy = y2 - y1
    
    [
        let ax = x1 - dx
        let ay = y1 - dy
        if City.isInRange (ax, ay) city then
            (ax, ay)

        let bx = x2 + dx
        let by = y2 + dy
        if City.isInRange (bx, by) city then
            (bx, by)
    ]

let solve f city =
    city.Antennas
    |> Map.toSeq
    |> Seq.collect (fun (_, ps) -> allCombinations ps |> Seq.collect (fun (p1, p2) -> f p1 p2 city))
    |> Seq.distinct
    |> Seq.length

let partOne (city: City) =
    solve generateLocations city

let generateRepeatingLocations position1 position2 city =
    [
        yield position1
        yield position2
        let x1, y1 = position1
        let x2, y2 = position2
        let dx = x2 - x1
        let dy = y2 - y1
        let mutable ax = x1 - dx
        let mutable ay = y1 - dy
        while City.isInRange (ax, ay) city do
            yield (ax, ay)
            ax <- ax - dx
            ay <- ay - dy
        
        let mutable bx = x2 + dx
        let mutable by = y2 + dy
        while City.isInRange (bx, by) city do
            yield (bx, by)
            bx <- bx + dx
            by <- by + dy
    ]

let partTwo (city: City) =
    solve generateRepeatingLocations city

// let city =
//     @"............
// ........0...
// .....0......
// .......0....
// ....0.......
// ......A.....
// ............
// ............
// ........A...
// .........A..
// ............
// ............"
//     |> _.Split('\n')
//     |> parse

let city = File.ReadLines("./input/day08.txt") |> parse

partOne city
partTwo city