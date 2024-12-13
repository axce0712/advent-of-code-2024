open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices

type Plant = char

type Position = int * int

type Area = Set<Position>

type Garden = { Regions: Map<Plant, list<Area>> }

let findRegion plant pos garden =
    garden.Regions
    |> Map.find plant
    |> List.find (Set.contains pos)

let parse (lines: seq<string>) = array2D lines

let tryGetRow fallback (y: int) (arr: 'a[,]) : seq<'a> =
    let f =
        if y >= 0 && y < arr.GetLength(0) then
            fun x -> arr[y, x]
        else
            fun _ -> fallback

    seq {
        for x in 0 .. arr.GetLength(1) - 1 do
            yield f x
    }

let tryGetColumn fallback (x: int) (arr: 'a[,]) : seq<'a> =
    let f =
        if x >= 0 && x < arr.GetLength(1) then
            fun y -> arr[y, x]
        else
            fun _ -> fallback

    seq {
        for y in 0 .. arr.GetLength(0) - 1 do
            yield f y
    }

let getRegions garden =
    let regions = Dictionary<Plant, list<HashSet<Position>>>()

    for y in 0 .. Array2D.length1 garden - 1 do
        for x in 0 .. Array2D.length2 garden - 1 do
            let plant = garden[y, x]

            match regions.TryGetValue(plant) with
            | true, areas when areas |> Seq.exists (Seq.contains (x, y)) -> ()
            | _ ->
                let area = HashSet<Position>()
                let queue = Queue<Position>([ (x, y) ])

                while queue.Count > 0 do
                    let (x, y) = queue.Dequeue()

                    if x >= 0
                       && x < Array2D.length2 garden
                       && y >= 0
                       && y < Array2D.length1 garden then
                        if
                            garden[y, x] = plant
                            && not (area |> Seq.contains (x, y))
                        then
                            area.Add((x, y)) |> ignore

                            [ (0, 1); (0, -1); (1, 0); (-1, 0) ]
                            |> List.iter (fun (dx, dy) -> queue.Enqueue((x + dx, y + dy)))

                regions[plant] <- area :: regions.GetValueOrDefault(plant, [])

    { Regions =
        regions
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> List.map Set.ofSeq)
        |> Map.ofSeq }

let partOne garden =
    let regions = getRegions garden
    regions

let lines =
    @"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"
    |> fun xs -> xs.Split('\n')

// let lines = File.ReadLines("./input/day12.txt")

let garden = parse lines

partOne garden
