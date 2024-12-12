open System.Collections.Generic
open System.IO

type Plant = char

type Position = int * int

type Region =
    {
        Plant: Plant
        Area: Set<Position>
    }

type Garden =
    {
        Regions: list<Region>
    }

let parse (lines: seq<string>) =
    let regions = Dictionary<Plant, list<HashSet<Position>>>()
    let garden = array2D lines
    let isInRange (x, y) garden =
        x >= 0 && x < Array2D.length2 garden && y >= 0 && y < Array2D.length1 garden

    for y in 0 .. Array2D.length1 garden - 1 do
        for x in 0 .. Array2D.length2 garden - 1 do
            let plant = garden[y, x]

            if plant <> '#' then
                match regions.TryGetValue(plant) with
                | true, areas when areas |> List.exists (_.Contains((x, y))) -> ()
                | _ ->
                    let area = HashSet<Position>()
                    let queue = Queue<Position>()
                    queue.Enqueue((x, y))
                    while queue.Count > 0 do
                        let (x, y) = queue.Dequeue()
                        if isInRange (x, y) garden && garden[y, x] = plant then
                            area.Add((x, y)) |> ignore
                            garden[y, x] <- '#'
                            
                            if not (area.Contains((x - 1, y))) then
                                queue.Enqueue((x - 1, y))

                            if not (area.Contains((x + 1, y))) then
                                queue.Enqueue((x + 1, y))
                            
                            if not (area.Contains((x, y - 1))) then
                                queue.Enqueue((x, y - 1))
                            
                            if not (area.Contains((x, y + 1))) then
                                queue.Enqueue((x, y + 1))

                    regions[plant] <- area :: regions.GetValueOrDefault(plant, [])

    {
        Regions =
            regions
            |> Seq.collect (fun kvp ->
                let (plant, areas) = (|KeyValue|) kvp
                areas
                |> Seq.map (fun area ->
                    {
                        Plant = plant
                        Area = Set.ofSeq area
                    }))
            |> List.ofSeq
    }

let perimeter region =
    region.Area
    |> Seq.sumBy (fun (x, y) ->
        4
        - (if region.Area |> Set.contains (x - 1, y) then 1 else 0)
        - (if region.Area |> Set.contains (x + 1, y) then 1 else 0)
        - (if region.Area |> Set.contains (x, y - 1) then 1 else 0)
        - (if region.Area |> Set.contains (x, y + 1) then 1 else 0))

let partOne garden =
    garden.Regions
    |> List.sumBy (fun region -> perimeter region * Set.count region.Area)

// let lines =
//     @"RRRRIICCFF
// RRRRIICCCF
// VVRRRCCFFF
// VVRCCCJFFF
// VVVVCJJCFE
// VVIVCCJJEE
// VVIIICJJEE
// MIIIIIJJEE
// MIIISIJEEE
// MMMISSJEEE"
//     |> fun xs -> xs.Split('\n')

let lines = File.ReadLines("./input/day12.txt")

let garden = parse lines

partOne garden
