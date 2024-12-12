open System
open System.Collections.Generic
open System.IO

type Stone = int64

type Stones = int64 []

let parse (input: string) =
    let mutable span = input.AsSpan()
    let length = span.Count(' ') + 1
    let stones = Array.zeroCreate<int64> length
    let mutable idx = 0
    let mutable occurrence = span.IndexOf(' ')

    while occurrence > 0 do
        stones[idx] <- Int32.Parse(span.Slice(0, occurrence))
        span <- span.Slice(occurrence + 1)
        occurrence <- span.IndexOf(' ')
        idx <- idx + 1

    stones[idx] <- Int32.Parse(span) |> int64
    stones

let (|EvenDigits|_|) (x: Stone) =
    let rec imp count x =
        match x / 10L with
        | 0L ->
            if count % 2 = 0 then
                Some(count / 2)
            else
                None
        | y -> imp (count + 1) y

    imp 1 x

let blinkOnce (stone: Stone) : Stones =
    match stone with
    | 0L -> [| 1 |]
    | EvenDigits length ->
        let x = int64 (Math.Pow(10, length))

        [| (stone - stone % x) / x; stone % x |]
    | _ -> [| stone * 2024L |]

let blink times (stones: Stones) =
    let blinkCache = Dictionary<Stone, Stones>()

    let countCache =
        stones
        |> Seq.map (fun stone -> KeyValuePair(stone, 1L))
        |> Dictionary<Stone, int64>

    for _ in 1..times do
        for kvp in
            countCache
            |> Seq.filter (fun kvp -> kvp.Value > 0L)
            |> Seq.toArray do
            let stone, countSoFar = (|KeyValue|) kvp

            let newStones =
                match blinkCache.TryGetValue(stone) with
                | true, newStones -> newStones
                | false, _ ->
                    let newStones = blinkOnce stone
                    blinkCache[stone] <- newStones
                    newStones

            for newStone in newStones do
                countCache[newStone] <- countCache.GetValueOrDefault(newStone)
                                        + countSoFar

            countCache[stone] <- countCache[stone] - countSoFar

    countCache |> Seq.sumBy (fun kvp -> kvp.Value)

let partOne (input: string) =
    let stones = parse input
    blink 25 stones

let partTwo (input: string) =
    let stones = parse input
    blink 75 stones

let input = File.ReadAllText("./input/day11.txt")

partOne input
partTwo input
