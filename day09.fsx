open System
open System.IO

type DiskMap = string

let split (input: DiskMap) =
    let length = input.Length / 2 + input.Length % 2
    let files = Array.zeroCreate<int> length
    let freeSpaces = Array.zeroCreate<int> length

    for i in 0 .. input.Length - 1 do
        if i % 2 = 0 then
            files[i / 2] <- int input[i] - int '0'
        else
            freeSpaces[i / 2] <- int input[i] - int '0'

    files, freeSpaces

let moveFileBlocks (input: DiskMap) =
    let files, freeSpaces = split input
    let target = Array.create<int> (Array.sum files) -1

    let rec moveFront frontIdx backIdx idx =
        let rec imp idx =
            if files[frontIdx] = 0 then
                idx
            else
                target[idx] <- frontIdx
                files[frontIdx] <- files[frontIdx] - 1
                imp (idx + 1)

        if idx < target.Length then
            let newIdx = imp idx
            moveBack frontIdx backIdx newIdx

    and moveBack frontIdx backIdx idx =
        let rec imp backIdx idx =
            if freeSpaces[frontIdx] = 0 || backIdx < 0 then
                backIdx, idx
            else if files[backIdx] = 0 then
                imp (backIdx - 1) idx
            else
                target[idx] <- backIdx
                files[backIdx] <- files[backIdx] - 1
                freeSpaces[frontIdx] <- freeSpaces[frontIdx] - 1
                imp backIdx (idx + 1)

        let newBackIdx, newIdx = imp backIdx idx
        moveFront (frontIdx + 1) newBackIdx newIdx

    let frontIndex = 0
    let backIndex = files.Length - 1
    moveFront frontIndex backIndex 0
    target

let checkSum (blocks: int []) =
    Seq.indexed blocks
    |> Seq.sumBy (fun (idx, block) ->
        if block <> -1 then
            int64 idx * int64 block
        else
            0L)

let partOne (input: DiskMap) = moveFileBlocks input |> checkSum

let moveWholeFiles (input: DiskMap) =
    let files, freeSpaces = split input
    let permutations = Array.create (Array.length files) -1

    let rec imp idx =
        if idx >= 0 then
            let span = freeSpaces.AsSpan().Slice(0, idx)

            // Find the first free space that can fit
            let mutable i = 0
            let mutable spaceIdx = -1

            while spaceIdx < 0 && i < span.Length do
                if freeSpaces[i] >= files[idx] then
                    spaceIdx <- i

                i <- i + 1

            if spaceIdx = -1 then
                imp (idx - 1)
            else
                // Mark permutation and adjust free spaces
                permutations[idx] <- spaceIdx
                freeSpaces[spaceIdx] <- freeSpaces[spaceIdx] - files[idx]

                if not (Array.contains idx permutations) then
                    freeSpaces[idx] <- freeSpaces[idx] + files[idx]
                else
                    // In the case that permutation exists on our index
                    // we need to pass the available space to the free space before

                    // See the example below when inspecting index 2
                    // 0099.|111777|244.|333.|...|5555.|6666.|....|8888|..
                    // 00992|111777.|44.|333.|...|5555.|6666.|....|8888|..
                    freeSpaces[idx - 1] <- freeSpaces[idx - 1] + files[idx]

                imp (idx - 1)

    imp (Array.length files - 1)

    let result = Array.create (Array.sum files + Array.sum freeSpaces) -1
    let mutable resultIdx = 0

    for idx in 0 .. files.Length - 1 do
        if permutations[idx] = -1 then
            for _ in 0 .. files[idx] - 1 do
                result[resultIdx] <- idx
                resultIdx <- resultIdx + 1

        let mutable span = permutations.AsSpan()
        let mutable permutationIdx = span.LastIndexOf(idx)

        while permutationIdx <> -1 do
            for _ in 0 .. files[permutationIdx] - 1 do
                result[resultIdx] <- permutationIdx
                resultIdx <- resultIdx + 1

            span <- span.Slice(0, permutationIdx)
            permutationIdx <- span.LastIndexOf(idx)

        for _ in 0 .. freeSpaces[idx] - 1 do
            resultIdx <- resultIdx + 1

    result

let partTwo (input: DiskMap) = moveWholeFiles input |> checkSum

let diskMap = File.ReadAllText("./input/day09.txt")

partOne diskMap

#time
partTwo diskMap
#time