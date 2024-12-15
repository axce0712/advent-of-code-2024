open System.IO

type Plant = char

type Garden = Plant [,]

module Garden =
    let height (garden: Garden) = Array2D.length1 garden

    let width (garden: Garden) = Array2D.length2 garden

    let item (x, y) (garden: Garden) = garden[y, x]

    let itemOrFallback (x, y) fallback (garden: Garden) =
        if x >= 0
           && x < Array2D.length2 garden
           && y >= 0
           && y < Array2D.length1 garden then
            garden[y, x]
        else
            fallback

    let isInRange (x, y) (garden: Garden) =
        x >= 0
        && x < Array2D.length2 garden
        && y >= 0
        && y < Array2D.length1 garden

let parse (lines: seq<string>) : Garden = array2D lines

let directions = [ (0, 1); (0, -1); (1, 0); (-1, 0) ]

let findArea plant (sx, sy) (garden: Garden) =
    let folder (x, y) (queue, area) (dx, dy) =
        let nx, ny = x + dx, y + dy

        if Garden.isInRange (nx, ny) garden
           && not (area |> Set.contains (nx, ny))
           && Garden.item (nx, ny) garden = plant then
            (nx, ny) :: queue, Set.add (nx, ny) area
        else
            queue, area

    let rec imp queue area =
        match queue with
        | [] -> area
        | pos :: rest ->
            let newQueue, newArea = directions |> List.fold (folder pos) (rest, area)
            imp newQueue newArea

    imp [ (sx, sy) ] (Set.add (sx, sy) Set.empty)

let findAreas (garden: Garden) =
    let folder areasSoFar pos =
        let plant = Garden.item pos garden

        match Map.tryFind plant areasSoFar with
        | Some areas when areas |> Seq.exists (Set.contains pos) -> areasSoFar
        | Some _
        | None ->
            let area = findArea plant pos garden

            areasSoFar
            |> Map.change plant (function
                | Some areasSoFar -> Some(area :: areasSoFar)
                | None -> Some [ area ])

    let xs = Seq.init (Garden.width garden) id
    let ys = Seq.init (Garden.height garden) id

    let positions =
        ys
        |> Seq.collect (fun y -> xs |> Seq.map (fun x -> x, y))

    positions |> Seq.fold folder Map.empty

let getRow (y: int) (start: int) (length: int) (garden: Garden) =
    seq {
        for x in 0 .. length - start - 1 do
            Garden.itemOrFallback (x, y) '.' garden
    }

let getColumn (x: int) (start: int) (length: int) (garden: Garden) =
    seq {
        for y in start .. length - start - 1 do
            Garden.itemOrFallback (x, y) '.' garden
    }

let (|Even|Uneven|) (p1, p2) = if p1 = p2 then Even else Uneven

let perimeter (before1, before2, positions) (current1, current2) =
    let b1, _ = before1
    let b2, _ = before2

    let newPositions =
        match b1, b2 with
        | Uneven -> before1 :: before2 :: positions
        | Even -> positions

    current1, current2, newPositions

let distinctSides (before1, before2, positions) (current1, current2) =
    let b1, (bx1, by1) = before1
    let b2, (bx2, by2) = before2
    let p1, _ = current1
    let p2, _ = current2

    let newPositions =
        match (b1, b2), (p1, p2) with
        | Uneven, Uneven ->
            let mutable newPositions = positions

            if b1 <> p1 then
                newPositions <- (b1, (bx1, by1)) :: newPositions

            if b2 <> p2 then
                newPositions <- (b2, (bx2, by2)) :: newPositions

            newPositions
        | Uneven, Even -> (b1, (bx1, by1)) :: (b2, (bx2, by2)) :: positions
        | _ -> positions

    current1, current2, newPositions

let findHorizontalSides f y (garden: Garden) =
    let row1 = getRow (y - 1) 0 (Garden.width garden + 1) garden
    let row2 = getRow y 0 (Garden.width garden + 1) garden
    let rowWithPositions1 = Seq.zip row1 (Seq.initInfinite (fun x -> (x, y - 1)))
    let rowWithPositions2 = Seq.zip row2 (Seq.initInfinite (fun x -> (x, y)))
    let initial1 = '.', (-1, y - 1)
    let initial2 = '.', (-1, y)

    let _, _, positions =
        Seq.zip rowWithPositions1 rowWithPositions2
        |> Seq.fold f (initial1, initial2, [])

    positions

let findVerticalSides f x (garden: Garden) =
    let column1 = getColumn (x - 1) 0 (Garden.height garden + 1) garden
    let column2 = getColumn x 0 (Garden.height garden + 1) garden
    let columnWithPositions1 = Seq.zip column1 (Seq.initInfinite (fun y -> (x - 1, y)))
    let columnWithPositions2 = Seq.zip column2 (Seq.initInfinite (fun y -> (x, y)))
    let initial1 = '.', (x - 1, -1)
    let initial2 = '.', (x, -1)

    let _, _, positions =
        Seq.zip columnWithPositions1 columnWithPositions2
        |> Seq.fold f (initial1, initial2, [])

    positions

let countSides f (garden: Garden) =
    let count =
        function
        | Some x -> Some(x + 1)
        | None -> Some 1

    Seq.collect
        id
        [ for y in 0 .. Garden.height garden do
              findHorizontalSides f y garden
          for x in 0 .. Garden.width garden do
              findVerticalSides f x garden ]
    |> Seq.fold (fun acc current -> acc |> Map.change current count) Map.empty

let solve f (garden: Garden) =
    let regions = findAreas garden
    let numberOfSides = countSides f garden

    let totalPrice =
        Map.toSeq regions
        |> Seq.sumBy (fun (_, areas) ->
            areas
            |> List.sumBy (fun positions ->
                let area = Set.count positions

                let sides =
                    positions
                    |> Seq.sumBy (fun pos ->
                        let key = Garden.item pos garden, pos

                        Map.tryFind key numberOfSides
                        |> Option.defaultValue 0)

                area * sides))

    totalPrice

let partOne (garden: Garden) = solve perimeter garden

let partTwo (garden: Garden) = solve distinctSides garden

let lines = File.ReadLines("./input/day12.txt")

let garden = parse lines

partOne garden
partTwo garden
