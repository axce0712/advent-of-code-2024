open System.IO
open System

type Direction =
    | North
    | South
    | West
    | East

type Position = int * int

type State =
    { Position: Position
      Direction: Direction
      Score: int
      Visited: Set<Position> }

let getLineWidth (map: string) =
    (map |> Seq.findIndex (fun c -> c = '\n')) + 1

let getPosition lineWidth index = index % lineWidth, index / lineWidth

let toIndex lineWidth (x, y) = y * lineWidth + x

let getStartPosition lineWidth (map: string) =
    let position =
        map.IndexOf('S')
        |> getPosition lineWidth

    { Position = position
      Direction = East
      Score = 0
      Visited = Set.add position Set.empty }

let rotateClockwise direction =
    match direction with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let rotateCounterClockwise direction =
    match direction with
    | North -> West
    | West -> South
    | South -> East
    | East -> North

let add direction (x, y) =
    match direction with
    | North -> x, y - 1
    | South -> x, y + 1
    | West -> x - 1, y
    | East -> x + 1, y

let tryMove f lineWidth (map: string) increment bestScores state =
    let direction = f state.Direction
    let nextPosition = add direction state.Position
    let nextPositionIdx = toIndex lineWidth nextPosition

    if
        not (Set.contains nextPosition state.Visited)
        && (map[nextPositionIdx] = '.' || map[nextPositionIdx] = 'E')
    then
        let newScore = state.Score + increment

        let scoreSoFar =
            bestScores
            |> Map.tryFind (nextPosition, direction)
            |> Option.defaultValue Int32.MaxValue

        if newScore < scoreSoFar then
            let newBestScores =
                bestScores
                |> Map.add (nextPosition, direction) newScore

            let newState =
                { Position = nextPosition
                  Score = newScore
                  Direction = direction
                  Visited = Set.add nextPosition state.Visited }

            Some(newState, newBestScores)
        else
            None
    else
        None

let tryAhead lineWidth (map: string) state = tryMove id lineWidth map 1 state

let tryClockwise lineWidth (map: string) state =
    tryMove rotateClockwise lineWidth map 1001 state

let tryCounterClockwise lineWidth (map: string) state =
    tryMove rotateCounterClockwise lineWidth map 1001 state

let moves =
    [ tryAhead
      tryClockwise
      tryCounterClockwise ]

let move lineWidth (map: string) bestScores state =
    moves
    |> List.fold
        (fun ((statesSoFar, bestScoresSoFar) as acc) move ->
            move lineWidth map bestScoresSoFar state
            |> Option.map (fun (newState, newBestScores) -> newState :: statesSoFar, newBestScores)
            |> Option.defaultValue acc)
        ([], bestScores)

let collect lineWidth (map: string) bestScores candidates =
    let mutable completed = []
    let mutable nextCandidates = []
    let mutable bestScoresSoFar = bestScores

    for candidate in candidates do
        let nextStates, newBestScores = move lineWidth map bestScoresSoFar candidate

        for nextState in nextStates do
            let idx = nextState.Position |> toIndex lineWidth

            if map[idx] = 'E' then
                completed <- nextState.Score :: completed
            else
                nextCandidates <- nextState :: nextCandidates

        bestScoresSoFar <- newBestScores

    completed, nextCandidates, bestScoresSoFar

let run (map: string) =
    let rec imp lineWidth map completedSoFar bestScores candidates =
        let completed, nextCandidates, newBestScores =
            collect lineWidth map bestScores candidates

        let mutable newCompleted = completedSoFar

        for score in completed do
            newCompleted <- score :: newCompleted

        match nextCandidates with
        | [] -> List.min newCompleted
        | xs -> imp lineWidth map newCompleted newBestScores xs

    let lineWidth = getLineWidth map
    let startPosition = getStartPosition lineWidth map
    let nextPositions, bestScores = move lineWidth map Map.empty startPosition
    imp lineWidth map [] bestScores nextPositions

let map =
    @"#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"

let input = File.ReadAllText("./input/day16.txt")

run input
