module Day6 exposing (..)

import Util exposing (int, minimum, maximum)

import Set exposing (Set)

type alias Point = (Int, Int)
type alias Bbox = {xMin: Int, yMin: Int, xMax: Int, yMax: Int}


part1 = 
    let
        points = testInput |> parse
        bbox = createBbox points
    in
        -- visit points bbox (1,1)
        bbox


outside : Bbox -> Point -> Bool
outside bbox (x,y) = 
    x<bbox.xMin || x>bbox.xMax || y<bbox.yMin || y>bbox.yMax


maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd a b = 
    case a of
        Nothing -> Nothing
        Just aValue -> 
            case b of
                Nothing -> Nothing
                Just bValue -> Just (aValue + bValue)





testInput = """1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"""

closest : List Point -> Point -> Maybe Point
closest points p = 
    let
        pointsWithDistances = 
            points 
                |> List.map (\ p2 -> (p2, distance p p2))
                |> List.sortBy Tuple.second

        _ = Debug.log "lskdl" pointsWithDistances

    in
        case pointsWithDistances of
            (a, distA) :: (b, distB)  :: _ -> if distA /= distB then Just a else Nothing
            _ -> Debug.todo "Illegal state"


distance : Point -> Point -> Int
distance (ax, ay) (bx, by) =

    abs(ax - bx) + abs(ay - by)


createBbox : List Point -> Bbox
createBbox points =
    Bbox
        (minimum <| List.map Tuple.first points)
        (minimum <| List.map Tuple.second points)
        (maximum <| List.map Tuple.first points)
        (maximum <| List.map Tuple.second points)

parse : String -> List Point
parse input = 
    String.lines input
    |> List.map parseLine

parseLine : String -> Point
parseLine line = 
    let
        tokens = String.split "," line
    in
        case tokens of
            [x,y] -> ((int x), (int y))
            _ -> Debug.todo ("Invalid input line: " ++ line)

puzzleInput = """252, 125
128, 333
89, 324
141, 171
266, 338
117, 175
160, 236
234, 202
165, 192
204, 232
83, 192
229, 178
333, 57
70, 243
108, 350
161, 63
213, 277
87, 299
163, 68
135, 312
290, 87
73, 246
283, 146
80, 357
66, 312
159, 214
221, 158
175, 54
298, 342
348, 162
249, 90
189, 322
311, 181
194, 244
53, 295
80, 301
262, 332
268, 180
139, 287
115, 53
163, 146
220, 268
79, 85
95, 112
349, 296
179, 274
113, 132
158, 264
316, 175
268, 215"""