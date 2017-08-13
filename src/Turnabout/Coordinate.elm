module Turnabout.Coordinate exposing (Coordinate, add, subtract, byOne, walk)

import Turnabout.Direction as Direction exposing (Cardinal(..))


type alias Coordinate =
    ( Int, Int )


type alias Distance =
    Int


type alias Path
    = List Cardinal


{-| The coordinate, moved one unit in the given direction
-}
by : Distance -> Direction.Cardinal -> Coordinate -> Coordinate
by distance direction ( x, y ) =
    case direction of
        South ->
            ( x, y + distance )

        West ->
            ( x - distance, y )

        North ->
            ( x, y - distance )

        East ->
            ( x + distance, y )


{-| The coordinate, moved one unit in the given direction
-}
byOne : Direction.Cardinal -> Coordinate -> Coordinate
byOne =
    by 1


{-| The coordinate, after moving along the path of a couple directions
-}
walk : Path -> Coordinate -> Coordinate
walk directions current =
    case directions of
        [] ->
            current

        direction :: rest ->
            walk rest (byOne direction current)


add : Coordinate -> Coordinate -> Coordinate
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


subtract : Coordinate -> Coordinate -> Coordinate
subtract ( x2, y2 ) ( x1, y1 ) =
    ( x2 - x1, y2 - y1 )
