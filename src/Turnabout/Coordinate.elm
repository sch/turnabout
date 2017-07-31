module Turnabout.Coordinate
    exposing
        ( Coordinate
        , add
        , subtract
        , byOne
        )

import Turnabout.Direction exposing (Direction(..))


type alias Coordinate =
    ( Int, Int )

type alias Distance = Int

{-| The coordinate, moved one unit in the given direction
-}
by: Distance -> Direction -> Coordinate -> Coordinate
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
byOne : Direction -> Coordinate -> Coordinate
byOne = by 1

add : Coordinate -> Coordinate -> Coordinate
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


subtract : Coordinate -> Coordinate -> Coordinate
subtract ( x2, y2 ) ( x1, y1 ) =
    ( x2 - x1, y2 - y1 )
