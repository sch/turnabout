module Turnabout.CoordinateTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Coordinate as Coordinate
import Turnabout.Direction exposing (Cardinal(..))


suite : Test
suite =
    describe "Coordinates are points holding x and y location"
        [ test "the coordinate if you walk along a path of directions" <|
            \_ ->
                ( 0, 0 )
                    |> Coordinate.walk [ North, West, South, South, East, East ]
                    |> Expect.equal ( 1, 1 )
        ]
