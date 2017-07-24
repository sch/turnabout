module Turnabout.MovesTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Moves as Moves exposing (..)
import Turnabout.Cardinality exposing (Cardinality(..))


suite : Test
suite =
    describe "Level data and generation"
        [ test "Can determine cardinality by rotating clockwise" <|
            \_ ->
                Moves.initial
                    |> Moves.rotateClockwise
                    |> Moves.rotateClockwise
                    |> Moves.rotateClockwise
                    |> Moves.rotateClockwise
                    |> Moves.rotateClockwise
                    |> Moves.toCardinality
                    |> Expect.equal West
        , test "Can determine cardinality by rotating counter-clockwise" <|
            \_ ->
                Moves.initial
                    |> Moves.rotateCounterClockwise
                    |> Moves.toCardinality
                    |> Expect.equal East
        ]
