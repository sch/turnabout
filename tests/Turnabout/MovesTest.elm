module Turnabout.MovesTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Moves as Moves exposing (..)
import Turnabout.Direction exposing (Cardinal(..))


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
                    |> Moves.toDirection
                    |> Expect.equal West
        , test "Can determine cardinality by rotating counter-clockwise" <|
            \_ ->
                Moves.initial
                    |> Moves.rotateCounterClockwise
                    |> Moves.toDirection
                    |> Expect.equal East
        ]
