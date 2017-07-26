module Turnabout.BoardTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Board as Board exposing (..)


suite : Test
suite =
    describe "The game board that marbles, goals, and blocks fall against"
        [ test "contains walls" <|
            \_ ->
                Board.empty
                    |> Board.insertWall ( 0, 0 )
                    |> Board.at ( 0, 0 )
                    |> Expect.equal (Just Wall)
        , test "contains floors" <|
            \_ ->
                Board.empty
                    |> Board.insertWall ( 1, 0 )
                    |> Board.insertWall ( 0, 1 )
                    |> Board.insertWall ( 2, 1 )
                    |> Board.insertWall ( 1, 2 )
                    |> Board.insertFloor ( 1, 1 )
                    |> Board.at ( 1, 1 )
                    |> Expect.equal (Just Floor)
        , test "has a width and height" <|
            \_ ->
                let
                    (Board.Board _ size) =
                        Board.empty
                            |> Board.insertWall ( 1, 0 )
                            |> Board.insertWall ( 0, 1 )
                            |> Board.insertWall ( 2, 1 )
                            |> Board.insertWall ( 1, 2 )
                            |> Board.insertFloor ( 1, 1 )
                in
                    Expect.equal (Size 3 3) size
        , test "can tell you if a coordinate is a wall" <|
            \_ ->
                Board.empty
                    |> Board.insertWall ( 0, 0 )
                    |> Board.isWall ( 0, 0 )
                    |> Expect.equal True
        ]
