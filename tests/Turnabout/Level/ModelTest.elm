module Turnabout.Level.ModelTest exposing (suite)

import Test exposing (..)
import Expect
import Fixtures.Level
import Turnabout.Level.Model as Level exposing (Level, Movable(..), MovableId(..))
import Turnabout.Block as Block
import Turnabout.Color exposing (Color(..))
import Turnabout.Moves as Moves
import Turnabout.Marble as Marble


-- In this test, we use the first level of the game to build on top of. Each
-- adds some elements to the playfield depending on what they're testing, but
-- hopefully it strikes a balance between expressive and obvious.


moves =
    Moves.initial
        |> Moves.rotateCounterClockwise
        |> Moves.rotateCounterClockwise


newLevel =
    Level.applyMoves moves Fixtures.Level.levelOne


suite : Test
suite =
    describe "Level data and generation"
        [ skip <|
            test "Can apply game moves" <|
                \_ ->
                    Fixtures.Level.levelOne
                        |> Level.applyMoves moves
                        |> .movables
                        |> Expect.equal [ Goal Red ( 4, 4 ), Murble Marble.red ( 1, 1 ) ]
        , skip <|
            test "holds blocks" <|
                \_ ->
                    Fixtures.Level.levelOne
                        |> Level.insertBlock (MovableId 5) ( 6, 4 )
                        |> Level.getBlock (MovableId 5)
                        |> Expect.equal (Just Block.singleton)
        , skip <|
            test "holds positions of movable pieces" <|
                \_ ->
                    Fixtures.Level.levelOne
                        |> Level.insertBlock (MovableId 5) ( 6, 4 )
                        |> Level.positionOf (MovableId 5)
                        |> Expect.equal (Ok ( 6, 4 ))
        , skip <|
            test "knows whether a block is occupying a space" <|
                \_ ->
                    Fixtures.Level.levelOne
                        |> Level.insertBlock (MovableId 5) ( 6, 3 )
                        |> Level.insertBlock (MovableId 5) ( 6, 4 )
                        |> Level.blockAt ( 6, 4 )
                        |> Expect.true "Expected a block"
        , skip <|
            test "treats the position of a block as the part occupying the top-left corner" <|
                \_ ->
                    Fixtures.Level.levelOne
                        |> Level.insertBlock (MovableId 5) ( 6, 3 )
                        |> Level.insertBlock (MovableId 5) ( 6, 4 )
                        |> Level.positionOf (MovableId 5)
                        |> Expect.equal (Ok ( 6, 3 ))
        , skip <|
            test "defines marbles as having a position and color tied to an identity" <|
                \_ ->
                    Fixtures.Level.levelOne
                        |> Level.positionOf (MovableId 10)
                        |> Expect.equal (Ok ( 6, 3 ))
        ]
