module Turnabout.LevelTest exposing (suite)

import Test exposing (..)
import Expect
import Fixtures.Level
import Turnabout.Level as Level exposing (Level)
import Turnabout.Level.Model as Model exposing (Movable(..), Color(..), MovableId(..))
import Turnabout.Block as Block
import Turnabout.Moves as Moves


-- SIDENOTE
--
-- I kinda hate elm's testing DSL. I was thinking: what do I like? I like
-- clojure.spec. I like Elm's HTML api. What do I like about these two? Well,
-- they describe _data_. But something that mimics rspec is not the way. Maybe
-- this could work?
--
--
-- suite [ test [ description "it can convert a 2D array to a dict of coordinates"
--              , function Level.toCoordinateDict
--              , input fixture
--              , output twoDimensionalList
--              ]
--       , test [ description "it can ...
--              , function Level.size
--              , ...
--              ]
--       ]


moves =
    Moves.initial
        |> Moves.rotateCounterClockwise
        |> Moves.rotateCounterClockwise


newLevel =
    Level.applyMoves moves Fixtures.Level.levelOne


suite : Test
suite =
    describe "Level data and generation"
        [ test "Can apply game moves" <|
            \_ ->
                newLevel.movables
                    |> Expect.equal [ Goal Red ( 4, 4 ), Marble Red ( 1, 1 ) ]
        , test "holds blocks" <|
            \_ ->
                newLevel
                    |> Level.insertBlock 5 ( 6, 4 )
                    |> Model.getBlock (MovableId 5)
                    |> Expect.equal (Just Block.singleton)
        , test "holds positions of movable pieces" <|
            \_ ->
                newLevel
                    |> Level.insertBlock 5 ( 6, 4 )
                    |> Model.positionOf (MovableId 5)
                    |> Expect.equal (Just ( 6, 4 ))
        , test "knows whether a block is occupying a space" <|
            \_ ->
                newLevel
                    |> Level.insertBlock 5 ( 6, 3 )
                    |> Level.insertBlock 5 ( 6, 4 )
                    |> Level.blockAt ( 6, 4 )
                    |> Expect.true "Expected a block"
        , test "treats the position of a block as the part occupying the top-left corner" <|
            \_ ->
                newLevel
                    |> Level.insertBlock 5 ( 6, 3 )
                    |> Level.insertBlock 5 ( 6, 4 )
                    |> Level.positionOf 5
                    |> Expect.equal (Just ( 6, 3 ))
        ]
