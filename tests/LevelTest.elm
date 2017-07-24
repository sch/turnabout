module LevelTest exposing (suite)

import Test exposing (..)
import Expect
import Dict
import Fixtures.Level
import Turnabout.Level as Level
import Turnabout.Level.Types exposing (..)
import Turnabout.Moves as Moves


fixture =
    Dict.fromList
        [ ( ( 0, 0 ), "a" )
        , ( ( 0, 1 ), "b" )
        , ( ( 1, 0 ), "c" )
        , ( ( 1, 1 ), "d" )
        ]


twoDimensionalList =
    [ [ "a", "b" ], [ "c", "d" ] ]



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
        [ test "it can convert a 2D array to a dict of coordinates" <|
            \_ ->
                Level.toCoordinateDict twoDimensionalList
                    |> Expect.equal fixture
        , test "Can apply game moves" <|
            \_ ->
                newLevel.movables
                    |> Expect.equal [ Goal Red ( 4, 4 ), Marble Red ( 1, 1 ) ]
        ]
