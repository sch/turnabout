module LevelTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Types exposing (..)
import Turnabout.Level as Level exposing (Cardinality(..))
import Dict


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


run =
    always


suite : Test
suite =
    describe "Level data and generation"
        [ test
            "it can convert a 2D array to a dict of coordinates"
            (run (Expect.equal (Level.toCoordinateDict twoDimensionalList) fixture))
        , test
            "Can determine cardinality by rotating clockwise"
            (run (Expect.equal (Level.determineCardinality [ Clockwise, Clockwise, Clockwise ]) East))
        , test
            "Can determine cardinality by rotating counter-clockwise"
            (run (Expect.equal (Level.determineCardinality [ CounterClockwise ]) East))
        ]
