module LevelTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Level as Level
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


run =
    always


suite : Test
suite =
    describe "Level data and generation"
        [ test
            "it can convert a 2D array to a dict of coordinates"
            (run
                (Expect.equal
                    (Level.toCoordinateDict twoDimensionalList)
                    fixture
                )
            )
        ]
