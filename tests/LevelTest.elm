module LevelTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Level as Level exposing (LayeredLevel, Size)
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


twoByTwoWalls =
    """
##
##
"""


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
        , test "can parse a single board tile"
            (run
                (Expect.equal
                    (Level.parse "#")
                    (LayeredLevel (Dict.singleton ( 0, 0 ) Level.Wall) [] (Size 1 1))
                )
            )
        , test "can parse a two-dimensional board"
            (run
                (let
                    board =
                        Dict.fromList
                            [ ( ( 0, 0 ), Level.Wall )
                            , ( ( 0, 1 ), Level.Wall )
                            , ( ( 1, 0 ), Level.Wall )
                            , ( ( 1, 1 ), Level.Wall )
                            ]
                 in
                    (Expect.equal
                        (Level.parse twoByTwoWalls)
                        (LayeredLevel board [] (Size 2 2))
                    )
                )
            )
        ]
