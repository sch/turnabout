module Turnabout.Level.ParserTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Level.Parser as Parser exposing (Level, Size)
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
    describe "Level data and generation from a string"
        [ test "can parse a single board tile"
            (run
                (Expect.equal
                    (Parser.parse "#")
                    (Level (Dict.singleton ( 0, 0 ) Parser.Wall) [] (Size 1 1))
                )
            )
        , test "can parse a two-dimensional board"
            (run
                (let
                    board =
                        Dict.fromList
                            [ ( ( 0, 0 ), Parser.Wall )
                            , ( ( 0, 1 ), Parser.Wall )
                            , ( ( 1, 0 ), Parser.Wall )
                            , ( ( 1, 1 ), Parser.Wall )
                            ]
                 in
                    (Expect.equal
                        (Parser.parse twoByTwoWalls)
                        (Level board [] (Size 2 2))
                    )
                )
            )
        ]
