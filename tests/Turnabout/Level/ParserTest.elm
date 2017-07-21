module Turnabout.Level.ParserTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Level.Parser as Parser exposing (Level, Size)
import Dict


twoByTwoWalls =
    """
##
##
"""


oneMarble =
    """
####
#.r#
####
"""


wholeNineYards =
    """
########
#.....R#
#r..11##
##.##1##
#....1##
#.#111##
#b....B#
########
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
        , test "can parse a board with a marble"
            (run
                (let
                    board =
                        Dict.fromList
                            [ ( ( 0, 0 ), Parser.Wall )
                            , ( ( 1, 0 ), Parser.Wall )
                            , ( ( 2, 0 ), Parser.Wall )
                            , ( ( 3, 0 ), Parser.Wall )
                            , ( ( 0, 1 ), Parser.Wall )
                            , ( ( 1, 1 ), Parser.Floor )
                            , ( ( 2, 1 ), Parser.Floor )
                            , ( ( 3, 1 ), Parser.Wall )
                            , ( ( 0, 2 ), Parser.Wall )
                            , ( ( 1, 2 ), Parser.Wall )
                            , ( ( 2, 2 ), Parser.Wall )
                            , ( ( 3, 2 ), Parser.Wall )
                            ]

                    movables =
                        [ Parser.Marble Parser.Red ( 2, 1 ) ]
                 in
                    (Expect.equal
                        (Parser.parse oneMarble)
                        (Level board movables (Size 4 3))
                    )
                )
            )
        , test "can parse a board with marbles and blocks"
            (run
                (let
                    board =
                        Dict.fromList
                            [ ( ( 0, 0 ), Parser.Wall )
                            , ( ( 1, 0 ), Parser.Wall )
                            , ( ( 2, 0 ), Parser.Wall )
                            , ( ( 3, 0 ), Parser.Wall )
                            , ( ( 4, 0 ), Parser.Wall )
                            , ( ( 5, 0 ), Parser.Wall )
                            , ( ( 6, 0 ), Parser.Wall )
                            , ( ( 7, 0 ), Parser.Wall )

                            -- row 2
                            , ( ( 0, 1 ), Parser.Wall )
                            , ( ( 1, 1 ), Parser.Floor )
                            , ( ( 2, 1 ), Parser.Floor )
                            , ( ( 3, 1 ), Parser.Floor )
                            , ( ( 4, 1 ), Parser.Floor )
                            , ( ( 5, 1 ), Parser.Floor )
                            , ( ( 6, 1 ), Parser.Floor )
                            , ( ( 7, 1 ), Parser.Wall )

                            -- row 3
                            , ( ( 0, 2 ), Parser.Wall )
                            , ( ( 1, 2 ), Parser.Floor )
                            , ( ( 2, 2 ), Parser.Floor )
                            , ( ( 3, 2 ), Parser.Floor )
                            , ( ( 4, 2 ), Parser.Floor )
                            , ( ( 5, 2 ), Parser.Floor )
                            , ( ( 6, 2 ), Parser.Wall )
                            , ( ( 7, 2 ), Parser.Wall )

                            -- row 4
                            , ( ( 0, 3 ), Parser.Wall )
                            , ( ( 1, 3 ), Parser.Wall )
                            , ( ( 2, 3 ), Parser.Floor )
                            , ( ( 3, 3 ), Parser.Wall )
                            , ( ( 4, 3 ), Parser.Wall )
                            , ( ( 5, 3 ), Parser.Floor )
                            , ( ( 6, 3 ), Parser.Wall )
                            , ( ( 7, 3 ), Parser.Wall )

                            -- row 5
                            , ( ( 0, 4 ), Parser.Wall )
                            , ( ( 1, 4 ), Parser.Floor )
                            , ( ( 2, 4 ), Parser.Floor )
                            , ( ( 3, 4 ), Parser.Floor )
                            , ( ( 4, 4 ), Parser.Floor )
                            , ( ( 5, 4 ), Parser.Floor )
                            , ( ( 6, 4 ), Parser.Wall )
                            , ( ( 7, 4 ), Parser.Wall )

                            -- row 6
                            , ( ( 0, 5 ), Parser.Wall )
                            , ( ( 1, 5 ), Parser.Floor )
                            , ( ( 2, 5 ), Parser.Wall )
                            , ( ( 3, 5 ), Parser.Floor )
                            , ( ( 4, 5 ), Parser.Floor )
                            , ( ( 5, 5 ), Parser.Floor )
                            , ( ( 6, 5 ), Parser.Wall )
                            , ( ( 7, 5 ), Parser.Wall )

                            -- row 7
                            , ( ( 0, 6 ), Parser.Wall )
                            , ( ( 1, 6 ), Parser.Floor )
                            , ( ( 2, 6 ), Parser.Floor )
                            , ( ( 3, 6 ), Parser.Floor )
                            , ( ( 4, 6 ), Parser.Floor )
                            , ( ( 5, 6 ), Parser.Floor )
                            , ( ( 6, 6 ), Parser.Floor )
                            , ( ( 7, 6 ), Parser.Wall )

                            -- row 8
                            , ( ( 0, 7 ), Parser.Wall )
                            , ( ( 1, 7 ), Parser.Wall )
                            , ( ( 2, 7 ), Parser.Wall )
                            , ( ( 3, 7 ), Parser.Wall )
                            , ( ( 4, 7 ), Parser.Wall )
                            , ( ( 5, 7 ), Parser.Wall )
                            , ( ( 6, 7 ), Parser.Wall )
                            , ( ( 7, 7 ), Parser.Wall )
                            ]

                    movables =
                        [ Parser.Marble Parser.Red ( 1, 2 )
                        , Parser.Marble Parser.Blue ( 1, 6 )
                        , Parser.Goal Parser.Red ( 1, 6 )
                        , Parser.Goal Parser.Blue ( 6, 6 )
                        , Parser.Block (Parser.BlockId 1)
                            [ ( 4, 2 )
                            , ( 5, 2 )
                            , ( 5, 3 )
                            , ( 5, 4 )
                            , ( 3, 5 )
                            , ( 4, 5 )
                            , ( 5, 5 )
                            ]
                        ]
                 in
                    (Expect.equal
                        (Parser.parse wholeNineYards)
                        (Level board movables (Size 8 8))
                    )
                )
            )
        ]
