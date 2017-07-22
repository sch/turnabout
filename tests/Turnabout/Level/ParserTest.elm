module Turnabout.Level.ParserTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Level.Parser as Parser
import Turnabout.Level.Types exposing (..)
import Dict


twoByTwoWalls : String
twoByTwoWalls =
    """
##
##
"""


oneMarble : String
oneMarble =
    """
####
#.r#
####
"""


wholeNineYards : String
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


wholeNineYardsBoard : Board
wholeNineYardsBoard =
    Dict.fromList
        [ ( ( 0, 0 ), Wall )
        , ( ( 1, 0 ), Wall )
        , ( ( 2, 0 ), Wall )
        , ( ( 3, 0 ), Wall )
        , ( ( 4, 0 ), Wall )
        , ( ( 5, 0 ), Wall )
        , ( ( 6, 0 ), Wall )
        , ( ( 7, 0 ), Wall )

        -- row 2
        , ( ( 0, 1 ), Wall )
        , ( ( 1, 1 ), Floor )
        , ( ( 2, 1 ), Floor )
        , ( ( 3, 1 ), Floor )
        , ( ( 4, 1 ), Floor )
        , ( ( 5, 1 ), Floor )
        , ( ( 6, 1 ), Floor )
        , ( ( 7, 1 ), Wall )

        -- row 3
        , ( ( 0, 2 ), Wall )
        , ( ( 1, 2 ), Floor )
        , ( ( 2, 2 ), Floor )
        , ( ( 3, 2 ), Floor )
        , ( ( 4, 2 ), Floor )
        , ( ( 5, 2 ), Floor )
        , ( ( 6, 2 ), Wall )
        , ( ( 7, 2 ), Wall )

        -- row 4
        , ( ( 0, 3 ), Wall )
        , ( ( 1, 3 ), Wall )
        , ( ( 2, 3 ), Floor )
        , ( ( 3, 3 ), Wall )
        , ( ( 4, 3 ), Wall )
        , ( ( 5, 3 ), Floor )
        , ( ( 6, 3 ), Wall )
        , ( ( 7, 3 ), Wall )

        -- row 5
        , ( ( 0, 4 ), Wall )
        , ( ( 1, 4 ), Floor )
        , ( ( 2, 4 ), Floor )
        , ( ( 3, 4 ), Floor )
        , ( ( 4, 4 ), Floor )
        , ( ( 5, 4 ), Floor )
        , ( ( 6, 4 ), Wall )
        , ( ( 7, 4 ), Wall )

        -- row 6
        , ( ( 0, 5 ), Wall )
        , ( ( 1, 5 ), Floor )
        , ( ( 2, 5 ), Wall )
        , ( ( 3, 5 ), Floor )
        , ( ( 4, 5 ), Floor )
        , ( ( 5, 5 ), Floor )
        , ( ( 6, 5 ), Wall )
        , ( ( 7, 5 ), Wall )

        -- row 7
        , ( ( 0, 6 ), Wall )
        , ( ( 1, 6 ), Floor )
        , ( ( 2, 6 ), Floor )
        , ( ( 3, 6 ), Floor )
        , ( ( 4, 6 ), Floor )
        , ( ( 5, 6 ), Floor )
        , ( ( 6, 6 ), Floor )
        , ( ( 7, 6 ), Wall )

        -- row 8
        , ( ( 0, 7 ), Wall )
        , ( ( 1, 7 ), Wall )
        , ( ( 2, 7 ), Wall )
        , ( ( 3, 7 ), Wall )
        , ( ( 4, 7 ), Wall )
        , ( ( 5, 7 ), Wall )
        , ( ( 6, 7 ), Wall )
        , ( ( 7, 7 ), Wall )
        ]


wholeNineYardsMovables : List Movable
wholeNineYardsMovables =
    [ Marble Red ( 1, 2 )
    , Marble Blue ( 1, 6 )
    , Goal Red ( 1, 6 )
    , Goal Blue ( 6, 6 )
    , Block (BlockId 1)
        [ ( 4, 2 )
        , ( 5, 2 )
        , ( 5, 3 )
        , ( 5, 4 )
        , ( 3, 5 )
        , ( 4, 5 )
        , ( 5, 5 )
        ]
    ]


run =
    always


suite : Test
suite =
    describe "Level data and generation from a string"
        [ test "can parse a single board tile"
            (run
                (Expect.equal
                    (Parser.parse "#")
                    (Level (Dict.singleton ( 0, 0 ) Wall) [] (Size 1 1))
                )
            )
        , test "can parse a two-dimensional board"
            (run
                (let
                    board =
                        Dict.fromList
                            [ ( ( 0, 0 ), Wall )
                            , ( ( 0, 1 ), Wall )
                            , ( ( 1, 0 ), Wall )
                            , ( ( 1, 1 ), Wall )
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
                            [ ( ( 0, 0 ), Wall )
                            , ( ( 1, 0 ), Wall )
                            , ( ( 2, 0 ), Wall )
                            , ( ( 3, 0 ), Wall )
                            , ( ( 0, 1 ), Wall )
                            , ( ( 1, 1 ), Floor )
                            , ( ( 2, 1 ), Floor )
                            , ( ( 3, 1 ), Wall )
                            , ( ( 0, 2 ), Wall )
                            , ( ( 1, 2 ), Wall )
                            , ( ( 2, 2 ), Wall )
                            , ( ( 3, 2 ), Wall )
                            ]

                    movables =
                        [ Marble Red ( 2, 1 ) ]
                 in
                    (Expect.equal
                        (Parser.parse oneMarble)
                        (Level board movables (Size 4 3))
                    )
                )
            )
        , skip <|
            test "can parse a board with marbles and blocks" <|
                \_ ->
                    Parser.parse wholeNineYards
                        |> Expect.equal (Level wholeNineYardsBoard wholeNineYardsMovables (Size 8 8))
        ]
