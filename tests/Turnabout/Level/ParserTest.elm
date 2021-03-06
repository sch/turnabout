module Turnabout.Level.ParserTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Board as Board exposing (Board, Tile(Wall, Floor))
import Turnabout.Color as Color
import Turnabout.Marble as Marble
import Turnabout.Level.Parser as Parser
import Turnabout.Level.Model as Level exposing (Level, Movable(..), BlockId(..))


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


wholeNineYardsMovables : List Movable
wholeNineYardsMovables =
    [ Murble Marble.red ( 1, 2 )
    , Murble Marble.blue ( 1, 6 )
    , Goal Color.Red ( 1, 6 )
    , Goal Color.Blue ( 6, 6 )
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
        [ test "can parse a two-dimensional board" <|
            \_ ->
                Parser.parse twoByTwoWalls
                    |> .board
                    |> Board.isWall ( 1, 1 )
                    |> Expect.true "Expected there to be a wall"
        , test "can parse a board with a marble" <|
            \_ ->
                Parser.parse oneMarble
                    |> .movables
                    |> Expect.equal [ Murble Marble.red ( 2, 1 ) ]
        , test "can parse a board with marbles and blocks" <|
            \_ ->
                Parser.parse wholeNineYards
                    |> Level.blockAt ( 5, 2 )
                    |> Expect.true "There should be a block at (5, 2)"
        ]
