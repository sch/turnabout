module Turnabout.Level exposing (Tile(..), Color(..), Level, get, size, all)

import Dict exposing (Dict)


type Color
    = Red
    | Green
    | Blue


type Tile
    = Wall
    | Block
    | Marble Color
    | Goal Color
    | Floor
    | Empty


type alias Level =
    List (List Tile)


type alias Size =
    { width : Int, height : Int }


type alias Coordinate =
    ( Int, Int )


type Movable
    = Coordinate Tile


type alias LayeredLevel =
    { board : Dict Coordinate Tile
    , movables : List Movable
    , size : Size
    }


size : Level -> Size
size level =
    { width = level |> List.map List.length |> List.maximum |> Maybe.withDefault 0
    , height = List.length level
    }


one =
    """
########
#......#
#......#
#......#
#r.#R..#
########
"""


two =
    """
#########
#r..#..r#
##..#..##
 #.....#
 #.....#
##.....##
#.......#
#########
"""


three =
    """
##########
#......###
#.####.###
#.##.#.###
#.#R.....#
#.##.#.#.#
#.#..#.#.#
#.#......#
#r#.####.#
##########
"""


four =
    """
########
#...#..#
#.#r#R.#
#.####.#
#.#1##.#
#.#1##.#
#......#
########
"""


five =
    """
##########
#........#
#.####2#.#
#.33##2#.#
#.######.#
#...#....#
###.#.#1##
#44.#.#1##
###R#r####
##########
"""


six =
    """
#######
#...#.#
#.1.r.#
#R111.#
##111.#
 #111.##
 #111..#
 #.#####
 ###
"""


seven =
    """
#########
#.......#
#.#####.#
#.##.##.#
#.##.##.#
#.##22..##
#.##1##..#
#r#R1....#
##########
"""


eight =
    """
####
#...#
#....#
#.....#
#r.....#
 #......#
  #......#
   #......#
    #......#
     #......#
      #....11#
       #...11#
        #...r#
         #####
"""


nine =
    """
#####
#r..##
###..##
  ##1.##
   ##..#
   ##..#
  ##23##
###45##
#R67##
#####
"""


ten =
    """
############
#..........#
#.#..R..#..#
#......#...#
#...#......#
#.#..r...#.#
#....#.....#
#.#.....#..#
#...#.#....#
#..#.....#.#
#..........#
############
"""


eleven =
    """
#########
#.......#
#b...#.r#
####.####
   #.#
####.####
#R.....B#
#########
"""


twelve =
    """
      #
      ##
      #B#
      #..#
   ####..#
  #......#
 #r....#.#####
#####.#....b#
    #......#
    #..####
    #..#
     #R#
      ##
       #
"""


thirteen =
    """
####    ####
#..######..#
#b........r#
##........##
 #..#..#..#
##..B..R..##
###......###
##........##
 ##......##
  ########
"""


fourteen =
    """
#######
#.....##
##..#..##
#..b##.R#
#..##..##
#....b##
#..####
#..#
#.r#
####
"""


fifteen =
    """
########
#.....R#
#r..11##
##.##1##
#....1##
# #111##
#b....B#
########
"""


sixteen =
    """
 ########
 #......#
##.####.#
#.....#.#
#.###.#.#
#.###.#.#
#R#br.#B#
#########
"""


seventeen =
    """
 ########
 #......#
##.####.#
#.....#.#
#.###.#.#
#.###.#.#
#R#rb.#B#
#########
"""


eighteen =
    """
  ######
  #....#
  #.rb.#
  #.##.#
  #..#.#
  #..#.#
####.#.#
#...B###
#r#..R#
#######
"""


nineteen =
    """
###       ###
#.#       #.#
#1.#     #.2#
#b.#     #.r#
 #..#   #..#
  #..###..#
  #.......#
 #.........#
#..R.....B..#
#...........#
#....#.#....#
 #....#....#
  #..#.#..#
   #######
"""


twenty =
    """
  #######
###.....#
#.......#
#.......#
#...B...#
#.b...r.#
###R..###
  #...#
  #####
"""


all =
    List.map parseLevelString
        [ one
        , two
        , three
        , four
        , five
        , six
        , seven
        , eight
        , nine
        , ten
        , eleven
        , twelve
        , thirteen
        , fourteen
        , fifteen
        , sixteen
        , seventeen
        , eighteen
        , nineteen
        , twenty
        ]


get : Int -> Level
get number =
    all
        |> List.drop (number - 1)
        |> List.head
        |> Maybe.withDefault (parseLevelString one)


empty : LayeredLevel
empty =
    { board = Dict.empty
    , movables = []
    , size = { width = 0, height = 0 }
    }


parse : String -> LayeredLevel
parse levelString =
    let
        rows =
            levelString |> String.lines |> List.indexedMap parseHelp
    in
        empty


parseHelp : Int -> String -> LayeredLevel
parseHelp rowIndex rowString =
    let
        pairs =
            rowString
                |> String.toList
                |> List.indexedMap (\colIndex char -> ( ( colIndex, rowIndex ), parseLevelChar char ))
    in
        { empty | board = Dict.fromList pairs }


parseLevelString : String -> Level
parseLevelString string =
    string |> String.lines |> List.map parseLevelRow


parseLevelRow row =
    row |> String.toList |> (List.map parseLevelChar)


parseLevelChar : Char -> Tile
parseLevelChar char =
    case char of
        '#' ->
            Wall

        'r' ->
            Marble Red

        'g' ->
            Marble Green

        'b' ->
            Marble Blue

        'R' ->
            Goal Red

        'G' ->
            Goal Green

        'B' ->
            Goal Blue

        '1' ->
            Block

        '2' ->
            Block

        '3' ->
            Block

        '4' ->
            Block

        '5' ->
            Block

        '6' ->
            Block

        '7' ->
            Block

        '8' ->
            Block

        '9' ->
            Block

        '.' ->
            Floor

        _ ->
            Empty
