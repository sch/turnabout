module Turnabout.Level
    exposing
        ( Tile(..)
        , Color(..)
        , Level
        , Size
        , get
        , size
        , all
        , toCoordinateDict
        , parse
        )

import Turnabout.Level.Parser as Parser
import Dict exposing (Dict)


-- re-exports


parse =
    Parser.parse



--- types


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | Purple


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


{-| Takes a two-dimensional list and creates a dictionary where the keys are
coordinate pairs for the location of the values in the 2d list.
dict =
Dict.fromList
[ ((0, 0), "a")
, ((0, 1), "b")
, ((1, 0), "c")
, ((1, 1), "d")
]
toCoordinateDict [["a", "b"], ["c", "d"]] == dict
-}
toCoordinateDict : List (List a) -> Dict ( Int, Int ) a
toCoordinateDict twoDee =
    let
        pairs =
            List.indexedMap addRowToPair twoDee |> List.concatMap identity

        addRowToPair rowIndex columns =
            List.indexedMap (\columnIndex item -> ( ( rowIndex, columnIndex ), item )) columns
    in
        Dict.fromList pairs


size : Level -> Size
size level =
    let
        width =
            level
                |> List.map List.length
                |> List.maximum
                |> Maybe.withDefault 0

        height =
            List.length level
    in
        Size width height


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


twentyone =
    """
##########
#r...##Y.#
####...#.#
#...P..G.#
#.#..#.###
#.#..#...#
#R######.#
#b#g##...#
#y#B.p.###
##########
"""


twentytwo =
    """
  ####
  #R.#
  #B.#
###G.#
#.1Y.#
#.#P.#
#.##.#
#.#p.#
#.2y.#
###g.#
  #b.#
  #r.#
  ####
"""


twentythree =
    """
##########
#....#.g.#
#r.#.b.#.#
##R###...#
#..##....#
#.#......#
#.g......#
#.b......#
#.##.....#
##########
"""


twentyfour =
    """
###########
#........##
#rgr..r...#
#########r#
#.........#
#.####.g..#
#....#....#
######...##
#gb..#..###
###gb...###
###########
"""


twentyfive =
    """
  ####
###R.#######
#........#.#
#........#.#
#....r###..#
#....B.....#
###..#.....#
#..##....#.#
#..G#...b..#
#....####.##
#.........#
#g........#
##..#######
 ####
"""


twentysix =
    """
  ###
  #.###
 ##.#.###
 #....11#
###.#.###
#223...#
##r3#4##
 ####4#
    #R#
    ###
"""


twentyseven =
    """
#####
#...#
#...##
#....#
#.r.##
#111#
#1.1#
#.22##
#.32.#
#.33.#
#..R##
#####
"""


twentyeight =
    """
     ####
      ##
      ##
#    ####    #
 #  #Y..Y#  #
  ####..####
   #......#
   #......#
 ###..1...###
#  #..11..#  #
   #.221..#
  ##y#2.#y##
 #  ##..##  #
#     ##     #
"""


twentynine =
    """
#########
#.......#
#....R..#
##.....##
 ##b.r##
  ##.##
   #.#
  #B.##
 ##...##
##.....##
#....#..#
#.......#
#########
"""


thirty =
    """
    ######
   ##....##
  ##......##
 ##........##
##..........#
#...........#
#......R....##
#...........B#
#...........##
##b........##
 ##r......##
  ##.....##
   ##...##
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
        , twentyone
        , twentytwo
        , twentythree
        , twentyfour
        , twentyfive
        , twentysix
        , twentyseven
        , twentyeight
        , twentynine
        , thirty
        ]


get : Int -> Level
get number =
    all
        |> List.drop (number - 1)
        |> List.head
        |> Maybe.withDefault (parseLevelString one)


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

        'y' ->
            Marble Yellow

        'p' ->
            Marble Purple

        'R' ->
            Goal Red

        'G' ->
            Goal Green

        'B' ->
            Goal Blue

        'Y' ->
            Goal Yellow

        'P' ->
            Goal Purple

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
