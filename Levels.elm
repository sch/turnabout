module Levels exposing (Tile(..), Level, get, size, all)


type MarbleColor
    = Red
    | Green
    | Blue


type Tile
    = Wall
    | Block
    | Marble MarbleColor
    | Goal MarbleColor
    | Floor
    | Empty


type alias Level =
    List (List Tile)


type alias Size =
    { width : Int, height : Int }


size : Level -> Size
size level =
    { width = List.length (List.head level |> Maybe.withDefault [])
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


all =
    List.map parseLevelString [ one, two, three, four, five, six, seven, eight, nine, ten ]


get : Int -> Level
get number =
    all
        |> List.drop (number - 1)
        |> List.head
        |> Maybe.withDefault (parseLevelString one)


parseLevelString : String -> Level
parseLevelString string =
    string |> String.trim |> String.lines |> List.map parseLevelRow


parseLevelRow row =
    row |> String.toList |> (List.map parseLevelChar)


parseLevelChar : Char -> Tile
parseLevelChar char =
    case char of
        '#' ->
            Wall

        'r' ->
            Marble Red

        'R' ->
            Goal Red

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

        '.' ->
            Floor

        _ ->
            Empty
