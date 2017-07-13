module Levels exposing (exampleLevel, parseLevelString, size, Tile(..), Level)


type MarbleColor
    = Red
    | Green
    | Blue


type Tile
    = Wall
    | Block
    | Marble MarbleColor
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


exampleLevel =
    parseLevelString exampleLevelString


exampleLevelString =
    """
####
#  #
# r#
####
"""


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

        _ ->
            Empty
