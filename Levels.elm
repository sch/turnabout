module Levels exposing (exampleLevel, parseLevelString, Tile(..), Level)


type MarbleColor
    = Red
    | Green
    | Blue


type Tile
    = Wall
    | Block
    | Marble MarbleColor
    | Empty


type Level
    = List List Tile


exampleLevel =
    parseLevelString exampleLevelString


exampleLevelString =
    """
####
#  #
# r#
####
"""


parseLevelString string =
    string |> String.trim |> String.lines |> List.map parseLevelRow


parseLevelRow row =
    row |> String.toList |> (List.map parseLevelChar) |> List.indexedMap (,)


parseLevelChar char =
    case char of
        '#' ->
            Wall

        'r' ->
            Marble Red

        _ ->
            Empty
