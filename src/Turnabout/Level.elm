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
import Turnabout.Level.String as LevelStrings
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


all =
    List.map parseLevelString LevelStrings.all


get : Int -> Level
get number =
    all
        |> List.drop (number - 1)
        |> List.head
        |> Maybe.withDefault [ [] ]


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
