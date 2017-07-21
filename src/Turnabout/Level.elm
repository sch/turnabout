module Turnabout.Level
    exposing
        ( Cardinality(..)
        , get
        , all
        , toCoordinateDict
        , determineCardinality
        , applyMoves
        )

import Turnabout.Types exposing (Rotation(Clockwise, CounterClockwise), Moves)
import Turnabout.Level.Parser as Parser
import Turnabout.Level.String as LevelStrings
import Turnabout.Level.Types exposing (..)
import Dict exposing (Dict)


type Cardinality
    = North
    | South
    | East
    | West


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


all : List Level
all =
    List.map Parser.parse LevelStrings.all


get : Int -> Maybe Level
get number =
    all |> List.drop (number - 1) |> List.head


applyMoves : Moves -> Level -> Level
applyMoves moves level =
    case moves of
        [] ->
            level

        move :: rest ->
            level


shiftSouth : Level -> Level
shiftSouth level =
    if settled level then
        level
    else
        shiftSouth { level | movables = shiftMovables level.movables }


settled : Level -> Bool
settled level =
    True


shiftMovables : List Movable -> List Movable
shiftMovables movables =
    movables


determineCardinality : Moves -> Cardinality
determineCardinality moves =
    moves |> List.map toInt |> List.sum |> toCardinality


toInt : Rotation -> Int
toInt rotation =
    case rotation of
        Clockwise ->
            1

        CounterClockwise ->
            -1


toCardinality : Int -> Cardinality
toCardinality modulo =
    case modulo of
        (-3) ->
            West

        (-2) ->
            North

        (-1) ->
            East

        0 ->
            South

        1 ->
            West

        2 ->
            North

        3 ->
            East

        _ ->
            Debug.crash "There are only four directions."
