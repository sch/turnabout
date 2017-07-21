module Turnabout.Level exposing (get, all, toCoordinateDict)

import Turnabout.Level.Parser as Parser
import Turnabout.Level.String as LevelStrings
import Turnabout.Level.Types exposing (..)
import Dict exposing (Dict)


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
