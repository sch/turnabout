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
    applyMovesHelp (Debug.log "moves to apply" moves) level South


applyMovesHelp : Moves -> Level -> Cardinality -> Level
applyMovesHelp moves level gravity =
    case moves of
        [] ->
            level

        move :: rest ->
            let
                g =
                    Debug.log "next gravity" (nextGravity move gravity)
            in
                applyMovesHelp rest (shiftMovables g level) g


shiftMovables : Cardinality -> Level -> Level
shiftMovables direction level =
    { level | movables = List.map (moveUntilBlocked direction level) level.movables }


moveUntilBlocked : Cardinality -> Level -> Movable -> Movable
moveUntilBlocked direction level movable =
    if isBlocked direction level movable then
        movable
    else
        case movable of
            Marble color coordinate ->
                moveUntilBlocked
                    direction
                    level
                    (Marble color (moveOneUnit direction coordinate))

            Goal _ coordinate ->
                movable

            Block _ coordinate ->
                movable


{-| Given a level and a movable, check to see if that movable has settled into
a position where it can move no longer.
-}
isBlocked : Cardinality -> Level -> Movable -> Bool
isBlocked direction level movable =
    case movable of
        Marble color coordinate ->
            let
                nextSpace =
                    moveOneUnit direction coordinate
            in
                isWall level.board nextSpace
                    || List.any (occupying nextSpace) level.movables

        Goal color coordinate ->
            True

        Block _ _ ->
            True


{-| Checks to see if the given coordinate in a board is a wall or a floor
-}
isWall : Board -> Coordinate -> Bool
isWall board point =
    board
        |> Dict.get point
        |> Maybe.map (flip (==) Wall)
        |> Maybe.withDefault False


{-| Determines if there's a movable occupying the given point
-}
occupying : Coordinate -> Movable -> Bool
occupying candidate movable =
    case movable of
        Marble _ coordinate ->
            coordinate == candidate

        Goal _ coordinate ->
            coordinate == candidate

        Block _ coordinates ->
            List.any (flip (==) candidate) coordinates


{-| The coordinate, moved one unit in the given direction
-}
moveOneUnit : Cardinality -> Coordinate -> Coordinate
moveOneUnit direction ( x, y ) =
    case direction of
        South ->
            ( x, y + 1 )

        West ->
            ( x - 1, y )

        North ->
            ( x, y - 1 )

        East ->
            ( x + 1, y )


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


nextGravity : Rotation -> Cardinality -> Cardinality
nextGravity rotation gravityDirection =
    case gravityDirection of
        South ->
            if rotation == Clockwise then
                East
            else
                West

        West ->
            if rotation == Clockwise then
                South
            else
                North

        North ->
            if rotation == Clockwise then
                West
            else
                East

        East ->
            if rotation == Clockwise then
                North
            else
                South


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
