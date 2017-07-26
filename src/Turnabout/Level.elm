module Turnabout.Level exposing (get, all, toCoordinateDict, applyMoves)

import Turnabout.Direction exposing (Direction(..))
import Turnabout.Moves exposing (Rotation(Clockwise, CounterClockwise), Moves)
import Turnabout.Board as Board exposing (Board)
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


applyMoves : Moves -> Level -> Level
applyMoves moves level =
    applyMovesHelp moves level South


applyMovesHelp : Moves -> Level -> Direction -> Level
applyMovesHelp moves level gravity =
    case moves of
        [] ->
            level

        move :: rest ->
            let
                g =
                    nextGravity move gravity
            in
                applyMovesHelp rest (shiftMovables g level) g


shiftMovables : Direction -> Level -> Level
shiftMovables direction level =
    { level | movables = List.map (moveUntilBlocked direction level) level.movables }


moveUntilBlocked : Direction -> Level -> Movable -> Movable
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
isBlocked : Direction -> Level -> Movable -> Bool
isBlocked direction level movable =
    case movable of
        Marble color coordinate ->
            let
                nextSpace =
                    moveOneUnit direction coordinate
            in
                (level.board |> Board.isWall nextSpace)
                    || List.any (occupying nextSpace) level.movables

        Goal color coordinate ->
            True

        Block _ _ ->
            True


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
moveOneUnit : Direction -> Coordinate -> Coordinate
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


nextGravity : Rotation -> Direction -> Direction
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
