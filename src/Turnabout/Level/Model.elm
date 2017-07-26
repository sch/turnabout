module Turnabout.Level.Model
    exposing
        ( Level
        , Coordinate
        , Color(..)
        , Movable(..)
        , MovableId(..)
        , BlockId(..)
        , empty
        , applyMoves
        , getBlock
        , insertBlock
        , positionOf
        , blockAt
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Turnabout.Direction exposing (Direction(..))
import Turnabout.Moves exposing (Rotation(Clockwise, CounterClockwise), Moves)
import Turnabout.Block as Block exposing (Block)
import Turnabout.Board as Board exposing (Board)


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | Purple


type BlockId
    = BlockId Int


type Movable
    = Marble Color Coordinate
    | Block BlockId (List Coordinate)
    | Goal Color Coordinate


type Parseable
    = Tile
    | Movable
    | Empty


type alias Size =
    { width : Int, height : Int }


type alias Coordinate =
    ( Int, Int )


type MovableId
    = MovableId Int


type alias Level =
    { board : Board
    , movables : List Movable
    , blocks : Dict Int Block
    , positions : Dict Int Coordinate
    }


empty : Level
empty =
    { board = Board.empty
    , movables = []
    , blocks = Dict.empty
    , positions = Dict.empty
    }


getBlock : MovableId -> Level -> Maybe Block
getBlock (MovableId id) level =
    Dict.get id level.blocks


insertBlock : MovableId -> Coordinate -> Level -> Level
insertBlock (MovableId id) position level =
    case Dict.get id level.blocks of
        Just block ->
            let
                offset =
                    positionOf (MovableId id) level
                        |> Maybe.withDefault ( 0, 0 )

                normalizedPosition =
                    coordinateSubtract position offset

                newBlock =
                    Block.withPart normalizedPosition block
            in
                { level
                    | blocks = Dict.insert id newBlock level.blocks
                }

        Nothing ->
            { level
                | blocks = Dict.insert id Block.singleton level.blocks
                , positions = Dict.insert id position level.positions
            }


positionOf : MovableId -> Level -> Maybe Coordinate
positionOf (MovableId id) level =
    Dict.get id level.positions


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


blockAt : Coordinate -> Level -> Bool
blockAt position level =
    let
        blockAtHelp : Int -> Block -> Set Coordinate -> Set Coordinate
        blockAtHelp id (Block.Block parts) coordinates =
            case positionOf (MovableId id) level of
                Just rootPosition ->
                    let
                        blockParts =
                            List.map (coordinateAdd rootPosition) parts
                    in
                        coordinates
                            |> Set.union (Set.fromList blockParts)
                            |> Set.insert rootPosition

                Nothing ->
                    coordinates
    in
        level.blocks
            |> Dict.foldl blockAtHelp Set.empty
            |> Set.member position


coordinateAdd : Coordinate -> Coordinate -> Coordinate
coordinateAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


coordinateSubtract : Coordinate -> Coordinate -> Coordinate
coordinateSubtract ( x2, y2 ) ( x1, y1 ) =
    ( x2 - x1, y2 - y1 )
