module Turnabout.Level.Model
    exposing
        ( Level
        , Movable(..)
        , MovableId(..)
        , BlockId(..)
        , empty
        , applyMoves
        , getBlock
        , insertBlock
        , positionOf
        , blockAt
        , withMarble
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Turnabout.Direction as Direction exposing (Cardinal(..))
import Turnabout.Coordinate as Coordinate exposing (Coordinate)
import Turnabout.Marble exposing (Marble)
import Turnabout.Moves exposing (Rotation(Clockwise, CounterClockwise), Moves)
import Turnabout.Block as Block exposing (Block)
import Turnabout.Color exposing (Color)
import Turnabout.Board as Board exposing (Board)


type BlockId
    = BlockId Int


type Movable
    = Murble Marble Coordinate
    | Block BlockId (List Coordinate)
    | Goal Color Coordinate


type alias Id =
    Int


type alias Size =
    { width : Int, height : Int }


type MovableId
    = MovableId Int


type alias Level =
    { board : Board
    , movables : List Movable
    , marbles : Dict Id Marble
    , blocks : Dict Id Block
    , positions : Dict Id Coordinate
    }


empty : Level
empty =
    { board = Board.empty
    , movables = []
    , marbles = Dict.empty
    , blocks = Dict.empty
    , positions = Dict.empty
    }


getBlock : MovableId -> Level -> Maybe Block
getBlock (MovableId id) level =
    Dict.get id level.blocks


nextId : Level -> Id
nextId level =
    level
        |> .positions
        |> Dict.keys
        |> List.maximum
        |> Maybe.withDefault 9
        |> (+) 1


{-| Inserts a marble into the Level, inferrring the id
-}
withMarble : Marble -> Coordinate -> Level -> Level
withMarble marble position level =
    insertMarble (nextId level) marble position level


{-| Inserts a marble into the Level, with an explicit id
-}
insertMarble : Id -> Marble -> Coordinate -> Level -> Level
insertMarble id marble position level =
    { level
        | marbles = Dict.insert id marble level.marbles
        , positions = Dict.insert id position level.positions
    }


{-| If we know a block's id already (like when we're parsing a level string) we
either want to create an instance and set ts position, or if it's already part
of some existing block group, we want to grow that block group with the new
position.
-}
insertBlock : MovableId -> Coordinate -> Level -> Level
insertBlock (MovableId id) position level =
    case Dict.get id level.blocks of
        Just block ->
            growExistingBlock id block position level

        Nothing ->
            insertNewBlock id position level


{-| When a block has an ID, but no instance represented in the level yet, we
make an instance and set its location.
-}
insertNewBlock : Int -> Coordinate -> Level -> Level
insertNewBlock id position level =
    { level
        | blocks = Dict.insert id Block.singleton level.blocks
        , positions = Dict.insert id position level.positions
    }


{-| When we have an existing block, and we want it to take up more space, we
figure out the position of the new block part relative to the top-left corner
of the existing block, and update the block with the new part. Since the block
is already positioned, we only care about the block itself.
-}
growExistingBlock : Int -> Block -> Coordinate -> Level -> Level
growExistingBlock id block position level =
    case positionOf (MovableId id) level of
        Ok offset ->
            let
                normalizedPosition =
                    Coordinate.subtract position offset

                newBlock =
                    Block.withPart normalizedPosition block
            in
                { level | blocks = Dict.insert id newBlock level.blocks }

        Err message ->
            Debug.crash message


positionOf : MovableId -> Level -> Result String Coordinate
positionOf (MovableId id) level =
    Dict.get id level.positions
        |> Result.fromMaybe ("Position for movable with id " ++ (toString id) ++ "doesn't exist")


applyMoves : Moves -> Level -> Level
applyMoves moves level =
    applyMovesHelp moves level South


applyMovesHelp : Moves -> Level -> Direction.Cardinal -> Level
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


shiftMovables : Direction.Cardinal -> Level -> Level
shiftMovables direction level =
    { level | movables = List.map (moveUntilBlocked direction level) level.movables }


moveUntilBlocked : Direction.Cardinal -> Level -> Movable -> Movable
moveUntilBlocked direction level movable =
    if isBlocked direction level movable then
        movable
    else
        case movable of
            Murble marble coordinate ->
                moveUntilBlocked
                    direction
                    level
                    (Murble marble (Coordinate.byOne direction coordinate))

            Goal _ coordinate ->
                movable

            Block _ coordinate ->
                movable


{-| Given a level and a movable, check to see if that movable has settled into
a position where it can move no longer.
-}
isBlocked : Direction.Cardinal -> Level -> Movable -> Bool
isBlocked direction level movable =
    case movable of
        Murble _ coordinate ->
            let
                nextSpace =
                    Coordinate.byOne direction coordinate
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
        Murble _ coordinate ->
            coordinate == candidate

        Goal _ coordinate ->
            coordinate == candidate

        Block _ coordinates ->
            List.any (flip (==) candidate) coordinates


nextGravity : Rotation -> Direction.Cardinal -> Direction.Cardinal
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
                Ok rootPosition ->
                    let
                        blockParts =
                            List.map (Coordinate.add rootPosition) parts
                    in
                        coordinates
                            |> Set.union (Set.fromList blockParts)
                            |> Set.insert rootPosition

                Err message ->
                    Debug.crash message
    in
        level.blocks
            |> Dict.foldl blockAtHelp Set.empty
            |> Set.member position
