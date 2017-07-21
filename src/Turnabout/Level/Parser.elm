module Turnabout.Level.Parser
    exposing
        ( Tile(..)
        , Color(..)
        , Movable(..)
        , BlockId(..)
        , Level
        , Size
        , parse
        )

-- import Parser exposing (Parser, (|=), succeed)

import Dict exposing (Dict)


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | Purple


{-| Tiles are immutable board pieces --- either a wall or a floor piece. A tile
can have a Movable on top of it if it's a Floor, but a movable can't pass
through a Wall.
-}
type Tile
    = Wall
    | Floor


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


type alias Board =
    Dict Coordinate Tile


type alias Level =
    { board : Board
    , movables : List Movable
    , size : Size
    }


empty : Level
empty =
    { board = Dict.empty
    , movables = []
    , size = Size 0 0
    }



-- levelParser : Parser LayeredLevel
-- levelParser =
--     succeed LayeredLevel
--         |= " "


parse : String -> Level
parse levelString =
    let
        ( level, _, _ ) =
            parseHelp ( empty, (String.toList levelString), ( 0, 0 ) )
    in
        level


parseHelp : ( Level, List Char, Coordinate ) -> ( Level, List Char, Coordinate )
parseHelp construct =
    case construct of
        -- we've seen all characters! no more work to do
        ( level, [], index ) ->
            ( level, [], index )

        -- eat up beginning newlines
        ( level, '\n' :: rest, ( 0, 0 ) ) ->
            parseHelp ( level, rest, ( 0, 0 ) )

        -- we've reached the end of a line, advance the index
        ( level, '\n' :: rest, ( _, row ) ) ->
            parseHelp ( level, rest, ( 0, row + 1 ) )

        ( level, 'r' :: rest, index ) ->
            parseHelp ( (level |> addMarble Red index), rest, (nextIndex index 'r') )

        ( level, 'g' :: rest, index ) ->
            parseHelp ( (level |> addMarble Green index), rest, (nextIndex index 'g') )

        ( level, 'b' :: rest, index ) ->
            parseHelp ( (level |> addMarble Blue index), rest, (nextIndex index 'b') )

        ( level, 'R' :: rest, index ) ->
            parseHelp ( (level |> addGoal Red index), rest, (nextIndex index 'R') )

        ( level, 'G' :: rest, index ) ->
            parseHelp ( (level |> addGoal Green index), rest, (nextIndex index 'G') )

        ( level, 'B' :: rest, index ) ->
            parseHelp ( (level |> addGoal Blue index), rest, (nextIndex index 'B') )

        ( level, '#' :: rest, index ) ->
            let
                newLevel =
                    Level
                        (markWall index level.board)
                        level.movables
                        (newSize level.size index)
            in
                parseHelp ( newLevel, rest, (nextIndex index '#') )

        ( level, '.' :: rest, index ) ->
            let
                newLevel =
                    Level
                        (markFloor index level.board)
                        level.movables
                        (newSize level.size index)
            in
                parseHelp ( newLevel, rest, (nextIndex index '.') )

        ( level, _ :: rest, index ) ->
            parseHelp ( level, rest, (nextIndex index '#') )


markFloor : Coordinate -> Board -> Board
markFloor index board =
    Dict.insert index Floor board


markWall : Coordinate -> Board -> Board
markWall index board =
    Dict.insert index Wall board


{-| increment the index to point to the coordinate of the next parseable
character.
-}
nextIndex : Coordinate -> Char -> Coordinate
nextIndex ( column, row ) token =
    if token == '\n' then
        ( 0, row + 1 )
    else
        ( column + 1, row )


{-| figure out the new size of the board based on the current size and the
coordinate of the currently parsed token.
-}
newSize : Size -> Coordinate -> Size
newSize { width, height } ( columnIndex, rowIndex ) =
    Size (max (columnIndex + 1) width) (max (rowIndex + 1) height)

addMarble : Color -> Coordinate -> Level -> Level
addMarble color index level =
    Level
        (markFloor index level.board)
        (Marble color index :: level.movables)
        (newSize level.size index)


addGoal : Color -> Coordinate -> Level -> Level
addGoal color index level =
    Level
        (markFloor index level.board)
        (Goal color index :: level.movables)
        (newSize level.size index)
