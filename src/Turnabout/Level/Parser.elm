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

        ( level, char :: rest, index ) ->
            let
                newLevel =
                    case char of
                        'r' ->
                            level |> addMarble Red index

                        'g' ->
                            level |> addMarble Green index

                        'b' ->
                            level |> addMarble Blue index

                        'y' ->
                            level |> addMarble Yellow index

                        'p' ->
                            level |> addMarble Purple index

                        'R' ->
                            level |> addGoal Red index

                        'G' ->
                            level |> addGoal Green index

                        'B' ->
                            level |> addGoal Blue index

                        'Y' ->
                            level |> addGoal Yellow index

                        'P' ->
                            level |> addGoal Purple index

                        '#' ->
                            Level
                                (markWall index level.board)
                                level.movables
                                (newSize level.size index)

                        '.' ->
                            Level
                                (markFloor index level.board)
                                level.movables
                                (newSize level.size index)

                        _ ->
                            level
            in
                parseHelp ( newLevel, rest, (nextIndex index char) )


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
