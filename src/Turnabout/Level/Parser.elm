module Turnabout.Level.Parser exposing (parse, empty)

import Dict exposing (Dict)
import Turnabout.Level.Types exposing (..)


empty : Level
empty =
    { board = Dict.empty
    , movables = []
    , size = Size 0 0
    }


parse : String -> Level
parse levelString =
    parseHelp ( empty, (String.toList levelString), ( 0, 0 ) )


parseHelp : ( Level, List Char, Coordinate ) -> Level
parseHelp construct =
    case construct of
        -- we've seen all characters! no more work to do
        ( level, [], _ ) ->
            level

        -- eat up beginning newlines
        ( level, '\n' :: rest, ( 0, 0 ) ) ->
            parseHelp ( level, rest, ( 0, 0 ) )

        -- we've reached the end of a line, advance the index
        ( level, '\n' :: rest, ( _, row ) ) ->
            parseHelp ( level, rest, ( 0, row + 1 ) )

        -- parse a tile into a new level state
        ( level, char :: rest, index ) ->
            let
                ( column, row ) =
                    index

                nextIndex =
                    ( column + 1, row )

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

                        '1' ->
                            level |> addBlock 1 index

                        '2' ->
                            level |> addBlock 2 index

                        '3' ->
                            level |> addBlock 3 index

                        '4' ->
                            level |> addBlock 4 index

                        '5' ->
                            level |> addBlock 5 index

                        '6' ->
                            level |> addBlock 6 index

                        '7' ->
                            level |> addBlock 7 index

                        '8' ->
                            level |> addBlock 8 index

                        '9' ->
                            level |> addBlock 9 index

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
                parseHelp ( newLevel, rest, nextIndex )


markFloor : Coordinate -> Board -> Board
markFloor index board =
    Dict.insert index Floor board


markWall : Coordinate -> Board -> Board
markWall index board =
    Dict.insert index Wall board


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


addBlock : Int -> Coordinate -> Level -> Level
addBlock _ index level =
    Level
        (markFloor index level.board)
        level.movables
        (newSize level.size index)
