module Turnabout.Level.Parser exposing (parse)

import Turnabout.Board as Board exposing (Board)
import Turnabout.Level.Model as Level
    exposing
        ( Level
        , Coordinate
        , Color(..)
        , Movable(..)
        , MovableId(..)
        )


parse : String -> Level
parse levelString =
    parseHelp ( Level.empty, (String.toList levelString), ( 0, 0 ) )


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
                            level |> withMarble Red index

                        'g' ->
                            level |> withMarble Green index

                        'b' ->
                            level |> withMarble Blue index

                        'y' ->
                            level |> withMarble Yellow index

                        'p' ->
                            level |> withMarble Purple index

                        'R' ->
                            level |> withGoal Red index

                        'G' ->
                            level |> withGoal Green index

                        'B' ->
                            level |> withGoal Blue index

                        'Y' ->
                            level |> withGoal Yellow index

                        'P' ->
                            level |> withGoal Purple index

                        '1' ->
                            level |> withBlock 1 index

                        '2' ->
                            level |> withBlock 2 index

                        '3' ->
                            level |> withBlock 3 index

                        '4' ->
                            level |> withBlock 4 index

                        '5' ->
                            level |> withBlock 5 index

                        '6' ->
                            level |> withBlock 6 index

                        '7' ->
                            level |> withBlock 7 index

                        '8' ->
                            level |> withBlock 8 index

                        '9' ->
                            level |> withBlock 9 index

                        '#' ->
                            level |> withWall index

                        '.' ->
                            level |> withFloor index

                        _ ->
                            level
            in
                parseHelp ( newLevel, rest, nextIndex )


withMarble : Color -> Coordinate -> Level -> Level
withMarble color index level =
    { level | movables = (Marble color index :: level.movables) }
        |> withFloor index


withGoal : Color -> Coordinate -> Level -> Level
withGoal color index level =
    { level | movables = (Goal color index :: level.movables) }
        |> withFloor index


withBlock : Int -> Coordinate -> Level -> Level
withBlock blockId index level =
    level
        |> Level.insertBlock (MovableId blockId) index
        |> withFloor index


withFloor : Coordinate -> Level -> Level
withFloor index level =
    { level | board = Board.insertFloor index level.board }


withWall : Coordinate -> Level -> Level
withWall index level =
    { level | board = Board.insertWall index level.board }
