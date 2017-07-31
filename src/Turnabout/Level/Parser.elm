module Turnabout.Level.Parser exposing (parse)

import Turnabout.Board as Board exposing (Board)
import Turnabout.Coordinate exposing (Coordinate)
import Turnabout.Color as Color exposing (Color)
import Turnabout.Marble as Marble exposing (Marble)
import Turnabout.Level.Model as Level
    exposing
        ( Level
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
                            level |> withMarble Marble.red index

                        'g' ->
                            level |> withMarble Marble.green index

                        'b' ->
                            level |> withMarble Marble.blue index

                        'y' ->
                            level |> withMarble Marble.yellow index

                        'p' ->
                            level |> withMarble Marble.purple index

                        'R' ->
                            level |> withGoal Color.Red index

                        'G' ->
                            level |> withGoal Color.Green index

                        'B' ->
                            level |> withGoal Color.Blue index

                        'Y' ->
                            level |> withGoal Color.Yellow index

                        'P' ->
                            level |> withGoal Color.Purple index

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


withMarble : Marble -> Coordinate -> Level -> Level
withMarble marble index level =
    { level | movables = ((Murble marble index) :: level.movables) }
        |> Level.withMarble marble index
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
