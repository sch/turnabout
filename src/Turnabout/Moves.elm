module Turnabout.Moves
    exposing
        ( Moves
        , Rotation(..)
        , initial
        , rotate
        , rotateClockwise
        , rotateCounterClockwise
        , undo
        , toDegrees
        , toDirection
        )

import Turnabout.Direction exposing (Direction(..))


type Rotation
    = Clockwise
    | CounterClockwise


type alias Moves =
    List Rotation


initial : Moves
initial =
    []


rotate : Rotation -> Moves -> Moves
rotate direction moves =
    moves ++ [ direction ]


rotateClockwise : Moves -> Moves
rotateClockwise =
    rotate Clockwise


rotateCounterClockwise : Moves -> Moves
rotateCounterClockwise =
    rotate CounterClockwise


undo : Moves -> Moves
undo moves =
    moves |> List.reverse |> List.drop 1 |> List.reverse


toDegrees : Moves -> Int
toDegrees moves =
    moves |> List.map rotationInDegrees |> List.sum


rotationInDegrees : Rotation -> Int
rotationInDegrees rotation =
    case rotation of
        Clockwise ->
            90

        CounterClockwise ->
            -90


toDirection : Moves -> Direction
toDirection moves =
    case toDegrees moves % 360 of
        (-90) ->
            West

        (-180) ->
            North

        (-270) ->
            East

        0 ->
            South

        90 ->
            West

        180 ->
            North

        270 ->
            East

        _ ->
            Debug.crash "There are only four directions."
