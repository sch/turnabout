module Turnabout exposing (main, Model)

import Html exposing (Html)
import Levels
import View exposing (boardView)
import Keyboard


main =
    Html.program
        { init = ( levelStart, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Rotation
    = Clockwise
    | CounterClockwise


type Cardinality
    = North
    | South
    | East
    | West


type alias Model =
    { gravity : Cardinality
    , currentLevel : Int
    , moves : List Rotation
    }


levelStart : Model
levelStart =
    { gravity = South
    , currentLevel = 5
    , moves = []
    }


type Msg
    = Rotate Rotation
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate rotation ->
            ( { model | moves = rotation :: model.moves }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs rotationsFromCode


type Key
    = LeftArrow
    | RightArrow
    | Other


rotationsFromCode : Int -> Msg
rotationsFromCode keyCode =
    case keyCode of
        37 ->
            Rotate CounterClockwise

        39 ->
            Rotate Clockwise

        _ ->
            NoOp


view model =
    let
        level =
            Levels.get model.currentLevel

        rotation =
            reduceMoves model.moves
    in
        boardView level rotation


rotationInDegrees : Rotation -> Int
rotationInDegrees rotation =
    case rotation of
        Clockwise ->
            90

        CounterClockwise ->
            -90


reduceMoves : List Rotation -> Int
reduceMoves moves =
    List.foldl (+) 0 (List.map rotationInDegrees moves)
