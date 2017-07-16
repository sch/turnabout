module Turnabout exposing (initialState, view, update, subscriptions)

import Levels
import View exposing (boardView)
import Keyboard


type Rotation
    = Clockwise
    | CounterClockwise


type BoardMove
    = ClockwiseFrom Cardinality
    | CounterClockwiseFrom Cardinality


type Cardinality
    = North
    | South
    | East
    | West


type Msg
    = Rotate Rotation
    | NoOp


type Key
    = LeftArrow
    | RightArrow
    | Other


type alias Model =
    { gravity : Cardinality
    , currentLevel : Int
    , moves : List Rotation
    }


initialState : Model
initialState =
    { gravity = South
    , currentLevel = 5
    , moves = []
    }


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

        previousRotation =
            reduceMoves (List.tail model.moves |> Maybe.withDefault [])
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
