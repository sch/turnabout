module Turnabout exposing (initialState, view, update, subscriptions)

import Levels
import View exposing (boardView)
import Keyboard
import Animation exposing (Angle, deg)


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
    | Animate Animation.Msg
    | NoOp


type Key
    = LeftArrow
    | RightArrow
    | Other


type alias Model =
    { gravity : Cardinality
    , currentLevel : Int
    , moves : List Rotation
    , style : Animation.State
    }


initialState : Model
initialState =
    { gravity = South
    , currentLevel = 5
    , moves = []
    , style =
        Animation.styleWith
            (Animation.spring { stiffness = 250, damping = 23 })
            [ Animation.rotate (deg 0) ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate rotation ->
            let
                moves =
                    rotation :: model.moves

                degrees =
                    reduceMoves moves |> toFloat |> deg

                animationSteps =
                    Animation.to [ Animation.rotate degrees ]

                style =
                    Animation.queue [ animationSteps ] model.style
            in
                ( { model | moves = moves, style = style }, Cmd.none )

        Animate amount ->
            ( { model | style = Animation.update amount model.style }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs rotationsFromCode
        , Animation.subscription Animate [ model.style ]
        ]


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
    boardView (Levels.get model.currentLevel) model.style


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
