module Turnabout exposing (initialState, view, update, subscriptions)

import Levels
import Board
import Keyboard
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Animation exposing (Angle, deg)
import Octicons


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
    | Undo
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
    , currentLevel = 10
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

                style =
                    animateRotation moves model.style
            in
                ( { model | moves = moves, style = style }, Cmd.none )

        Undo ->
            let
                moves =
                    List.tail model.moves |> Maybe.withDefault []

                style =
                    animateRotation moves model.style
            in
                ( { model | moves = moves, style = style }, Cmd.none )

        Animate amount ->
            ( { model | style = Animation.update amount model.style }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


animateRotation : List Rotation -> Animation.State -> Animation.State
animateRotation moves style =
    let
        degrees =
            reduceMoves moves |> toFloat |> deg

        animationSteps =
            Animation.to [ Animation.rotate degrees ]
    in
        Animation.queue [ animationSteps ] style


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

        85 ->
            Undo

        _ ->
            NoOp


view : Model -> Html Msg
view model =
    Html.div [ Attributes.style [ ( "position", "relative" ) ] ]
        [ Board.view (Levels.get model.currentLevel) model.style
        , absolutelyPositioned [ ( "top", "10px" ), ( "right", "10px" ) ] (button Undo)
        , absolutelyPositioned [ ( "bottom", "10px" ), ( "left", "10px" ) ] (button (Rotate CounterClockwise))
        , absolutelyPositioned [ ( "bottom", "10px" ), ( "right", "10px" ) ] (button (Rotate Clockwise))
        ]


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


absolutelyPositioned : List ( String, String ) -> Html msg -> Html msg
absolutelyPositioned styles node =
    Html.div
        [ Attributes.style (List.append styles [ ( "position", "absolute" ) ])
        ]
        [ node ]


button : Msg -> Html Msg
button msg =
    let
        icon =
            case msg of
                Rotate CounterClockwise ->
                    Octicons.chevronLeft

                Rotate Clockwise ->
                    Octicons.chevronRight

                Undo ->
                    Octicons.issueReopened

                _ ->
                    Octicons.issueReopened

        iconOptions =
            Octicons.defaultOptions |> Octicons.size 30

        styles =
            [ ( "background-color", "rgba(255, 255, 255, 0.5)" )
            , ( "border", "none" )
            , ( "border-radius", "3px" )
            , ( "padding", "15px" )
            ]

        attributes =
            [ Attributes.style styles, Events.onClick msg ]
    in
        Html.button attributes [ icon iconOptions ]
