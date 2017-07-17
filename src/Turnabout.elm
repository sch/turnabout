module Turnabout exposing (Model, Msg, initialState, view, update, subscriptions)

import Board
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Keyboard
import Levels
import Octicons
import Task
import Turnabout.Types exposing (Rotation(Clockwise, CounterClockwise), Moves)


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
    | BoardMessage Board.Msg
    | Undo
    | NoOp


type Key
    = LeftArrow
    | RightArrow
    | Other


type alias Model =
    { gravity : Cardinality
    , currentLevel : Int
    , moves : Moves
    , board : Board.State
    }


initialState : Model
initialState =
    { gravity = South
    , currentLevel = 10
    , moves = []
    , board = Board.initialState
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate rotation ->
            let
                moves =
                    rotation :: model.moves
            in
                ( { model | moves = moves }, rotateCommand moves )

        Undo ->
            let
                moves =
                    List.tail model.moves |> Maybe.withDefault []
            in
                ( { model | moves = moves }, rotateCommand moves )

        BoardMessage boardMsg ->
            let
                ( board, command ) =
                    Board.update boardMsg model.board
            in
                ( { model | board = board }, Cmd.map BoardMessage command )

        NoOp ->
            ( model, Cmd.none )


rotateCommand : Moves -> Cmd Msg
rotateCommand moves =
    Task.perform (BoardMessage << Board.rotate) (Task.succeed moves)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs messagesFromCode
        , Board.subscriptions model.board |> Sub.map BoardMessage
        ]


messagesFromCode : Int -> Msg
messagesFromCode keyCode =
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
        [ Board.view (Levels.get model.currentLevel) model.board
        , absolutelyPositioned [ ( "top", "10px" ), ( "right", "10px" ) ] (button Undo)
        , absolutelyPositioned [ ( "bottom", "10px" ), ( "left", "10px" ) ] (button (Rotate CounterClockwise))
        , absolutelyPositioned [ ( "bottom", "10px" ), ( "right", "10px" ) ] (button (Rotate Clockwise))
        ]


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
