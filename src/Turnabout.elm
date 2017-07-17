module Turnabout exposing (Model, Msg, initialState, view, update, subscriptions)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Keyboard
import Octicons
import Task
import Turnabout.Board as Board
import Turnabout.Levels as Levels
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
    | Undo
    | BoardMessage Board.Msg
    | SelectLevel Int
    | ViewLevelSelect
    | NoOp


type Key
    = LeftArrow
    | RightArrow
    | Other


type alias Model =
    { gravity : Cardinality
    , currentLevel : Maybe Int
    , moves : Moves
    , board : Board.State
    }


initialState : Model
initialState =
    { gravity = South
    , currentLevel = Nothing
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

        SelectLevel levelNumber ->
            ( { model | currentLevel = Just levelNumber }, Cmd.none )

        ViewLevelSelect ->
            ( { model | currentLevel = Nothing }, Cmd.none )

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
    case model.currentLevel of
        Just levelNumber ->
            levelView (Levels.get levelNumber) model.board

        Nothing ->
            levelSelectView model


levelSelectView : Model -> Html Msg
levelSelectView model =
    let
        styles =
            [ ( "font-family", """
            -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"
            """ )
            , ( "color", "#555" )
            , ( "text-align", "center" )
            , ( "text-decoration", "none" )
            , ( "padding", "30px" )
            ]

        levelCount =
            List.length Levels.all

        li num =
            Html.li
                [ Events.onClick (SelectLevel num)
                , Attributes.style [ ( "padding", "10px" ) ]
                ]
                [ Html.text ("level " ++ (toString num)) ]
    in
        Html.div
            [ Attributes.style styles ]
            [ Html.ul
                [ Attributes.style [ ( "list-style", "none" ) ] ]
                (List.range 1 levelCount |> List.map li)
            ]


levelView : Levels.Level -> Board.State -> Html Msg
levelView level board =
    let
        offset =
            "16px"

        topLeft =
            absolutelyPositioned [ ( "top", offset ), ( "left", offset ) ]

        topRight =
            absolutelyPositioned [ ( "top", offset ), ( "right", offset ) ]

        bottomLeft =
            absolutelyPositioned [ ( "bottom", offset ), ( "left", offset ) ]

        bottomRight =
            absolutelyPositioned [ ( "bottom", offset ), ( "right", offset ) ]
    in
        Html.div [ Attributes.style [ ( "position", "relative" ) ] ]
            [ Board.view level board
            , button ViewLevelSelect |> topLeft
            , button Undo |> topRight
            , button (Rotate CounterClockwise) |> bottomLeft
            , button (Rotate Clockwise) |> bottomRight
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

                ViewLevelSelect ->
                    Octicons.listUnordered

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
