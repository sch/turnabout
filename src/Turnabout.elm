module Turnabout exposing (Model, Msg, initialState, view, update, subscriptions)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Keyboard
import Octicons
import Task
import Turnabout.Playfield as Playfield
import Turnabout.Level as Level
import Turnabout.Level.Types exposing (Level)
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
    | PlayfieldMessage Playfield.Msg
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
    , playfield : Playfield.State
    }


initialState : Model
initialState =
    { gravity = South
    , currentLevel = Nothing
    , moves = []
    , playfield = Playfield.initialState
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate rotation ->
            let
                moves =
                    rotation :: model.moves

                command =
                    send (PlayfieldMessage (Playfield.rotate moves))
            in
                ( { model | moves = moves }, command )

        Undo ->
            let
                moves =
                    List.tail model.moves |> Maybe.withDefault []

                command =
                    send (PlayfieldMessage (Playfield.rotate moves))
            in
                ( { model | moves = moves }, command )

        SelectLevel levelNumber ->
            let
                command =
                    send (PlayfieldMessage Playfield.appear)
            in
                ( { model | currentLevel = Just levelNumber, moves = [] }, command )

        ViewLevelSelect ->
            let
                command =
                    send (PlayfieldMessage Playfield.reset)
            in
                ( { model | currentLevel = Nothing }, command )

        PlayfieldMessage playfieldMsg ->
            let
                ( playfield, command ) =
                    Playfield.update playfieldMsg model.playfield
            in
                ( { model | playfield = playfield }, Cmd.map PlayfieldMessage command )

        NoOp ->
            ( model, Cmd.none )


send : Msg -> Cmd Msg
send msg =
    Task.perform (always msg) (Task.succeed identity)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs messagesFromCode
        , Playfield.subscriptions model.playfield |> Sub.map PlayfieldMessage
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
            case (Level.get levelNumber) of
                Just level ->
                    levelView level model.playfield

                Nothing ->
                    levelUnavailableView levelNumber

        Nothing ->
            levelSelectView model


levelUnavailableView : Int -> Html Msg
levelUnavailableView number =
    Html.text ("There isn't a level " ++ (toString number))


levelSelectView : Model -> Html Msg
levelSelectView model =
    let
        styles =
            [ ( "font-family", """
            -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial,
            sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"
            """ )
            , ( "color", "#555" )
            , ( "text-align", "center" )
            , ( "text-decoration", "none" )
            , ( "padding", "30px" )
            , ( "overflow", "auto" )
            ]

        levelCount =
            List.length Level.all

        li num =
            Html.li
                [ Events.onClick (SelectLevel num)
                , Attributes.style [ ( "padding", "10px" ), ( "cursor", "pointer" ) ]
                ]
                [ Html.text ("level " ++ (toString num)) ]
    in
        Html.div
            [ Attributes.style styles ]
            [ Html.ul
                [ Attributes.style [ ( "list-style", "none" ), ( "padding", "0" ) ] ]
                (List.range 1 levelCount |> List.map li)
            ]


levelView : Level -> Playfield.State -> Html Msg
levelView level playfield =
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
        Html.div [ Attributes.style [ ( "position", "relative" ), ( "height", "100%" ) ] ]
            [ Playfield.view level playfield
            , button ViewLevelSelect |> topLeft
            , button Undo |> topRight
            , button (Rotate CounterClockwise) |> bottomLeft
            , button (Rotate Clockwise) |> bottomRight
            ]


absolutelyPositioned : List ( String, String ) -> Html msg -> Html msg
absolutelyPositioned styles node =
    Html.div
        [ Attributes.style (List.append styles [ ( "position", "absolute" ) ]) ]
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
                    Debug.crash "You shouldn't be able to get here"

        iconOptions =
            Octicons.defaultOptions |> Octicons.size 24 |> Octicons.color "#555"

        styles =
            [ ( "background-color", "rgba(255, 255, 255, 0.5)" )
            , ( "border", "none" )
            , ( "border-radius", "3px" )
            , ( "padding", "10px 11px" )
            , ( "cursor", "pointer" )
            ]

        attributes =
            [ Attributes.style styles, Events.onClick msg ]
    in
        Html.button attributes [ icon iconOptions ]
