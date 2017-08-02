module Turnabout exposing (init, initWithLocation, view, update, subscriptions, changeUrl)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Keyboard
import Navigation
import Octicons
import Turnabout.Playfield as Playfield
import Turnabout.Level as Level exposing (Level)
import Turnabout.Level.Model as LevelModel
import Turnabout.Moves as Moves exposing (Moves, Rotation(..))
import UrlParser as Url exposing ((</>))


type Key
    = LeftArrow
    | RightArrow
    | Other


type alias Model =
    { moves : Moves
    , playfield : Playfield.State
    , history : List Route
    }


type Msg
    = Rotate Rotation
    | Undo
    | PlayfieldMessage Playfield.Msg
    | SelectLevel Int
    | ViewLevelSelect
    | UrlChange Navigation.Location
    | NoOp


changeUrl : Navigation.Location -> Msg
changeUrl location =
    UrlChange location


initialState : Model
initialState =
    { moves = Moves.initial
    , playfield = Playfield.initialState
    , history = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialState, Cmd.none )


initWithLocation : Navigation.Location -> ( Model, Cmd Msg )
initWithLocation location =
    ( { initialState | history = [ parseLocation location ] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate rotation ->
            case List.head model.history of
                Just route ->
                    case route of
                        Level number ->
                            let
                                moves =
                                    Moves.rotate rotation model.moves

                                ( playfield, cmd ) =
                                    case Level.get number of
                                        Just initialLevel ->
                                            let
                                                level =
                                                    Level.applyMoves moves initialLevel
                                            in
                                                Playfield.update (Playfield.rotate moves level) model.playfield

                                        Nothing ->
                                            ( model.playfield, Cmd.none )

                                command =
                                    Cmd.map PlayfieldMessage cmd
                            in
                                ( { model | moves = moves, playfield = playfield }, command )

                        LevelSelect ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Undo ->
            let
                moves =
                    Moves.undo model.moves

                ( playfield, cmd ) =
                    Playfield.update (Playfield.rotate moves LevelModel.empty) model.playfield

                command =
                    Cmd.map PlayfieldMessage cmd
            in
                ( { model | moves = moves, playfield = playfield }, command )

        SelectLevel levelNumber ->
            let
                ( playfield, cmd ) =
                    case (Level.get levelNumber) of
                        Just level ->
                            Playfield.update (Playfield.appear level) model.playfield

                        Nothing ->
                            ( model.playfield, Cmd.none )

                newModel =
                    { model | moves = Moves.initial, playfield = playfield }

                command =
                    Cmd.batch
                        [ Cmd.map PlayfieldMessage cmd
                        , Navigation.newUrl ("/level/" ++ (toString levelNumber))
                        ]
            in
                ( newModel, command )

        ViewLevelSelect ->
            let
                ( playfield, cmd ) =
                    Playfield.update Playfield.reset model.playfield

                command =
                    Cmd.batch
                        [ Cmd.map PlayfieldMessage cmd
                        , Navigation.newUrl "/"
                        ]
            in
                ( { model | playfield = playfield }, command )

        PlayfieldMessage playfieldMsg ->
            let
                ( playfield, command ) =
                    Playfield.update playfieldMsg model.playfield
            in
                ( { model | playfield = playfield }, Cmd.map PlayfieldMessage command )

        UrlChange location ->
            ( { model | history = (parseLocation location) :: model.history }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        playfieldSubscriptions =
            Playfield.subscriptions model.playfield |> Sub.map PlayfieldMessage
    in
        if Playfield.isAnimating model.playfield then
            playfieldSubscriptions
        else
            Sub.batch [ Keyboard.downs messagesFromCode, playfieldSubscriptions ]


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
    case model.history of
        [] ->
            loadingView

        latest :: _ ->
            case latest of
                Level levelNumber ->
                    case (Level.get levelNumber) of
                        Just initialLevel ->
                            let
                                level =
                                    Level.applyMoves model.moves initialLevel
                            in
                                levelView model.playfield level

                        Nothing ->
                            levelUnavailableView levelNumber

                LevelSelect ->
                    levelSelectView model


levelUnavailableView : Int -> Html Msg
levelUnavailableView number =
    Html.text ("There isn't a level " ++ (toString number))


loadingView : Html Msg
loadingView =
    Html.text "Loading..."


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


levelView : Playfield.State -> Level -> Html Msg
levelView playfieldState level =
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
            [ Playfield.view playfieldState level
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


type Route
    = LevelSelect
    | Level Int


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map LevelSelect Url.top
        , Url.map Level (Url.s "level" </> Url.int)
        ]


parseLocation : Navigation.Location -> Route
parseLocation location =
    Url.parsePath route location |> Maybe.withDefault LevelSelect
