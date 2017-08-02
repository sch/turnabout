module Turnabout exposing (init, view, update, subscriptions, changeUrl)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Navigation
import Turnabout.Playfield as Playfield
import Turnabout.Level as Level exposing (Level)
import Turnabout.Route as Route exposing (Route)


type Key
    = LeftArrow
    | RightArrow
    | Other


type Page
    = LevelSelect
    | Playfield Playfield.State
    | Error


type alias Model =
    { history : List Route
    , page : Page
    }


type Msg
    = PlayfieldMessage Playfield.Msg
    | SelectLevel Int
    | ViewLevelSelect
    | UrlChange Navigation.Location


changeUrl : Navigation.Location -> Msg
changeUrl location =
    UrlChange location


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Route.parse location

        history =
            [ route ]

        page =
            pageState route
    in
        ( { history = history, page = page }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectLevel levelNumber ->
            let
                command =
                    Navigation.newUrl ("/level/" ++ (toString levelNumber))
            in
                ( model, command )

        ViewLevelSelect ->
            let
                command =
                    Navigation.newUrl "/"
            in
                ( model, command )

        PlayfieldMessage Playfield.Back ->
            ( model, Navigation.newUrl "/" )

        PlayfieldMessage playfieldMsg ->
            case model.page of
                Playfield playfield ->
                    let
                        ( playfieldState, command ) =
                            Playfield.update playfieldMsg playfield
                    in
                        ( { model | page = Playfield playfieldState }, Cmd.map PlayfieldMessage command )

                _ ->
                    ( model, Cmd.none )

        UrlChange location ->
            let
                route =
                    Route.parse location

                history =
                    route :: model.history

                page =
                    pageState route
            in
                ( { model | history = history, page = page }, Cmd.none )


pageState : Route -> Page
pageState route =
    case route of
        Route.LevelSelect ->
            LevelSelect

        Route.Level number ->
            case Level.get number of
                Just level ->
                    Playfield (Tuple.first (Playfield.init level))

                Nothing ->
                    Error


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Playfield playfield ->
            Playfield.subscriptions playfield |> Sub.map PlayfieldMessage

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    case model.page of
        LevelSelect ->
            levelSelectView model

        Playfield playfield ->
            Playfield.view playfield
                |> Html.map PlayfieldMessage

        Error ->
            loadingView


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
