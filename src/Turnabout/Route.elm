module Turnabout.Route exposing (Route(..), parse)

import Navigation
import UrlParser as Url exposing ((</>))


type Route
    = LevelSelect
    | Level Int


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map LevelSelect Url.top
        , Url.map Level (Url.s "level" </> Url.int)
        ]


parse : Navigation.Location -> Route
parse location =
    Url.parsePath route location |> Maybe.withDefault LevelSelect
