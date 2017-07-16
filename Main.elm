module Main exposing (main)

import Turnabout exposing (..)
import Html


main =
    Html.program
        { init = ( initialState, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
