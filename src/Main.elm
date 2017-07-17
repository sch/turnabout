module Main exposing (main)

import Turnabout exposing (..)
import Html


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialState, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
