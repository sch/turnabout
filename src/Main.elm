module Main exposing (main)

import Turnabout
import Navigation


main =
    Navigation.program Turnabout.changeUrl
        { init = Turnabout.initWithLocation
        , view = Turnabout.view
        , update = Turnabout.update
        , subscriptions = Turnabout.subscriptions
        }
