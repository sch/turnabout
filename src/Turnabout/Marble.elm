module Turnabout.Marble
    exposing
        ( Marble(..)
        , red
        , blue
        , yellow
        , green
        , purple
        , view
        )

import Color.Convert exposing (colorToHex)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Turnabout.Color exposing (Color(..))


type Marble
    = Marble Color


red : Marble
red =
    Marble Red


blue : Marble
blue =
    Marble Blue


yellow : Marble
yellow =
    Marble Yellow


green : Marble
green =
    Marble Green


purple : Marble
purple =
    Marble Purple


{-| The size (in magic svg units) of each block
-}
size : Int
size =
    20


view : Marble -> Svg msg
view (Marble color) =
    Svg.circle
        [ fill (color |> Turnabout.Color.toRgb |> colorToHex)
        , r (toString ((size - 1) // 2))
        , cx (toString (size // 2))
        , cy (toString (size // 2))
        ]
        []
