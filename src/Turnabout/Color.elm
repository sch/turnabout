module Turnabout.Color exposing (Color(..), toRgb)

import Color as Core exposing (rgb)


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | Purple


toRgb : Color -> Core.Color
toRgb color =
    case color of
        Red ->
            rgb 208 73 66

        Green ->
            rgb 164 255 237

        Blue ->
            rgb 55 144 242

        Yellow ->
            rgb 247 179 74

        Purple ->
            rgb 249 180 250
