module Turnabout where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)

color = "rgb(208, 73, 66)"
blue = "rgb(32, 22, 129)"
size = 20

main : Html
main =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
    [ rect [ fill color, x "20", y "20", width "40", height "40"] []
    , rect [ fill color, x "80", y "40", width "40", height "40"] []
    , block "90" "90"
    , marble 100 100
    ]

block : String -> String -> Svg
block gridX gridY = rect [ fill color, x gridX, y gridY, width "20", height "20"] []

marble : Int -> Int -> Svg
marble x y = circle [ fill blue, cx (toString (x + 10)), cy (toString (y + 10)), r "10"] []

