module Turnabout where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Color exposing (..)

reddish = rgb 208 73 66
reddishString = "rgb(208, 73, 66)"
blue = "rgb(32, 22, 129)"
size = 20

type Cardinality = North | East | South | West
type Orientation = Top | Right | Bottom | Left
type Tile = Wall | Block | Marble
type Rotation = Clockwise | CounterClockwise

type Level = List List Tile

type alias Model =
  { gravity : Cardinality
  , board : Level
  , moves : List Rotation
  }

-- type alias Marble =
--   { color : Color
--   }

main : Html
main =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 200 300" ]
    [ wall (1, 3)
    , wall (2, 3)
    , wall (0, 3)
    , wall (0, 2)
    , wall (0, 1)
    , wall (0, 0)
    , wall (1, 0)
    , wall (2, 0)
    , wall (3, 0)
    , wall (4, 0)
    , wall (4, 1)
    , wall (4, 2)
    , wall (4, 3)
    , wall (2, 2)
    , wall (3, 3)
    , marble (3, 2)
    ]

wall : (Int, Int) -> Svg
wall (gridX, gridY) =
  rect
  [ fill reddishString
  , x (toString (gridX * size))
  , y (toString (gridY * size))
  , width (toString size)
  , height (toString size)
  ] []

marble : (Int, Int) -> Svg
marble (x, y) =
  circle
    [ fill blue
    , cx (toString ((x * size) + 10))
    , cy (toString ((y * size) + 10))
    , r "10"
    ] []

-- toFill : Color -> String
-- toFill color =
--   case color of
--     RGBA r g b a -> "rgba(" ++ (toString r) ++ ", 1, 1, 1)"
