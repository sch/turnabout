module Turnabout where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Color exposing (..)

reddish = rgb 208 73 66
reddishString = "rgb(208, 73, 66)"
blue = "rgb(32, 22, 129)"
size = 20

type MarbleColor = Red | Green | Blue
type Cardinality = North | East | South | West
type Orientation = Top | Right | Bottom | Left
type Tile = Wall | Block | Marble MarbleColor | Empty
type Rotation = Clockwise | CounterClockwise

type Level = List List Tile

type alias Model =
  { gravity : Cardinality
  , board : Level
  , moves : List Rotation
  }

exampleLevel =
  [ [ Wall,  Wall,       Wall,  Wall]
  , [ Wall, Empty,      Empty,  Wall]
  , [ Wall, Empty, Marble Red,  Wall]
  , [ Wall,  Wall,       Wall,  Wall]
  ]

levelStart =
  { gravity = South
  , board = exampleLevel
  , moves = []
  }

type Transform = Rotation Int | Translate Int Int

rotationInDegrees : Rotation -> Int
rotationInDegrees rotation =
  case rotation of
    Clockwise -> 90
    CounterClockwise -> -90
    otherwise -> 0

reduceMoves : List Rotation -> Int
reduceMoves moves = List.foldl (+) 0 (List.map rotationInDegrees moves)


transformString : Transform -> String
transformString trans =
  case trans of
    Rotation degrees -> "rotate(" ++ (toString degrees) ++ ")"

someMoves = [CounterClockwise, CounterClockwise, Clockwise]
someRotation = reduceMoves someMoves

main : Html
main =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 300 400"]
      [
        g [ transform ("translate(100,100) " ++ (transformString (Rotation someRotation))) ]
          [ svgWall (1, 3)
          , svgWall (2, 3)
          , svgWall (0, 3)
          , svgWall (0, 2)
          , svgWall (0, 1)
          , svgWall (0, 0)
          , svgWall (1, 0)
          , svgWall (2, 0)
          , svgWall (3, 0)
          , svgWall (4, 0)
          , svgWall (4, 1)
          , svgWall (4, 2)
          , svgWall (4, 3)
          , svgWall (2, 2)
          , svgWall (3, 3)
          , svgMarble (3, 2)
          ]
      ]

svgWall : (Int, Int) -> Svg
svgWall (gridX, gridY) =
  rect
  [ fill reddishString
  , x (toString (gridX * size))
  , y (toString (gridY * size))
  , width (toString size)
  , height (toString size)
  ] []

svgMarble : (Int, Int) -> Svg
svgMarble (x, y) =
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
