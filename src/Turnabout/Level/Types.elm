module Turnabout.Level.Types exposing (..)

import Dict exposing (Dict)
import Turnabout.Board as Board exposing (Board)


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | Purple


type BlockId
    = BlockId Int


type Movable
    = Marble Color Coordinate
    | Block BlockId (List Coordinate)
    | Goal Color Coordinate


type Parseable
    = Tile
    | Movable
    | Empty


type alias Size =
    { width : Int, height : Int }


type alias Coordinate =
    ( Int, Int )


type alias Level =
    { board : Board
    , movables : List Movable
    }
