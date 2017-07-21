module Turnabout.Level.Types exposing (..)

import Dict exposing (Dict)


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | Purple


{-| Tiles are immutable board pieces --- either a wall or a floor piece. A tile
can have a Movable on top of it if it's a Floor, but a movable can't pass
through a Wall.
-}
type Tile
    = Wall
    | Floor


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


type alias Board =
    Dict Coordinate Tile


type alias Level =
    { board : Board
    , movables : List Movable
    , size : Size
    }
