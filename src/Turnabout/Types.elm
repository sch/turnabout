module Turnabout.Types exposing (..)


type Rotation
    = Clockwise
    | CounterClockwise


type alias Moves =
    List Rotation
