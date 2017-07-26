module Turnabout.Level
    exposing
        ( Level
        , get
        , all
        , applyMoves
        , getBlock
        , insertBlock
        , positionOf
        , blockAt
        )

import Turnabout.Level.Model as Model
import Turnabout.Level.String as LevelStrings
import Turnabout.Level.Parser as Parser

type alias Level = Model.Level

all : List Level
all =
    List.map Parser.parse LevelStrings.all


get : Int -> Maybe Level
get number =
    all |> List.drop (number - 1) |> List.head


applyMoves =
    Model.applyMoves


getBlock =
    Model.getBlock


insertBlock : Int -> (Int, Int) -> Level -> Level
insertBlock id position level =
    Model.insertBlock (Model.MovableId id) position level


positionOf =
    Model.positionOf


blockAt =
    Model.blockAt
