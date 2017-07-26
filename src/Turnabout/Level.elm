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
        , toBlockPairs
        )

import Turnabout.Extension exposing (zipDict)
import Turnabout.Block exposing (Block)
import Turnabout.Level.Model as Model
import Turnabout.Level.String as LevelStrings
import Turnabout.Level.Parser as Parser


type alias Level =
    Model.Level


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


insertBlock : Int -> ( Int, Int ) -> Level -> Level
insertBlock id position level =
    Model.insertBlock (Model.MovableId id) position level


positionOf : Int -> Level -> Maybe Model.Coordinate
positionOf id level =
    Model.positionOf (Model.MovableId id) level


blockAt =
    Model.blockAt


toBlockPairs : Level -> List ( Block, Model.Coordinate )
toBlockPairs level =
    level.blocks |> zipDict level.positions
