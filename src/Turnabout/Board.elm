module Turnabout.Board
    exposing
        ( Board(..)
        , Tile(..)
        , Size(..)
        , empty
        , insertWall
        , insertFloor
        , at
        , isWall
        , toList
        )

import Dict exposing (Dict)


{-| A board represents a playfield that game pieces like marbles, goals, and
blocks can exist on. The contents of a board do no change throughout the course
of a level. Boards have a fixed size.
-}
type Board
    = Board TileDict Size


type alias TileDict =
    Dict Position Tile


{-| The board consists of two kinds of tiles: Walls and Floors. Walls are
immovable elments of the playfield, and Floors are the areas fenced in by
a wall. Movables on the board can only be placed on floors.
-}
type Tile
    = Wall
    | Floor


{-| Every board has a size representing the width and height in terms of
blocks. Because the size of a board doesn't change over the course of a game,
we treat it as a piece of data for fast lookup.
-}
type Size
    = Size Width Height


type alias Width =
    Int


type alias Height =
    Int


type alias Position =
    ( Int, Int )


{-| A board without any tiles.
-}
empty : Board
empty =
    Board (Dict.empty) (Size 0 0)


{-| Returns the tile, if it exists, at the requested position.
-}
at : Position -> Board -> Maybe Tile
at position (Board lookup size) =
    Dict.get position lookup


{-| Will tell you if a wall is at a given coordinate
-}
isWall : Position -> Board -> Bool
isWall position board =
    board |> at position |> Maybe.map (flip (==) Wall) |> Maybe.withDefault False


{-| Inserts a wall tile into the board at the requested position.
-}
insertWall : Position -> Board -> Board
insertWall =
    insert Wall


{-| Inserts a floor tile into the board at the requested position.
-}
insertFloor : Position -> Board -> Board
insertFloor =
    insert Floor


{-| While inserting a tile into the dictionary, we figure out how large the
board is.
-}
insert : Tile -> Position -> Board -> Board
insert tile position (Board lookup size) =
    let
        newDict =
            Dict.insert position tile lookup

        (Size width height) =
            size

        ( x, y ) =
            position

        newSize =
            Size (max (x + 1) width) (max (y + 1) height)
    in
        Board newDict newSize


{-| Returns the board as a list of pairs of positions and tiles.
-}
toList : Board -> List ( Position, Tile )
toList (Board dict _) =
    Dict.toList dict


toString : Board -> String
toString board =
    """
#####
#...#
#####
"""
