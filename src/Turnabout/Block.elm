module Turnabout.Block
    exposing
        ( Block(..)
        , singleton
        , withPart
        , width
        , height
        , view
        )

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Color
import Color.Convert exposing (colorToHex)


type Block
    = Block (List Part)


type alias Part =
    ( Int, Int )


singleton : Block
singleton =
    Block []


withPart : Part -> Block -> Block
withPart part (Block parts) =
    Block (part :: parts)


width : Block -> Int
width (Block parts) =
    parts |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0 |> (+) 1


height : Block -> Int
height (Block parts) =
    parts |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0 |> (+) 1


view : Int -> ( Block, ( Int, Int ) ) -> Svg msg
view size ( block, ( x, y ) ) =
    Svg.rect
        [ Attributes.fill (colorToHex Color.brown)
        , Attributes.x (toString (x * size + 1))
        , Attributes.y (toString (y * size + 1))
        , Attributes.width (toString ((width block * size) - 2))
        , Attributes.height (toString ((height block * size) - 2))
        ]
        []
