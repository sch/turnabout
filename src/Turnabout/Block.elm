module Turnabout.Block
    exposing
        ( Block
        , singleton
        , withPart
        , width
        , height
        , view
        )

import Svg exposing (Svg)


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


view : Block -> Svg msg
view block =
    Svg.text "block"
