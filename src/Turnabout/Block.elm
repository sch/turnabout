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
import Svg.Lazy exposing (lazy2)
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



-- VIEW


view : Int -> ( Block, ( Int, Int ) ) -> Svg msg
view size ( block, ( x, y ) ) =
    Svg.g
        [ Attributes.transform (translateString (x * size + 1) (y * size + 1)) ]
        [ lazy2 viewShape size block ]


viewShape : Int -> Block -> Svg msg
viewShape size block =
    Svg.rect
        [ Attributes.fill (colorToHex Color.brown)
        , Attributes.width (toString ((width block * size) - 2))
        , Attributes.height (toString ((height block * size) - 2))
        ]
        []


translateString x y =
    "translate(" ++ (toString x) ++ " " ++ (toString y) ++ ")"
