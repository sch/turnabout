module Turnabout.Block
    exposing
        ( Block(..)
        , singleton
        , withPart
        , view
        )

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Lazy exposing (lazy2)
import Color
import Color.Convert exposing (colorToHex)
import Turnabout.Coordinate exposing (Coordinate)


type Block
    = Block (List Coordinate)


singleton : Block
singleton =
    Block []


withPart : Coordinate -> Block -> Block
withPart part (Block parts) =
    Block (part :: parts)



-- VIEW


view : Int -> ( Block, ( Int, Int ) ) -> Svg msg
view size ( block, ( x, y ) ) =
    Svg.g
        [ Attributes.transform (translateString (x * size) (y * size)) ]
        [ lazy2 viewShape size block ]


viewShape : Int -> Block -> Svg msg
viewShape size block =
    Svg.polygon
        [ Attributes.fill (colorToHex Color.brown)
        , Attributes.points (perimeterPoints size block)
        ]
        []


perimeterPoints : Int -> Block -> String
perimeterPoints size block =
    perimeterPointsHelp size block [ ( 1, 1 ) ]
        |> List.map (\( x, y ) -> (toString x) ++ "," ++ (toString y))
        |> String.join " "


perimeterPointsHelp : Int -> Block -> List Coordinate -> List Coordinate
perimeterPointsHelp size (Block parts) points =
    case parts of
        [] ->
            [ ( 1, 1 )
            , ( size - 1, 1 )
            , ( size - 1, size - 1 )
            , ( 1, size - 1 )
            ]

        head :: rest ->
            perimeterPointsHelp size (Block rest) points


translateString : number -> number -> String
translateString x y =
    "translate(" ++ (toString x) ++ " " ++ (toString y) ++ ")"
