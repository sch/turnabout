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
import Turnabout.Coordinate as Coordinate exposing (Coordinate)
import Turnabout.Direction exposing (Direction(..))


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


type Cursor
    = Cursor Direction Coordinate


{-| This one is a doozy. This function will return a string of points for an Svg
polygon element. To do so, we get a bit turtle-graphics-y and walk the
perimeter, obeying a right-hand rule. It walks one unit at a time, incrementing
the distance by the size parameter. When it reaches "home" (the top-left block,
we return the beginning position. This means the algorithm builds the svg points
in reverse.)
-}
perimeterPoints : Int -> Block -> String
perimeterPoints size (Block parts) =
    let
        blockToThe : Direction -> Coordinate -> Bool
        blockToThe direction currentLocation =
            List.member
                (Coordinate.byOne direction currentLocation)
                (( 0, 0 ) :: parts)

        walkPerimeter : Cursor -> List Coordinate
        walkPerimeter cursor =
            case cursor of
                Cursor West ( 1, 0 ) ->
                    List.singleton ( 1, 1 )

                Cursor North ( 0, 1 ) ->
                    List.singleton ( 1, 1 )

                Cursor South ( x, y ) ->
                    if blockToThe East ( x, y ) then
                        ( x * size + 1, y * size + 1 ) :: walkPerimeter (Cursor East ( x + 1, y ))
                    else if blockToThe West ( x, y ) then
                        ( x * size + 1, y * size + 1 ) :: walkPerimeter (Cursor West ( x - 1, y ))
                    else
                        walkPerimeter (Cursor South ( x, y + size + 1 ))

                Cursor _ _ ->
                    Debug.crash "Not taking into account all cases"
    in
        walkPerimeter (Cursor South ( 1, 1 ))
            |> List.map (\( x, y ) -> (toString x) ++ "," ++ (toString y))
            |> String.join " "


translateString : number -> number -> String
translateString x y =
    "translate(" ++ (toString x) ++ " " ++ (toString y) ++ ")"
