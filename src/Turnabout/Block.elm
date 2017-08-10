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
import Turnabout.Coordinate as Coordinate exposing (Coordinate, byOne)
import Turnabout.Direction as Direction exposing (Cardinal(..))


type Block
    = Block (List Coordinate)


singleton : Block
singleton =
    Block []


withPart : Coordinate -> Block -> Block
withPart part (Block parts) =
    Block (part :: parts)


occupies : Coordinate -> Block -> Bool
occupies coordinate (Block parts) =
    List.member coordinate (( 0, 0 ) :: parts)



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


type alias Depth =
    Int


type Cursor
    = Cursor Direction.Cardinal Coordinate Depth


{-| This one is a doozy. This function will return a string of points for an Svg
polygon element. To do so, we get a bit turtle-graphics-y and walk the
perimeter, obeying a right-hand rule. It walks one unit at a time, incrementing
the distance by the size parameter. When it reaches "home" (the top-left block,
we return the beginning position. This means the algorithm builds the svg points
in reverse.)
-}
perimeterPoints : Int -> Block -> String
perimeterPoints size block =
    let
        walkPerimeter : Cursor -> List Coordinate
        walkPerimeter cursor =
            case cursor of
                Cursor _ _ 100 ->
                    Debug.log "Blew recursive drawing stack" []

                Cursor West ( 0, 0 ) _ ->
                    [ ( 1, 1 ) ]

                Cursor South ( 0, -1 ) _ ->
                    []

                Cursor South ( x, y ) depth ->
                    if block |> occupies (( x, y ) |> byOne South |> byOne West) then
                        ( x * size + 1, (y + 1) * size + 1 ) :: walkPerimeter (Cursor West ( x - 1, y + 1 ) (depth + 1))
                    else if block |> occupies (( x, y ) |> byOne South) then
                        walkPerimeter (Cursor South ( x, y + 1 ) (depth + 1))
                    else
                        ( x * size + 1, ((y + 1) * size - 1) ) :: walkPerimeter (Cursor East ( x, y ) (depth + 1))

                Cursor East ( x, y ) depth ->
                    if block |> occupies (( x, y ) |> byOne East |> byOne South) then
                        ( (x + 1) * size + 1, (y + 1) * size - 1 ) :: walkPerimeter (Cursor South ( x + 1, y + 1 ) (depth + 1))
                    else if block |> occupies (( x, y ) |> byOne East) then
                        walkPerimeter (Cursor East ( x + 1, y ) (depth + 1))
                    else
                        ( (x + 1) * size - 1, ((y + 1) * size - 1) ) :: walkPerimeter (Cursor North ( x, y ) (depth + 1))

                Cursor North ( x, y ) depth ->
                    if block |> occupies (( x, y ) |> byOne North |> byOne East) then
                        ( (x + 1) * size - 1, y * size - 1 ) :: walkPerimeter (Cursor East ( x + 1, y - 1 ) (depth + 1))
                    else if block |> occupies (( x, y ) |> byOne North) then
                        walkPerimeter (Cursor North ( x, y - 1 ) (depth + 1))
                    else
                        ( (x + 1) * size - 1, (y * size + 1) ) :: walkPerimeter (Cursor West ( x, y ) (depth + 1))

                Cursor West ( x, y ) depth ->
                    if block |> occupies (( x, y ) |> byOne West |> byOne North) then
                        ( x * size - 1, y * size + 1 ) :: walkPerimeter (Cursor North ( x - 1, y - 1 ) (depth + 1))
                    else if block |> occupies (( x, y ) |> byOne West) then
                        walkPerimeter (Cursor West ( x - 1, y ) (depth + 1))
                    else
                        ( x * size + 1, (y * size + 1) ) :: walkPerimeter (Cursor South ( x, y ) (depth + 1))
    in
        walkPerimeter (Cursor South ( 0, 0 ) 0)
            |> List.reverse
            |> List.map (\( x, y ) -> (toString x) ++ "," ++ (toString y))
            |> String.join " "


translateString : number -> number -> String
translateString x y =
    "translate(" ++ (toString x) ++ " " ++ (toString y) ++ ")"
