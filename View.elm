module View exposing (boardView)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color exposing (..)
import Animation exposing (Angle)
import Levels exposing (Level)


reddish =
    rgb 208 73 66


reddishString =
    "rgb(208, 73, 66)"


blue =
    "rgb(32, 22, 129)"


size =
    20


type Rotation
    = Clockwise
    | CounterClockwise


type Transform
    = Rotation Int Int Int
    | Translate Int Int


css =
    """
g {
    transform-origin: center;
}
"""


boardView : Level -> Animation.State -> Svg msg
boardView level animatedStyles =
    let
        svgPlayfield =
            level
                |> List.indexedMap
                    (\y row ->
                        List.indexedMap
                            (\x tile ->
                                convertTileToSvg tile ( x, y )
                            )
                            row
                    )
                |> List.concatMap identity
    in
        svg
            [ version "1.1"
            , x "0"
            , y "0"
            , width "100%"
            , viewBox "-100 -100 400 400"
            , preserveAspectRatio "xMaxYMin meet"
            ]
            [ Svg.style [] [ Svg.text css ]
            , g (Animation.render animatedStyles) svgPlayfield
            ]


type alias Point =
    ( Int, Int )


svgSquare : Point -> String -> Svg msg
svgSquare ( gridX, gridY ) color =
    rect
        [ fill color
        , x (toString (gridX * size))
        , y (toString (gridY * size))
        , width (toString size)
        , height (toString size)
        ]
        []


svgMarble : Point -> Svg msg
svgMarble ( x, y ) =
    circle
        [ fill blue
        , cx (toString ((x * size) + (size // 2)))
        , cy (toString ((y * size) + (size // 2)))
        , r (toString (size / 2))
        ]
        []


convertTileToSvg : Levels.Tile -> Point -> Svg msg
convertTileToSvg tile coordinates =
    case tile of
        Levels.Wall ->
            svgSquare coordinates reddishString

        Levels.Block ->
            svgSquare coordinates "tan"

        Levels.Marble color ->
            svgMarble coordinates

        Levels.Goal color ->
            svgSquare coordinates blue

        Levels.Floor ->
            svgSquare coordinates "lightgray"

        Levels.Empty ->
            svgSquare coordinates "white"



-- toFill : Color -> String
-- toFill color =
--   case color of
--     RGBA r g b a -> "rgba(" ++ (toString r) ++ ", 1, 1, 1)"
