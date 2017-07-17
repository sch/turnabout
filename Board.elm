module Board exposing (view)

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


inlineStyles : Svg.Attribute msg
inlineStyles =
    Svg.Attributes.style "transform-origin: center"


view : Level -> Animation.State -> Svg msg
view level animatedStyles =
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , width "100%"
        , viewBox "-200 -100 600 400"
        , preserveAspectRatio "xMaxYMin meet"
        , Svg.Attributes.style "background-color: #FAFEFA"
        ]
        [ theBoardItself level animatedStyles ]


theBoardItself : Level -> Animation.State -> Svg msg
theBoardItself level animatedStyles =
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

        boardAttributes =
            inlineStyles :: (Animation.render animatedStyles)
    in
        g boardAttributes svgPlayfield



type alias Point =
    ( Int, Int )


svgSquare : Point -> String -> Svg msg
svgSquare ( gridX, gridY ) color =
    rect
        [ fill color
        , x (toString (gridX * size - 1))
        , y (toString (gridY * size - 1))
        , width (toString (size - 1))
        , height (toString (size - 1))
        ]
        []


svgMarble : Point -> Svg msg
svgMarble ( x, y ) =
    g []
        [ svgSquare ( x, y ) "lightgray"
        , circle
            [ fill blue
            , cx (toString ((x * size - 1) + (size // 2)))
            , cy (toString ((y * size - 1) + (size // 2)))
            , r (toString ((size - 2) / 2))
            ]
            []
        ]


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
