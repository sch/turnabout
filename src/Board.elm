module Board exposing (view)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)
import Color exposing (..)
import Color.Convert exposing (colorToHex)
import Animation exposing (Angle)
import Levels exposing (Level)


reddish : Color
reddish =
    rgb 208 73 66


blue : Color
blue =
    rgb 32 22 129



{- This is how wide/tall each tile should be (pixel) -}


size : Int
size =
    10


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
    Svg.svg
        [ version "1.1"
        , x "0"
        , y "0"
        , width "100%"
        , height "100%"
        , viewBox "-100 -50 300 200"
        , preserveAspectRatio "xMaxYMin meet"
        , Svg.Attributes.style "background-color: #FAFEFA"
        , shapeRendering "crispEdges"
        ]
        [ theBoardItself level animatedStyles ]


theBoardItself : Level -> Animation.State -> Svg msg
theBoardItself level animatedStyles =
    Svg.g (inlineStyles :: (Animation.render animatedStyles)) [ (lazy boardTiles level) ]


boardTiles : Level -> Svg msg
boardTiles level =
    let
        tiles =
            level
                |> List.indexedMap
                    (\y row ->
                        List.indexedMap (\x tile -> convertTileToSvg tile ( x, y )) row
                    )
                |> List.concatMap identity
    in
        Svg.g [] tiles


type alias Point =
    ( Int, Int )


svgSquare : Point -> Color -> Svg msg
svgSquare ( gridX, gridY ) color =
    Svg.rect
        [ fill (colorToHex color)
        , x (toString (gridX * size - 1))
        , y (toString (gridY * size - 1))
        , width (toString (size - 1))
        , height (toString (size - 1))
        ]
        []


svgMarble : Point -> Svg msg
svgMarble ( x, y ) =
    Svg.g []
        [ svgSquare ( x, y ) Color.lightGray
        , Svg.circle
            [ fill (colorToHex blue)
            , cx (toString ((x * size - 1) + (size // 2)))
            , cy (toString ((y * size - 1) + (size // 2)))
            , r (toString ((size - 1) // 2))
            ]
            []
        ]


convertTileToSvg : Levels.Tile -> Point -> Svg msg
convertTileToSvg tile coordinates =
    case tile of
        Levels.Wall ->
            svgSquare coordinates reddish

        Levels.Block ->
            svgSquare coordinates Color.lightBrown

        Levels.Marble color ->
            svgMarble coordinates

        Levels.Goal color ->
            svgSquare coordinates blue

        Levels.Floor ->
            svgSquare coordinates Color.lightGray

        Levels.Empty ->
            svgSquare coordinates Color.white
