module Turnabout.Board exposing (Msg, State, initialState, rotate, appear, reset, update, subscriptions, view)

import Animation exposing (Angle, deg)
import Color exposing (..)
import Color.Convert exposing (colorToHex)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)
import Turnabout.Level as Level exposing (Level)
import Turnabout.Types exposing (Rotation(Clockwise, CounterClockwise), Moves)


-- CONFIGURATION


reddish : Color
reddish =
    rgb 208 73 66


blue : Color
blue =
    rgb 32 22 129


size : Int
size =
    20


springConfig =
    { stiffness = 200, damping = 21 }



-- TYPES


type alias Point =
    ( Int, Int )


type alias State =
    { styles : Animation.State
    }


type Msg
    = Animate Animation.Msg
    | Rotate Moves
    | Appear
    | Reset



-- STATE


initialState : State
initialState =
    { styles =
        Animation.styleWith
            (Animation.spring springConfig)
            [ Animation.rotate (deg 0), Animation.scale 0 ]
    }



-- COMMANDS


rotate : Moves -> Msg
rotate moves =
    Rotate moves


appear : Msg
appear =
    Appear


reset : Msg
reset =
    Reset



-- UPDATE


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Rotate moves ->
            ( { state | styles = animateRotation moves state.styles }, Cmd.none )

        Appear ->
            let
                property =
                    Animation.to [ Animation.scale 1 ]

                styles =
                    Animation.queue [ property ] state.styles
            in
                ( { state | styles = styles }, Cmd.none )

        Reset ->
            let
                property =
                    Animation.to [ Animation.scale 0, Animation.rotate (deg 0) ]

                styles =
                    Animation.queue [ property ] state.styles
            in
                ( { state | styles = styles }, Cmd.none )

        Animate amount ->
            ( { state | styles = Animation.update amount state.styles }, Cmd.none )


animateRotation : Moves -> Animation.State -> Animation.State
animateRotation moves style =
    let
        degrees =
            reduceMoves moves |> toFloat |> deg

        animationSteps =
            Animation.to [ Animation.rotate degrees ]
    in
        Animation.queue [ animationSteps ] style


rotationInDegrees : Rotation -> Int
rotationInDegrees rotation =
    case rotation of
        Clockwise ->
            90

        CounterClockwise ->
            -90


reduceMoves : Moves -> Int
reduceMoves moves =
    List.foldl (+) 0 (List.map rotationInDegrees moves)



-- SUBSCRIPTIONS


subscriptions : State -> Sub Msg
subscriptions state =
    Animation.subscription Animate [ state.styles ]



-- VIEW


view : Level -> State -> Svg msg
view level animationState =
    let
        viewBoxWidth =
            size * (Level.size level |> .width) + (size * 4)

        viewBoxheight =
            size * (Level.size level |> .height) + (size * 4)

        viewBoxValue =
            [ 0, 0, viewBoxWidth, viewBoxheight ] |> List.map toString |> String.join " "

        translationValue =
            "translate(" ++ (size * 2 |> toString) ++ ", " ++ (size * 2 |> toString) ++ ")"
    in
        Svg.svg
            [ version "1.1"
            , width "100%"
            , height "100%"
            , viewBox viewBoxValue
            , preserveAspectRatio "xMidYMid meet"
            , Svg.Attributes.style "background-color: #FAFEFA"
            , style "background-color: #AAA"
            ]
            [ Svg.g [ transform translationValue ] [ theBoardItself level animationState ] ]


inlineStyles : Svg.Attribute msg
inlineStyles =
    Svg.Attributes.style "transform-origin: center"


theBoardItself : Level -> State -> Svg msg
theBoardItself level animationState =
    Svg.g
        (inlineStyles :: Animation.render animationState.styles)
        [ (lazy boardTiles level) ]


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


svgSquare : Point -> Color -> Svg msg
svgSquare ( gridX, gridY ) color =
    Svg.rect
        [ fill (colorToHex color)
        , x (toString (gridX * size + 1))
        , y (toString (gridY * size + 1))
        , width (toString (size - 2))
        , height (toString (size - 2))
        ]
        []


svgMarble : Point -> Color -> Svg msg
svgMarble ( x, y ) color =
    Svg.g []
        [ svgSquare ( x, y ) Color.lightGray
        , Svg.circle
            [ fill (colorToHex color)
            , cx (toString ((x * size) + (size // 2)))
            , cy (toString ((y * size) + (size // 2)))
            , r (toString (size // 2))
            ]
            []
        ]


reifyColor : Level.Color -> Color
reifyColor color =
    case color of
        Level.Red ->
            rgb 208 73 66

        Level.Green ->
            rgb 0 200 0

        Level.Blue ->
            rgb 32 22 129


convertTileToSvg : Level.Tile -> Point -> Svg msg
convertTileToSvg tile coordinates =
    case tile of
        Level.Wall ->
            svgSquare coordinates Color.charcoal

        Level.Block ->
            svgSquare coordinates Color.lightBrown

        Level.Marble color ->
            svgMarble coordinates (reifyColor color)

        Level.Goal color ->
            svgSquare coordinates (reifyColor color)

        Level.Floor ->
            svgSquare coordinates Color.lightGray

        Level.Empty ->
            Svg.text ""
