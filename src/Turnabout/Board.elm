module Turnabout.Board exposing (Msg, State, initialState, rotate, appear, reset, update, subscriptions, view)

import Animation exposing (Angle, deg)
import Color exposing (..)
import Color.Convert exposing (colorToHex)
import Dict
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)
import Turnabout.Level as Level
import Turnabout.Level.Types as Level
import Turnabout.Types exposing (Rotation(Clockwise, CounterClockwise), Moves)


-- CONFIGURATION


{-| The size (in magic svg units) of each block
-}
size : Int
size =
    20


{-| How many blocks should be used as padding around the edges of the playfield
-}
padding : Int
padding =
    2


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


view : Level.Level -> State -> Svg msg
view level animationState =
    let
        viewBoxWidth =
            (level.size.width + (padding * 2)) * size

        viewBoxheight =
            (level.size.height + (padding * 2)) * size

        viewBoxValue =
            [ 0, 0, viewBoxWidth, viewBoxheight ] |> List.map toString |> String.join " "

        translationValue =
            "translate(" ++ (size * padding |> toString) ++ ", " ++ (size * padding |> toString) ++ ")"
    in
        Svg.svg
            [ version "1.1"
            , width "100%"
            , height "100%"
            , viewBox viewBoxValue
            , preserveAspectRatio "xMidYMid meet"
            , style "background-color:#F6F2F1 ; position:fixed ; top:0 ; left:0"
            ]
            [ Svg.g [ transform translationValue ] [ theBoardItself level animationState ] ]


inlineStyles : Svg.Attribute msg
inlineStyles =
    Svg.Attributes.style "transform-origin: center"


theBoardItself : Level.Level -> State -> Svg msg
theBoardItself level animationState =
    Svg.g
        (inlineStyles :: Animation.render animationState.styles)
        [ (lazy boardView level.board), movablesView level.movables ]


boardView : Level.Board -> Svg msg
boardView board =
    let
        accumulateSvgTiles coordinate tile list =
            (svgSquare (tileColor tile) coordinate) :: list

        tiles =
            Dict.foldl accumulateSvgTiles [] board
    in
        Svg.g [] tiles


tileColor : Level.Tile -> Color
tileColor tile =
    case tile of
        Level.Wall ->
            Color.darkCharcoal

        Level.Floor ->
            Color.lightGray


movablesView : List Level.Movable -> Svg msg
movablesView movables =
    let
        items =
            List.map movableView movables
    in
        Svg.g [] items


movableView : Level.Movable -> Svg a
movableView movable =
    case movable of
        Level.Marble color coordinates ->
            svgMarble (toColor color) coordinates

        Level.Goal color coordinates ->
            svgSquare (toColor color) coordinates

        Level.Block _ _ ->
            Svg.text ""



-- boardTiles : Level.Level -> Svg msg
-- boardTiles level =
--     let
--         tiles =
--             level
--                 |> List.indexedMap
--                     (\y row ->
--                         List.indexedMap (\x tile -> convertTileToSvg tile ( x, y )) row
--                     )
--                 |> List.concatMap identity
--     in
--         Svg.g [] tiles


svgSquare : Color -> Point -> Svg msg
svgSquare color ( gridX, gridY ) =
    Svg.rect
        [ fill (colorToHex color)
        , x (toString (gridX * size + 1))
        , y (toString (gridY * size + 1))
        , width (toString (size - 2))
        , height (toString (size - 2))
        ]
        []


svgRoundedSquare : Color -> Point -> Svg msg
svgRoundedSquare color ( gridX, gridY ) =
    Svg.rect
        [ fill (colorToHex color)
        , x (toString (gridX * size + 1))
        , y (toString (gridY * size + 1))
        , width (toString (size - 2))
        , height (toString (size - 2))
        , rx "2"
        , ry "2"
        ]
        []


svgMarble : Color -> Point -> Svg msg
svgMarble color ( x, y ) =
    Svg.g []
        [ svgSquare Color.white ( x, y )
        , Svg.circle
            [ fill (colorToHex color)
            , cx (toString ((x * size) + (size // 2)))
            , cy (toString ((y * size) + (size // 2)))
            , r (toString (size // 2))
            ]
            []
        ]


toColor : Level.Color -> Color
toColor color =
    case color of
        Level.Red ->
            rgb 208 73 66

        Level.Green ->
            rgb 164 255 237

        Level.Blue ->
            rgb 55 144 242

        Level.Yellow ->
            rgb 247 179 74

        Level.Purple ->
            rgb 249 180 250



-- convertTileToSvg : Level.Tile -> Point -> Svg msg
-- convertTileToSvg tile coordinates =
--     case tile of
--         Level.Wall ->
--             svgSquare coordinates Color.darkCharcoal
--         Level.Block ->
--             svgRoundedSquare coordinates Color.darkBrown
--         Level.Marble color ->
--             svgMarble coordinates (reifyColor color)
--         Level.Goal color ->
--             svgSquare coordinates (reifyColor color)
--         Level.Floor ->
--             svgSquare coordinates Color.white
--         Level.Empty ->
--             Svg.text ""
