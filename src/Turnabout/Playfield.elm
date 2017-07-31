module Turnabout.Playfield
    exposing
        ( Msg
        , State
        , initialState
        , rotate
        , appear
        , reset
        , update
        , subscriptions
        , isAnimating
        , view
        )

import Animation exposing (Angle, deg)
import Animation.Messenger
import Color exposing (..)
import Color.Convert exposing (colorToHex)
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)
import Turnabout.Block as Block exposing (Block)
import Turnabout.Board as Board exposing (Board)
import Turnabout.Color
import Turnabout.Coordinate exposing (Coordinate)
import Turnabout.Marble as Marble
import Turnabout.Extension exposing (zipDict)
import Turnabout.Level as Level exposing (Level)
import Turnabout.Level.Model as Level exposing (Movable(..))
import Turnabout.Moves as Moves exposing (Moves)


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


spring : Animation.Interpolation
spring =
    Animation.spring { stiffness = 160, damping = 18 }



-- TYPES


type alias Point =
    ( Int, Int )


type alias AnimatedPositions =
    Dict MovableId (Animation.Messenger.State Msg)


type alias State =
    { isAnimating : Bool
    , styles : Animation.Messenger.State Msg
    , animatedPositions : AnimatedPositions
    }


type alias MovableId =
    Int


type Msg
    = Animate Animation.Msg
    | AnimateMovables MovableId Animation.Msg
    | Rotate Moves Level
    | StartAnimatingMovables Level
    | Appear Level
    | Reset



-- STATE


initialState : State
initialState =
    { isAnimating = False
    , styles =
        Animation.styleWith spring
            [ Animation.rotate (deg 0)
            , Animation.scale 0
            ]
    , animatedPositions = Dict.empty
    }



-- QUERIES


isAnimating : State -> Bool
isAnimating state =
    state.isAnimating



-- COMMANDS


rotate : Moves -> Level -> Msg
rotate moves level =
    Rotate moves level


appear : Level -> Msg
appear level =
    Appear level


reset : Msg
reset =
    Reset



-- UPDATE


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Rotate moves level ->
            let
                styles =
                    animateRotation moves level state.styles
            in
                ( { state | styles = styles, isAnimating = True }, Cmd.none )

        StartAnimatingMovables level ->
            let
                updatedPositions =
                    updatePositons level state.animatedPositions
            in
                ( { state | isAnimating = False, animatedPositions = updatedPositions }, Cmd.none )

        Appear level ->
            let
                properties =
                    [ Animation.set [ Animation.scale 0, Animation.rotate (deg 0) ]
                    , Animation.to [ Animation.scale 1 ]
                    ]

                styles =
                    Animation.queue properties state.styles

                positions =
                    createInitialMovableStyles level
            in
                ( { state | styles = styles, animatedPositions = positions }, Cmd.none )

        Reset ->
            let
                property =
                    Animation.to [ Animation.scale 0, Animation.rotate (deg 0) ]

                styles =
                    Animation.queue [ property ] state.styles
            in
                ( { state | styles = styles }, Cmd.none )

        Animate amount ->
            let
                ( styles, commands ) =
                    Animation.Messenger.update amount state.styles
            in
                ( { state | styles = styles }, commands )

        AnimateMovables id amount ->
            let
                updateAnimationDictionaryValue id positions =
                    Animation.Messenger.update amount positions |> Tuple.first

                newPositions =
                    Dict.map updateAnimationDictionaryValue state.animatedPositions

                newState =
                    { state | animatedPositions = newPositions }
            in
                ( newState, Cmd.none )


animateRotation : Moves -> Level -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
animateRotation moves level style =
    let
        degrees =
            moves |> Moves.toDegrees |> toFloat |> deg

        animationSteps =
            [ Animation.to [ Animation.rotate degrees ]
            , Animation.Messenger.send (StartAnimatingMovables level)
            ]
    in
        Animation.interrupt animationSteps style


updatePositons : Level -> AnimatedPositions -> AnimatedPositions
updatePositons level positions =
    let
        marblesByIndex =
            Level.toMarblePairs level |> List.indexedMap (,) |> Dict.fromList

        updatePositons index styles =
            case (Dict.get index marblesByIndex) of
                Just ( _, position ) ->
                    queueAnimation (positionProperties position) styles

                Nothing ->
                    styles
    in
        Dict.map updatePositons positions


createInitialMovableStyles : Level -> AnimatedPositions
createInitialMovableStyles level =
    let
        interpolation =
            Animation.speed { perSecond = 500 }
    in
        level
            |> Level.toMarblePairs
            |> List.indexedMap
                (\index ( _, xy ) ->
                    ( index, Animation.styleWith interpolation (positionProperties xy) )
                )
            |> Dict.fromList



-- SUBSCRIPTIONS


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.batch
        [ Animation.subscription Animate [ state.styles ]
        , positionSubscriptions state.animatedPositions
        ]


positionSubscriptions : Dict MovableId (Animation.Messenger.State Msg) -> Sub Msg
positionSubscriptions positions =
    positions
        |> Dict.foldl
            (\id position list ->
                (Animation.subscription (AnimateMovables id) [ position ]) :: list
            )
            []
        |> Sub.batch



-- VIEW


view : State -> Level -> Svg msg
view state level =
    let
        (Board.Board _ (Board.Size w h)) =
            level.board

        viewBoxWidth =
            (w + (padding * 2)) * size

        viewBoxheight =
            (h + (padding * 2)) * size

        translationValue =
            "translate(" ++ (size * padding |> toString) ++ ", " ++ (size * padding |> toString) ++ ")"
    in
        Svg.svg
            [ version "1.1"
            , width "100%"
            , height "100%"
            , viewbox viewBoxWidth viewBoxheight
            , preserveAspectRatio "xMidYMid meet"
            , style "background-color:#F6F2F1 ; position:fixed ; top:0 ; left:0"
            ]
            [ Svg.g [ transform translationValue ] [ theBoardItself state level ] ]


viewbox : Int -> Int -> Svg.Attribute msg
viewbox width height =
    [ 0, 0, width, height ]
        |> List.map toString
        |> String.join " "
        |> Svg.Attributes.viewBox


theBoardItself : State -> Level -> Svg msg
theBoardItself state level =
    let
        inlineStyles =
            Svg.Attributes.style "transform-origin: center"

        attributes =
            inlineStyles :: Animation.render state.styles

        children =
            [ lazy boardView level.board
            , movablesView level.movables
            , blocksView level
            , marblesView level state.animatedPositions
            ]
    in
        Svg.g attributes children


boardView : Board -> Svg msg
boardView board =
    let
        tiles =
            board
                |> Board.toList
                |> List.map
                    (\( coordinate, tile ) ->
                        svgSquare (tileColor tile) coordinate
                    )
    in
        Svg.g [] tiles


tileColor : Board.Tile -> Color
tileColor tile =
    case tile of
        Board.Wall ->
            Color.charcoal

        Board.Floor ->
            Color.darkGray


movablesView : List Movable -> Svg msg
movablesView movables =
    let
        items =
            List.map movableView movables
    in
        Svg.g [] items


blocksView : Level -> Svg msg
blocksView level =
    let
        blocks =
            level |> Level.toBlockPairs |> List.map (Block.view size)
    in
        Svg.g [] blocks


marblesView : Level -> AnimatedPositions -> Svg msg
marblesView level positions =
    let
        marblesByIndex =
            Level.toMarblePairs level |> List.indexedMap (,) |> Dict.fromList

        -- ugh
        blocks =
            zipDict positions marblesByIndex
                |> List.map
                    (\( ( marble, _ ), xy ) ->
                        Svg.svg (Animation.render xy) [ (Marble.view marble) ]
                    )
    in
        Svg.g [] blocks


movableView : Movable -> Svg a
movableView movable =
    case movable of
        Murble _ _ ->
            Svg.text ""

        Goal color coordinates ->
            svgSquare (Turnabout.Color.toRgb color) coordinates

        Block _ _ ->
            Svg.text ""


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


positionProperties : Coordinate -> List Animation.Property
positionProperties ( x, y ) =
    [ Animation.x (toFloat (x * size))
    , Animation.y (toFloat (y * size))
    ]


queueAnimation :
    List Animation.Property
    -> Animation.Messenger.State Msg
    -> Animation.Messenger.State Msg
queueAnimation properties styles =
    Animation.queue [ Animation.to properties ] styles
