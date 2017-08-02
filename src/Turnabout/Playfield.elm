module Turnabout.Playfield exposing (Msg(Back), State, init, update, subscriptions, view)

import Animation exposing (Angle, deg)
import Animation.Messenger
import Color exposing (..)
import Color.Convert exposing (colorToHex)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Keyboard
import Octicons
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)
import Turnabout.Block as Block exposing (Block)
import Turnabout.Board as Board exposing (Board)
import Turnabout.Color
import Turnabout.Coordinate exposing (Coordinate)
import Turnabout.Extension exposing (zipDict)
import Turnabout.Level as Level exposing (Level)
import Turnabout.Level.Model exposing (Movable(..))
import Turnabout.Marble as Marble
import Turnabout.Moves as Moves exposing (Moves, Rotation(..))


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
    , level : Level
    , moves : Moves
    }


type alias MovableId =
    Int


type Msg
    = Animate Animation.Msg
    | AnimateMovables MovableId Animation.Msg
    | Rotate Rotation
    | StartAnimatingMovables Level
    | Undo
    | Back
    | NoOp



-- STATE


init : Level -> ( State, Cmd Msg )
init level =
    ( { isAnimating = False
      , styles =
            Animation.styleWith spring
                [ Animation.rotate (deg 0)
                , Animation.scale 0
                ]
                |> Animation.queue [ Animation.to [ Animation.scale 1 ] ]
      , animatedPositions = createInitialMovableStyles level
      , moves = Moves.initial
      , level = level
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Rotate rotation ->
            let
                moves =
                    Moves.rotate rotation state.moves

                level =
                    Turnabout.Level.Model.applyMoves moves state.level

                styles =
                    animateRotation moves level state.styles

                newState =
                    { state
                        | isAnimating = True
                        , moves = moves
                        , styles = styles
                    }
            in
                ( newState, Cmd.none )

        StartAnimatingMovables level ->
            let
                updatedPositions =
                    updatePositons level state.animatedPositions
            in
                ( { state | isAnimating = False, animatedPositions = updatedPositions }, Cmd.none )

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

        Undo ->
            let
                moves =
                    Moves.undo state.moves

                level =
                    Level.applyMoves moves state.level

                styles =
                    animateRotation moves level state.styles
            in
                ( { state | moves = moves, styles = styles }, Cmd.none )

        Back ->
            ( state, Cmd.none )

        NoOp ->
            ( state, Cmd.none )


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
    let
        animationSubscriptions =
            [ Animation.subscription Animate [ state.styles ]
            , positionSubscriptions state.animatedPositions
            ]

        subscriptions =
            if state.isAnimating then
                animationSubscriptions
            else
                (Keyboard.downs messagesFromCode) :: animationSubscriptions
    in
        Sub.batch subscriptions


positionSubscriptions : Dict MovableId (Animation.Messenger.State Msg) -> Sub Msg
positionSubscriptions positions =
    positions
        |> Dict.foldl
            (\id position list ->
                (Animation.subscription (AnimateMovables id) [ position ]) :: list
            )
            []
        |> Sub.batch


messagesFromCode : Int -> Msg
messagesFromCode keyCode =
    case keyCode of
        37 ->
            Rotate CounterClockwise

        39 ->
            Rotate Clockwise

        85 ->
            Undo

        _ ->
            NoOp



-- VIEW


svgView : State -> Svg msg
svgView state =
    let
        (Board.Board _ (Board.Size w h)) =
            state.level.board

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
            [ Svg.g [ transform translationValue ] [ theBoardItself state ] ]


viewbox : Int -> Int -> Svg.Attribute msg
viewbox width height =
    [ 0, 0, width, height ]
        |> List.map toString
        |> String.join " "
        |> Svg.Attributes.viewBox


theBoardItself : State -> Svg msg
theBoardItself state =
    let
        inlineStyles =
            Svg.Attributes.style "transform-origin: center"

        attributes =
            inlineStyles :: Animation.render state.styles

        children =
            [ lazy boardView state.level.board
            , movablesView state.level.movables
            , blocksView state.level
            , marblesView state.level state.animatedPositions
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


view : State -> Html Msg
view playfieldState =
    let
        isVisible =
            playfieldState.moves /= Moves.initial

        offset =
            "16px"

        topLeft =
            absolutelyPositioned [ ( "top", offset ), ( "left", offset ) ]

        topRight =
            absolutelyPositioned [ ( "top", offset ), ( "right", offset ) ]

        bottomLeft =
            absolutelyPositioned [ ( "bottom", offset ), ( "left", offset ) ]

        bottomRight =
            absolutelyPositioned [ ( "bottom", offset ), ( "right", offset ) ]
    in
        Html.div [ Attributes.style [ ( "position", "relative" ), ( "height", "100%" ) ] ]
            [ svgView playfieldState
            , button Back |> topLeft
            , button Undo |> visible isVisible |> topRight
            , button (Rotate CounterClockwise) |> bottomLeft
            , button (Rotate Clockwise) |> bottomRight
            ]


absolutelyPositioned : List ( String, String ) -> Html msg -> Html msg
absolutelyPositioned styles node =
    Html.div
        [ Attributes.style (List.append styles [ ( "position", "absolute" ) ]) ]
        [ node ]


visible : Bool -> Html msg -> Html msg
visible isVisible node =
    let
        ( opacity, touchable ) =
            if isVisible then
                ( "1", "all" )
            else
                ( "0", "none" )

        styles =
            [ ( "transition", "opacity 0.3s ease-in-out" )
            , ( "opacity", opacity )
            , ( "pointer-events", touchable )
            ]
    in
        Html.div [ Attributes.style styles ] [ node ]


button : Msg -> Html Msg
button msg =
    let
        icon =
            case msg of
                Rotate CounterClockwise ->
                    Octicons.chevronLeft

                Rotate Clockwise ->
                    Octicons.chevronRight

                Undo ->
                    Octicons.issueReopened

                Back ->
                    Octicons.listUnordered

                _ ->
                    Debug.crash "You shouldn't be able to get here"

        iconOptions =
            Octicons.defaultOptions |> Octicons.size 24 |> Octicons.color "#555"

        styles =
            [ ( "background-color", "rgba(255, 255, 255, 0.5)" )
            , ( "border", "none" )
            , ( "border-radius", "3px" )
            , ( "padding", "10px 11px" )
            , ( "cursor", "pointer" )
            , ( "touch-action", "manipulation" )
            ]

        attributes =
            [ Attributes.style styles, Events.onClick msg ]
    in
        Html.button attributes [ icon iconOptions ]
