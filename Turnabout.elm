module Turnabout exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Attributes
import Color exposing (..)
import Levels exposing (exampleLevel)
import Keyboard


main =
    Html.program
        { init = ( levelStart, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


reddish =
    rgb 208 73 66


reddishString =
    "rgb(208, 73, 66)"


blue =
    "rgb(32, 22, 129)"


size =
    20


type Cardinality
    = North
    | East
    | South
    | West


type Orientation
    = Top
    | Right
    | Bottom
    | Left


type Rotation
    = Clockwise
    | CounterClockwise


type alias Model =
    { gravity : Cardinality
    , board : Levels.Level
    , moves : List Rotation
    }


levelStart : Model
levelStart =
    { gravity = South
    , board = exampleLevel
    , moves = [ CounterClockwise, CounterClockwise, Clockwise ]
    }


type Msg
    = Rotate Rotation
    | NoOp


type Transform
    = Rotation Int Int Int
    | Translate Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate rotation ->
            ( { model | moves = rotation :: model.moves }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs rotationsFromCode


type Key
    = LeftArrow
    | RightArrow
    | Other


rotationsFromCode : Int -> Msg
rotationsFromCode keyCode =
    case keyCode of
        37 ->
            Rotate Clockwise

        39 ->
            Rotate CounterClockwise

        _ ->
            NoOp


rotationInDegrees : Rotation -> Int
rotationInDegrees rotation =
    case rotation of
        Clockwise ->
            90

        CounterClockwise ->
            -90


reduceMoves : List Rotation -> Int
reduceMoves moves =
    List.foldl (+) 0 (List.map rotationInDegrees moves)


transformString : Transform -> String
transformString trans =
    case trans of
        Rotation degrees x y ->
            "rotate(" ++ (toString degrees) ++ " " ++ (toString x) ++ " " ++ (toString y) ++ ")"

        Translate x y ->
            "translate(" ++ (toString x) ++ "px, " ++ (toString y) ++ "px)"


view : Model -> Html msg
view model =
    let
        levelSize =
            Levels.size model.board

        -- @TODO change back to this when generating SVG from levels programatically: (levelSize.width * size // 2)
        xCenter =
            (5 * size // 2)

        yCenter =
            (levelSize.height * size // 2)

        rotation =
            Rotation (reduceMoves model.moves) xCenter yCenter
    in
        Html.div [ Html.Attributes.style [ ( "margin", "1em auto" ) ] ]
            [ svg
                [ version "1.1"
                , x "0"
                , y "0"
                , width "100%"
                , viewBox "0 0 300 400"
                , preserveAspectRatio "xMaxYMin meet"
                ]
                [ g [ transform ("translate(100 100) " ++ transformString rotation) ]
                    [ svgWall ( 1, 3 )
                    , svgWall ( 2, 3 )
                    , svgWall ( 0, 3 )
                    , svgWall ( 0, 2 )
                    , svgWall ( 0, 1 )
                    , svgWall ( 0, 0 )
                    , svgWall ( 1, 0 )
                    , svgWall ( 2, 0 )
                    , svgWall ( 3, 0 )
                    , svgWall ( 4, 0 )
                    , svgWall ( 4, 1 )
                    , svgWall ( 4, 2 )
                    , svgWall ( 4, 3 )
                    , svgWall ( 2, 2 )
                    , svgWall ( 3, 3 )
                    , svgMarble ( 3, 2 )
                    ]
                ]
            ]


svgWall : ( Int, Int ) -> Svg msg
svgWall ( gridX, gridY ) =
    rect
        [ fill reddishString
        , x (toString (gridX * size))
        , y (toString (gridY * size))
        , width (toString size)
        , height (toString size)
        , stroke reddishString
        ]
        []


svgMarble : ( Int, Int ) -> Svg msg
svgMarble ( x, y ) =
    circle
        [ fill blue
        , cx (toString ((x * size) + (size // 2)))
        , cy (toString ((y * size) + (size // 2)))
        , r (toString (size / 2))
        ]
        []


toSvg field =
    let
        mapRowsToSvg =
            List.indexedMap
    in
        List.map mapRowsToSvg field


convertTileToSvg : Levels.Tile -> Svg msg
convertTileToSvg tile =
    case tile of
        Levels.Wall ->
            svgWall ( 1, 3 )

        Levels.Block ->
            svgMarble ( 3, 2 )

        Levels.Marble color ->
            svgMarble ( 3, 2 )

        Levels.Empty ->
            svgWall ( 1, 3 )



-- toFill : Color -> String
-- toFill color =
--   case color of
--     RGBA r g b a -> "rgba(" ++ (toString r) ++ ", 1, 1, 1)"
