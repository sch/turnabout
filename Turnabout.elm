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
    , moves = []
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
            Rotate CounterClockwise

        39 ->
            Rotate Clockwise

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
            Levels.size (Debug.log "the board" model.board)

        xCenter =
            (levelSize.width * size // 2)

        yCenter =
            (levelSize.height * size // 2)

        rotation =
            Rotation (reduceMoves model.moves) xCenter yCenter

        svgPlayfield =
            model.board
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
                    svgPlayfield
                ]
            ]


type alias Point = (Int, Int)

svgSquare : Point -> String -> Svg msg
svgSquare ( gridX, gridY ) color =
    rect
        [ fill color
        , x (toString (gridX * size))
        , y (toString (gridY * size))
        , width (toString size)
        , height (toString size)
        , stroke color
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
