module Turnabout.BlockTest exposing (suite)

import Test exposing (..)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, attribute)
import Svg.Attributes
import Turnabout.Block as Block exposing (Block)


suite : Test
suite =
    describe "A block movable"
        [ test "draws a square when given a one-unit block" <|
            \_ ->
                Block.singleton
                    |> expectPolygonWithPoints "1,1 19,1 19,19 1,19"
        , test "draws a 1x2 rectangle when given a block of that shape" <|
            \_ ->
                Block.singleton
                    |> Block.withPart ( 0, 1 )
                    |> expectPolygonWithPoints "1,1 19,1 19,39 1,39"
        , test "draws some nutty concave thing" <|
            \_ ->
                Block.singleton
                    |> Block.withPart ( 1, 0 )
                    |> Block.withPart ( 1, 1 )
                    |> Block.withPart ( 1, 2 )
                    |> Block.withPart ( 1, 3 )
                    |> Block.withPart ( 0, 3 )
                    |> Block.withPart ( -1, 3 )
                    |> expectPolygonWithPoints "1,1 39,1 39,79 -19,79 -19,61 21,61 21,19 1,19"
        ]


expectPolygonWithPoints : String -> Block -> Expect.Expectation
expectPolygonWithPoints points block =
    Block.view 20 ( block, ( 3, 4 ) )
        |> Query.fromHtml
        |> Query.find [ tag "polygon" ]
        |> Query.has [ attribute (Svg.Attributes.points points) ]
