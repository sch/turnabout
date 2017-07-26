module Turnabout.BlockTest exposing (suite)

import Test exposing (..)
import Expect
import Turnabout.Block as Block exposing (Block)


suite : Test
suite =
    describe "A block movable"
        [ test "takes up at least one tile" <|
            \_ ->
                Block.singleton
                    |> (\block ->
                            ( Block.width block, Block.height block )
                       )
                    |> Expect.equal ( 1, 1 )
        , test "can be made of more than one tile" <|
            \_ ->
                Block.singleton
                    |> Block.withPart ( 1, 0 )
                    |> (\block ->
                            ( Block.width block, Block.height block )
                       )
                    |> Expect.equal ( 2, 1 )
        ]
