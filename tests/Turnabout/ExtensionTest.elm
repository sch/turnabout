module Turnabout.ExtensionTest exposing (suite)

import Test exposing (..)
import Expect
import Dict
import Turnabout.Extension exposing (zipDict)


suite : Test
suite =
    describe "A set of functions that extend core data structures"
        [ test "zipDict: give you a list of value pairs from two dictionaries with the same keys" <|
            \_ ->
                Dict.fromList [ ( 1, "butter" ), ( 5, "apple" ), ( 3, "watermelon" ) ]
                    |> zipDict (Dict.fromList [ ( 1, "yellow" ), ( 2, "blue" ), ( 5, "red" ) ])
                    |> Expect.equal [ ( "apple", "red" ), ( "butter", "yellow" ) ]
        ]
