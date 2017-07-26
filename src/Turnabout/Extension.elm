module Turnabout.Extension exposing (zipDict)

import Dict exposing (Dict)


{-| Get a list of pairs of all values for two dictionaries that have the same keys.
It will only match up with keys in the second dict that are also in the first.
-}
zipDict : Dict comparable a -> Dict comparable b -> List ( b, a )
zipDict haystack needle =
    let
        onlyInHaystack _ _ pairs =
            pairs

        inBoth _ h n pairs =
            ( n, h ) :: pairs

        onlyInNeedle _ _ pairs =
            pairs

        initialPairs =
            []
    in
        Dict.merge onlyInHaystack inBoth onlyInNeedle haystack needle initialPairs
