module Fixtures.Level exposing (..)

import Turnabout.Level.Types exposing (..)
import Turnabout.Level.String as LevelStrings
import Turnabout.Level.Parser as Parser


levelOne : Level
levelOne =
    Parser.parse LevelStrings.one
