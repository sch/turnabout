module Fixtures.Level exposing (..)

import Turnabout.Level exposing (Level)
import Turnabout.Level.String as LevelStrings
import Turnabout.Level.Parser as Parser
import Turnabout.Level.Parser as Parser


levelOne : Level
levelOne =
    Parser.parse LevelStrings.one
