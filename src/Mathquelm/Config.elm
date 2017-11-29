module Mathquelm.Config exposing (..)

import Mathquelm.AutoCommands exposing (AutoCommand)
import Style.Scale as Scale


type alias Config =
    { showCenterLines : Bool
    , showBoxes : Bool
    , maxDepth : Int
    , baseFontSize : Float
    , modularScale : Float
    , autoCmds : List AutoCommand
    }


default =
    { showCenterLines = False
    , showBoxes = False
    , maxDepth = 3
    , baseFontSize = 28.8
    , modularScale = 0.7
    , autoCmds = [ Mathquelm.AutoCommands.Cos ]
    }


scaled config depth =
    Scale.modular config.baseFontSize config.modularScale depth


toggleCenterLineDisplay config =
    { config | showCenterLines = not config.showCenterLines }


toggleBoxesDisplay config =
    { config | showBoxes = not config.showBoxes }
