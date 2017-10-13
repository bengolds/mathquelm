module Mathquelm.Config exposing (..)

import Style.Scale as Scale


type alias Config =
    { showCenterLines : Bool
    , showBoxes : Bool
    , maxDepth : Int
    , baseFontSize : Float
    , modularScale : Float
    }


default =
    { showCenterLines = False
    , showBoxes = False
    , maxDepth = 3
    , baseFontSize = 28.8
    , modularScale = 0.7
    }


scaled config depth =
    Scale.modular config.baseFontSize config.modularScale depth


toggleCenterLineDisplay config =
    { config | showCenterLines = not config.showCenterLines }


toggleBoxesDisplay config =
    { config | showBoxes = not config.showBoxes }
