module Mathquelm.AutoCommands exposing (..)

import Mathquelm.EditableMath as EMath


type AutoCommand
    = Cos


triggerString : AutoCommand -> String
triggerString autoCmd =
    case autoCmd of
        Cos ->
            "cos"


toCommand : AutoCommand -> EMath.Command
toCommand autoCmd =
    case autoCmd of
        Cos ->
            EMath.Cos []
