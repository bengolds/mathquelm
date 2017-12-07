module Mathquelm.AutoCommands exposing (..)

import Mathquelm.Edit.Command as EditCommand


type AutoCommand
    = Cos


triggerString : AutoCommand -> String
triggerString autoCmd =
    case autoCmd of
        Cos ->
            "cos"


toCommand : AutoCommand -> EditCommand.Command
toCommand autoCmd =
    case autoCmd of
        Cos ->
            EditCommand.Cos []
