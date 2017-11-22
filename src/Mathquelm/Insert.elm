module Mathquelm.Insert exposing (..)

import Mathquelm.Digit as Digit exposing (Digit)
import Mathquelm.EditableMath as EMath exposing (MathBeingEdited(..))


type Insertion
    = InsertVar String
    | InsertDigit Digit
    | InsertFraction
    | InsertPlus


insert : Insertion -> MathBeingEdited -> MathBeingEdited
insert insertion mathBeingEdited =
    case insertion of
        InsertVar var ->
            insertCmd (EMath.Var var) mathBeingEdited

        InsertDigit digit ->
            insertCmd (EMath.Digit digit) mathBeingEdited

        InsertFraction ->
            insertFraction mathBeingEdited

        InsertPlus ->
            insertCmd EMath.Plus mathBeingEdited


insertCmd cmd mathBeingEdited =
    case mathBeingEdited of
        Cursor ( cursorBlock, restOfTree ) ->
            Cursor ( EMath.insertLeftOfCursor cmd cursorBlock, restOfTree )
                |> checkForAutoCmds

        Selection ( selectionBlock, restOfTree ) ->
            Selection ( { selectionBlock | selected = [ cmd ] }, restOfTree )


checkForAutoCmds =
    identity


insertFraction mathBeingEdited =
    mathBeingEdited
