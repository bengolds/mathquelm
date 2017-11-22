module Mathquelm.Insert exposing (..)

import List.Extra as List
import Mathquelm.Digit as Digit exposing (Digit)
import Mathquelm.EditableMath as EMath exposing (Command(..), MathBeingEdited(..))


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
    case mathBeingEdited of
        Cursor ( cursorBlock, restOfTree ) ->
            Cursor <|
                let
                    rightParent =
                        cursorBlock.right

                    ( leftParent, numerator ) =
                        splitAtRightMostTerm (List.reverse cursorBlock.left)

                    fraction =
                        if List.isEmpty numerator then
                            EMath.DivWithTopHole []
                        else
                            EMath.DivWithBotHole numerator

                    blockWithBlockHole =
                        EMath.BlockWithBlockHole
                            (List.reverse leftParent)
                            fraction
                            rightParent
                in
                ( EMath.BlockWithCursor [] [], blockWithBlockHole :: restOfTree )

        Selection ( selectionBlock, restOfTree ) ->
            mathBeingEdited


splitAtRightMostTerm block =
    ( List.dropWhileRight ((/=) Plus) block
    , List.takeWhileRight ((/=) Plus) block
    )
