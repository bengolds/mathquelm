module Mathquelm.Insert exposing (..)

import List.Extra as List
import Mathquelm.Digit as Digit exposing (Digit)
import Mathquelm.EditableMath as EMath exposing (Command(..), MathBeingEdited(..))
import Mathquelm.ListZipper as ListZipper


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
                        ListZipper.getAllAfter cursorBlock

                    ( leftParent, numerator ) =
                        splitAtRightMostTerm (ListZipper.getAllBefore cursorBlock)

                    fraction =
                        if List.isEmpty numerator then
                            EMath.DivWithTopHole []
                        else
                            EMath.DivWithBotHole numerator

                    newParentBlock =
                        EMath.BlockWithBlockHole
                            -- TODO THIS IS JANKY
                            (ListZipper.fromList rightParent
                                |> ListZipper.insertBefore leftParent
                            )
                            fraction
                in
                ( ListZipper.empty, newParentBlock :: restOfTree )

        Selection ( selectionBlock, restOfTree ) ->
            mathBeingEdited


splitAtRightMostTerm block =
    ( List.dropWhileRight ((/=) Plus) block
    , List.takeWhileRight ((/=) Plus) block
    )
