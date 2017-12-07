module Mathquelm.Insert exposing (..)

import List.Extra as List
import Mathquelm.AutoCommands as AutoCommand exposing (AutoCommand)
import Mathquelm.Config exposing (Config)
import Mathquelm.Digit as Digit exposing (Digit)
import Mathquelm.Edit.Command exposing (Command(..))
import Mathquelm.Edit.EditableMath as EditableMath exposing (MathBeingEdited(..))
import Mathquelm.Edit.MathWithCursor as Cursor exposing (BlockWithCursor)
import Mathquelm.Edit.MathWithSelection as Selection exposing (BlockWithSelection)
import Mathquelm.Edit.TreeWithBlockHole as TreeWithBlockHole exposing (CommandWithBlockHole(..))
import Mathquelm.ListZipper as ListZipper


type Insertion
    = InsertVar String
    | InsertDigit Digit
    | InsertFraction
    | InsertPlus


insert : Config -> Insertion -> MathBeingEdited -> MathBeingEdited
insert config insertion mathBeingEdited =
    case insertion of
        InsertVar var ->
            insertCmd (Var var) mathBeingEdited
                |> checkForAutoCmds config.autoCmds

        InsertDigit digit ->
            insertCmd (Digit digit) mathBeingEdited

        InsertFraction ->
            insertFraction mathBeingEdited

        InsertPlus ->
            insertCmd Plus mathBeingEdited


insertCmd : Command -> MathBeingEdited -> MathBeingEdited
insertCmd cmd mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            Cursor (Cursor.insertLeftOfCursor cmd mathWithCursor)

        Selection mathWithSelection ->
            Cursor
                (EditableMath.deleteInsideSelection mathWithSelection
                    |> Cursor.insertLeftOfCursor cmd
                )


checkForAutoCmds autoCmds mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            autoCmds
                |> List.filterMap
                    (tryMatchAutoCmd mathWithCursor)
                |> List.head
                |> Maybe.withDefault mathWithCursor
                |> Cursor

        Selection ( selectionBlock, restOfTree ) ->
            Selection ( selectionBlock, restOfTree )


getStringAroundCursor cursorBlock =
    let
        buildString nextCmd string =
            case nextCmd of
                Var text ->
                    string ++ text

                _ ->
                    string

        isVar cmd =
            case cmd of
                Var _ ->
                    True

                _ ->
                    False

        leftString =
            ListZipper.getAllBefore cursorBlock
                |> List.takeWhileRight isVar
                |> List.foldr buildString ""
                |> String.reverse

        rightString =
            ListZipper.getAllAfter cursorBlock
                |> List.takeWhile isVar
                |> List.foldl buildString ""

        stringAroundCursor =
            leftString ++ rightString

        cursorIndex =
            String.length leftString
    in
    ( stringAroundCursor, cursorIndex )


tryMatchAutoCmd : Cursor.MathWithCursor -> AutoCommand -> Maybe Cursor.MathWithCursor
tryMatchAutoCmd ( cursorBlock, restOfTree ) autoCmd =
    let
        cmdString =
            AutoCommand.triggerString autoCmd

        ( stringAroundCursor, cursorIndex ) =
            getStringAroundCursor cursorBlock

        _ =
            Debug.log "stringAroundCursor: " ( stringAroundCursor, cursorIndex )

        getFirstMatchContainingCursor : List Int -> Maybe Int
        getFirstMatchContainingCursor startIndices =
            List.filter
                (\startIndex ->
                    (startIndex < cursorIndex)
                        && (cursorIndex <= startIndex + String.length cmdString)
                )
                startIndices
                |> List.head

        nTimes n fn val =
            if n <= 0 then
                val
            else
                fn (nTimes (n - 1) fn val)
    in
    String.indices cmdString stringAroundCursor
        |> getFirstMatchContainingCursor
        |> Maybe.andThen
            (\startIndex ->
                cursorBlock
                    |> nTimes (cursorIndex - startIndex) ListZipper.removeBefore
                    |> nTimes (startIndex + String.length cmdString - cursorIndex) ListZipper.removeAfter
                    |> ListZipper.insertAfter
                        [ AutoCommand.toCommand autoCmd ]
                    |> (\c -> ( c, restOfTree ))
                    |> Cursor.enterCommandToRight
            )


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
                            DivWithTopHole []
                        else
                            DivWithBotHole numerator

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
