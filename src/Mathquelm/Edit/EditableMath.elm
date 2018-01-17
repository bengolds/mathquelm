module Mathquelm.Edit.EditableMath exposing (..)

import Mathquelm.Edit.Command as Command exposing (Block, Command)
import Mathquelm.Edit.MathWithCursor as Cursor exposing (MathWithCursor)
import Mathquelm.Edit.MathWithSelection as Selection exposing (MathWithSelection)
import Mathquelm.Edit.TreeWithBlockHole as TreeWithBlockHole exposing (TreeWithBlockHole)
import Mathquelm.ListZipper as ListZipper exposing (ListZipper, getAllAfter, getAllBefore)
import Mathquelm.Math as Math exposing (Math)
import Mathquelm.Util exposing (..)


-- Movement Actions {{{


goLeft : MathBeingEdited -> Maybe MathBeingEdited
goLeft mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            Cursor.enterCommandToLeft mathWithCursor
                |> orElse (Cursor.jumpCommandToLeft mathWithCursor)
                |> orElse (Cursor.exitBlockLeftward mathWithCursor)
                |> Maybe.map Cursor

        Selection mathWithSelection ->
            Just (Cursor (placeCursorAtLeftOfSelection mathWithSelection))


goRight : MathBeingEdited -> Maybe MathBeingEdited
goRight mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            Cursor.enterCommandToRight mathWithCursor
                |> orElse (Cursor.jumpCommandToRight mathWithCursor)
                |> orElse (Cursor.exitBlockRightward mathWithCursor)
                |> Maybe.map Cursor

        Selection mathWithSelection ->
            Just (Cursor (placeCursorAtRightOfSelection mathWithSelection))


goUp : MathBeingEdited -> Maybe MathBeingEdited
goUp mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            Cursor.enterTopOfCommandToRight mathWithCursor
                |> orElse (Cursor.enterTopOfCommandToLeft mathWithCursor)
                |> orElse (Cursor.exitBlockUpward mathWithCursor)
                |> Maybe.map Cursor

        _ ->
            Nothing


goDown : MathBeingEdited -> Maybe MathBeingEdited
goDown mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            Cursor.enterBottomOfCommandToRight mathWithCursor
                |> orElse (Cursor.enterBottomOfCommandToLeft mathWithCursor)
                |> orElse (Cursor.exitBlockDownward mathWithCursor)
                |> Maybe.map Cursor

        _ ->
            Nothing



-- }}}
-- Deletion Actions {{{


deleteLeft : MathBeingEdited -> Maybe MathBeingEdited
deleteLeft mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            Cursor.deleteBefore mathWithCursor
                |> orElse (Cursor.deleteParentCommand mathWithCursor)
                |> Maybe.map Cursor

        Selection mathWithSelection ->
            Just (Cursor (deleteInsideSelection mathWithSelection))


deleteRight : MathBeingEdited -> Maybe MathBeingEdited
deleteRight mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            Cursor.deleteAfter mathWithCursor
                |> orElse (Cursor.deleteParentCommand mathWithCursor)
                |> Maybe.map Cursor

        Selection mathWithSelection ->
            Just (Cursor (deleteInsideSelection mathWithSelection))



-- }}}
-- Selection Actions {{{


selectRight : MathBeingEdited -> Maybe MathBeingEdited
selectRight mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            turnCursorIntoSelection Selection.Right mathWithCursor
                |> Selection
                |> selectRight

        Selection mathWithSelection ->
            Selection.moveEdgeOfSelectionRightward mathWithSelection
                |> orElse (Selection.selectWholeBlock mathWithSelection)
                |> Maybe.map normalizeSelection


selectLeft : MathBeingEdited -> Maybe MathBeingEdited
selectLeft mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            turnCursorIntoSelection Selection.Left mathWithCursor
                |> Selection
                |> selectLeft

        Selection mathWithSelection ->
            Selection.moveEdgeOfSelectionLeftward mathWithSelection
                |> orElse (Selection.selectWholeBlock mathWithSelection)
                |> Maybe.map normalizeSelection


selectUp : MathBeingEdited -> Maybe MathBeingEdited
selectUp mathBeingEdited =
    Nothing


selectDown : MathBeingEdited -> Maybe MathBeingEdited
selectDown mathBeingEdited =
    Nothing


selectAll : MathBeingEdited -> Maybe MathBeingEdited
selectAll mathBeingEdited =
    stopEditing mathBeingEdited
        |> Selection.makeSelectionFromBlock
        |> Selection
        |> Just


unselect : MathBeingEdited -> Maybe MathBeingEdited
unselect mathBeingEdited =
    case mathBeingEdited of
        Selection mathWithSelection ->
            Just <|
                Cursor <|
                    case Selection.getDirection mathWithSelection of
                        Selection.Left ->
                            placeCursorAtLeftOfSelection mathWithSelection

                        Selection.Right ->
                            placeCursorAtRightOfSelection mathWithSelection

        _ ->
            Nothing


normalizeSelection : MathWithSelection -> MathBeingEdited
normalizeSelection mathWithSelection =
    case Selection.getSelectedBlock mathWithSelection of
        [] ->
            Cursor (deleteInsideSelection mathWithSelection)

        _ ->
            Selection mathWithSelection


turnCursorIntoSelection : Selection.LeftRight -> MathWithCursor -> MathWithSelection
turnCursorIntoSelection direction ( cursorBlock, restOfTree ) =
    ( { restOfBlock = cursorBlock
      , selected = []
      , direction = direction
      , innerSelection = Selection.None
      }
    , restOfTree
    )


placeCursorAtLeftOfSelection : MathWithSelection -> MathWithCursor
placeCursorAtLeftOfSelection ( { selected, restOfBlock }, restOfTree ) =
    ( ListZipper.insertAfter selected restOfBlock, restOfTree )


placeCursorAtRightOfSelection : MathWithSelection -> MathWithCursor
placeCursorAtRightOfSelection ( { selected, restOfBlock }, restOfTree ) =
    ( ListZipper.insertBefore selected restOfBlock, restOfTree )


deleteInsideSelection : MathWithSelection -> MathWithCursor
deleteInsideSelection ( selectionBlock, restOfTree ) =
    ( selectionBlock.restOfBlock
    , restOfTree
    )



-- }}}
-- MathBeingEdited {{{


type MathBeingEdited
    = Cursor MathWithCursor
    | Selection MathWithSelection


startEditing : Block -> MathBeingEdited
startEditing block =
    Cursor ( Cursor.placeCursorOnLeft block, [] )


stopEditing : MathBeingEdited -> Block
stopEditing mathBeingEdited =
    List.foldl
        TreeWithBlockHole.fillBlockHole
        (getBlockBeingEdited mathBeingEdited)
        (getRestOfTree mathBeingEdited)


toMath : MathBeingEdited -> Math.Math
toMath mathBeingEdited =
    stopEditing mathBeingEdited
        |> Command.blockToMath


getBlockBeingEdited : MathBeingEdited -> Block
getBlockBeingEdited mathBeingEdited =
    case mathBeingEdited of
        Cursor ( cursorBlock, _ ) ->
            Cursor.removeCursor cursorBlock

        Selection ( selectionBlock, _ ) ->
            Selection.removeSelection selectionBlock


getRestOfTree : MathBeingEdited -> TreeWithBlockHole
getRestOfTree mathBeingEdited =
    case mathBeingEdited of
        Cursor ( _, restOfTree ) ->
            restOfTree

        Selection ( _, restOfTree ) ->
            restOfTree



-- }}}
