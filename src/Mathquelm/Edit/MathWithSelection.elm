module Mathquelm.Edit.MathWithSelection exposing (..)

import Mathquelm.Edit.Command exposing (Block, Command)
import Mathquelm.Edit.TreeWithBlockHole as TreeWithBlockHole exposing (TreeWithBlockHole, fillCommandHole)
import Mathquelm.ListZipper as ListZipper exposing (ListZipper, getAllAfter, getAllBefore)
import Mathquelm.Util exposing (..)


type alias MathWithSelection =
    ( BlockWithSelection, TreeWithBlockHole )


type LeftRight
    = Left
    | Right


type alias BlockWithSelection =
    { restOfBlock : ListZipper Command
    , selected : Block
    , innerSelection : InnerSelection
    , direction : LeftRight
    }



-- TODO: Is there a way to do this with a recursive thing -- essentially, have one previous selection?


type InnerSelection
    = InnerSelection MathWithSelection
    | None


restoreToInnerSelection : MathWithSelection -> Maybe MathWithSelection
restoreToInnerSelection ( selectionBlock, restOfTree ) =
    case selectionBlock.innerSelection of
        InnerSelection inner ->
            Just inner

        None ->
            Nothing


getDirection : MathWithSelection -> LeftRight
getDirection ( blockSelection, _ ) =
    blockSelection.direction


getSelectedBlock : MathWithSelection -> Block
getSelectedBlock ( blockSelection, _ ) =
    blockSelection.selected


getRestOfBlock : MathWithSelection -> ListZipper Command
getRestOfBlock ( blockSelection, _ ) =
    blockSelection.restOfBlock


updateRestOfBlock :
    (ListZipper Command -> ListZipper Command)
    -> MathWithSelection
    -> MathWithSelection
updateRestOfBlock fn ( blockSelection, restOfTree ) =
    ( { blockSelection
        | restOfBlock = fn blockSelection.restOfBlock
      }
    , restOfTree
    )


setSelectedBlock : Block -> MathWithSelection -> MathWithSelection
setSelectedBlock newSelected ( blockSelection, restOfTree ) =
    ( { blockSelection | selected = newSelected }, restOfTree )


insertBeforeSelection : Command -> MathWithSelection -> MathWithSelection
insertBeforeSelection toInsert mathWithSelection =
    updateRestOfBlock (ListZipper.insertBefore [ toInsert ]) mathWithSelection


insertAfterSelection : Command -> MathWithSelection -> MathWithSelection
insertAfterSelection toInsert mathWithSelection =
    updateRestOfBlock (ListZipper.insertAfter [ toInsert ]) mathWithSelection


getAfterSelection : MathWithSelection -> Maybe Command
getAfterSelection ( blockSelection, _ ) =
    ListZipper.getAfter blockSelection.restOfBlock


getBeforeSelection : MathWithSelection -> Maybe Command
getBeforeSelection ( blockSelection, _ ) =
    ListZipper.getBefore blockSelection.restOfBlock


removeCommandAfterSelection : MathWithSelection -> MathWithSelection
removeCommandAfterSelection mathWithSelection =
    updateRestOfBlock ListZipper.removeAfter mathWithSelection


removeCommandBeforeSelection : MathWithSelection -> MathWithSelection
removeCommandBeforeSelection mathWithSelection =
    updateRestOfBlock ListZipper.removeBefore mathWithSelection


addToEndOfSelection : Command -> MathWithSelection -> MathWithSelection
addToEndOfSelection toAdd mathWithSelection =
    setSelectedBlock
        (getSelectedBlock mathWithSelection ++ [ toAdd ])
        mathWithSelection


addToStartOfSelection : Command -> MathWithSelection -> MathWithSelection
addToStartOfSelection toAdd mathWithSelection =
    setSelectedBlock
        (toAdd :: getSelectedBlock mathWithSelection)
        mathWithSelection


removeSelection : BlockWithSelection -> Block
removeSelection selectionBlock =
    selectionBlock.restOfBlock
        |> ListZipper.insertAfter selectionBlock.selected
        |> ListZipper.toList


moveEdgeOfSelectionRightward : MathWithSelection -> Maybe MathWithSelection
moveEdgeOfSelectionRightward mathWithSelection =
    case getDirection mathWithSelection of
        Left ->
            case getSelectedBlock mathWithSelection of
                leftmost :: [] ->
                    restoreToInnerSelection mathWithSelection
                        |> orElse
                            (--TODO THIS IS REPETITIVE, GIVE A VERB?
                             Just
                                (setSelectedBlock [] mathWithSelection
                                    |> insertBeforeSelection leftmost
                                )
                            )

                leftmost :: remainingSelection ->
                    Just
                        (setSelectedBlock remainingSelection mathWithSelection
                            |> insertBeforeSelection leftmost
                        )

                _ ->
                    Nothing

        Right ->
            case getAfterSelection mathWithSelection of
                Just next ->
                    Just
                        (removeCommandAfterSelection mathWithSelection
                            |> addToEndOfSelection next
                        )

                _ ->
                    Nothing


moveEdgeOfSelectionLeftward : MathWithSelection -> Maybe MathWithSelection
moveEdgeOfSelectionLeftward mathWithSelection =
    case getDirection mathWithSelection of
        Right ->
            case List.reverse (getSelectedBlock mathWithSelection) of
                rightmost :: [] ->
                    restoreToInnerSelection mathWithSelection
                        |> orElse
                            (--TODO THIS IS REPETITIVE, GIVE A VERB?
                             Just
                                (setSelectedBlock [] mathWithSelection
                                    |> insertAfterSelection rightmost
                                )
                            )

                rightmost :: remainingSelectionReversed ->
                    Just
                        (setSelectedBlock
                            (List.reverse remainingSelectionReversed)
                            mathWithSelection
                            |> insertAfterSelection rightmost
                        )

                _ ->
                    Nothing

        Left ->
            case getBeforeSelection mathWithSelection of
                Just next ->
                    Just
                        (removeCommandBeforeSelection mathWithSelection
                            |> addToStartOfSelection next
                        )

                _ ->
                    Nothing



--TODO make this more idiomatic


selectWholeBlock : MathWithSelection -> Maybe MathWithSelection
selectWholeBlock ( selectionBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            Just
                ( { restOfBlock = parentBlockWithHole.restOfBlock
                  , selected =
                        [ fillCommandHole
                            parentBlockWithHole.commandWithBlockHole
                            (removeSelection selectionBlock)
                        ]
                  , direction = selectionBlock.direction
                  , innerSelection = InnerSelection ( selectionBlock, restOfTree )
                  }
                , grandparents
                )

        _ ->
            Nothing
