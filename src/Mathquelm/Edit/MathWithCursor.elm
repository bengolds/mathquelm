module Mathquelm.Edit.MathWithCursor exposing (..)

import Mathquelm.Edit.Command exposing (Block, Command(..))
import Mathquelm.Edit.TreeWithBlockHole as TreeWithBlockHole
    exposing
        ( BlockWithBlockHole
        , CommandWithBlockHole(..)
        , TreeWithBlockHole
        , fillCommandHole
        , getCommandBeingEdited
        )
import Mathquelm.ListZipper as ListZipper exposing (ListZipper)


type alias MathWithCursor =
    ( BlockWithCursor, TreeWithBlockHole )


type alias BlockWithCursor =
    ListZipper Command



-- Cursor Movement {{{


enterCommandToLeft : MathWithCursor -> Maybe MathWithCursor
enterCommandToLeft ( cursorBlock, restOfTree ) =
    case ListZipper.getBefore cursorBlock of
        Just (Cos blockToEnter) ->
            Just
                ( placeCursorOnRight blockToEnter
                , TreeWithBlockHole.push
                    (BlockWithBlockHole
                        (ListZipper.removeBefore cursorBlock)
                        CosWithHole
                    )
                    restOfTree
                )

        Just (Div top bot) ->
            Just
                ( placeCursorOnRight bot
                , TreeWithBlockHole.push
                    (BlockWithBlockHole
                        (ListZipper.removeBefore cursorBlock)
                        (DivWithBotHole top)
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterCommandToRight : MathWithCursor -> Maybe MathWithCursor
enterCommandToRight ( cursorBlock, restOfTree ) =
    case ListZipper.getAfter cursorBlock of
        Just (Cos blockToEnter) ->
            Just
                ( placeCursorOnLeft blockToEnter
                , TreeWithBlockHole.push
                    (BlockWithBlockHole
                        (ListZipper.removeAfter cursorBlock)
                        CosWithHole
                    )
                    restOfTree
                )

        Just (Div top bot) ->
            Just
                ( placeCursorOnLeft top
                , TreeWithBlockHole.push
                    (BlockWithBlockHole
                        (ListZipper.removeAfter cursorBlock)
                        (DivWithTopHole bot)
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterTopOfCommandToLeft : MathWithCursor -> Maybe MathWithCursor
enterTopOfCommandToLeft ( cursorBlock, restOfTree ) =
    case ListZipper.getBefore cursorBlock of
        Just (Div top bot) ->
            Just
                ( placeCursorOnRight top
                , TreeWithBlockHole.push
                    (BlockWithBlockHole
                        (ListZipper.removeBefore cursorBlock)
                        (DivWithTopHole bot)
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterTopOfCommandToRight : MathWithCursor -> Maybe MathWithCursor
enterTopOfCommandToRight ( cursorBlock, restOfTree ) =
    case ListZipper.getAfter cursorBlock of
        Just (Div top bot) ->
            Just
                ( placeCursorOnLeft top
                , TreeWithBlockHole.push
                    (BlockWithBlockHole
                        (ListZipper.removeAfter cursorBlock)
                        (DivWithTopHole bot)
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterBottomOfCommandToLeft : MathWithCursor -> Maybe MathWithCursor
enterBottomOfCommandToLeft ( cursorBlock, restOfTree ) =
    case ListZipper.getBefore cursorBlock of
        Just (Div top bot) ->
            Just
                ( placeCursorOnRight bot
                , TreeWithBlockHole.push
                    (BlockWithBlockHole
                        (ListZipper.removeBefore cursorBlock)
                        (DivWithBotHole top)
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterBottomOfCommandToRight : MathWithCursor -> Maybe MathWithCursor
enterBottomOfCommandToRight ( cursorBlock, restOfTree ) =
    case ListZipper.getAfter cursorBlock of
        Just (Div top bot) ->
            Just
                ( placeCursorOnLeft bot
                , TreeWithBlockHole.push
                    (BlockWithBlockHole
                        (ListZipper.removeAfter cursorBlock)
                        (DivWithBotHole top)
                    )
                    restOfTree
                )

        _ ->
            Nothing


jumpCommandToLeft : MathWithCursor -> Maybe MathWithCursor
jumpCommandToLeft ( cursorBlock, restOfTree ) =
    if not (ListZipper.isAtStart cursorBlock) then
        Just ( ListZipper.goLeft cursorBlock, restOfTree )
    else
        Nothing


jumpCommandToRight : MathWithCursor -> Maybe MathWithCursor
jumpCommandToRight ( cursorBlock, restOfTree ) =
    if not (ListZipper.isAtEnd cursorBlock) then
        Just ( ListZipper.goRight cursorBlock, restOfTree )
    else
        Nothing


exitBlockLeftward : MathWithCursor -> Maybe MathWithCursor
exitBlockLeftward ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithBotHole top) ->
            moveToTopOfFraction ( cursorBlock, restOfTree )

        Just _ ->
            exitCommandLeftward ( cursorBlock, restOfTree )

        Nothing ->
            Nothing


exitBlockRightward : MathWithCursor -> Maybe MathWithCursor
exitBlockRightward ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithTopHole bottom) ->
            moveToBottomOfFraction ( cursorBlock, restOfTree )

        Just _ ->
            exitCommandRightward ( cursorBlock, restOfTree )

        _ ->
            Nothing


exitBlockUpward : MathWithCursor -> Maybe MathWithCursor
exitBlockUpward ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithBotHole top) ->
            moveToTopOfFraction ( cursorBlock, restOfTree )

        Just _ ->
            exitCommandLeftward ( cursorBlock, restOfTree )
                |> Maybe.andThen exitBlockUpward

        Nothing ->
            Nothing


exitBlockDownward : MathWithCursor -> Maybe MathWithCursor
exitBlockDownward ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithTopHole bot) ->
            moveToBottomOfFraction ( cursorBlock, restOfTree )

        Just _ ->
            exitCommandLeftward ( cursorBlock, restOfTree )
                |> Maybe.andThen exitBlockDownward

        Nothing ->
            Nothing


exitCommandLeftward : MathWithCursor -> Maybe MathWithCursor
exitCommandLeftward ( cursorBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            Just
                (( placeCursorAtHole parentBlockWithHole
                 , grandparents
                 )
                    |> insertAfter
                        (fillCommandHole
                            parentBlockWithHole.commandWithBlockHole
                            (removeCursor cursorBlock)
                        )
                )

        _ ->
            Nothing


exitCommandRightward : MathWithCursor -> Maybe MathWithCursor
exitCommandRightward ( cursorBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            Just
                (( placeCursorAtHole parentBlockWithHole
                 , grandparents
                 )
                    |> insertBefore
                        (fillCommandHole
                            parentBlockWithHole.commandWithBlockHole
                            (removeCursor cursorBlock)
                        )
                )

        _ ->
            Nothing


moveToBottomOfFraction : MathWithCursor -> Maybe MathWithCursor
moveToBottomOfFraction ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithTopHole bot) ->
            Just
                (( cursorBlock, restOfTree )
                    |> setCursorBlock (placeCursorOnLeft bot)
                    |> setCommandBeingEdited
                        (DivWithBotHole (removeCursor cursorBlock))
                )

        _ ->
            Nothing


moveToTopOfFraction : MathWithCursor -> Maybe MathWithCursor
moveToTopOfFraction ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithBotHole top) ->
            Just
                (( cursorBlock, restOfTree )
                    |> setCursorBlock (placeCursorOnRight top)
                    |> setCommandBeingEdited
                        (DivWithTopHole (removeCursor cursorBlock))
                )

        _ ->
            Nothing



-- }}}


insertAfter : Command -> MathWithCursor -> MathWithCursor
insertAfter command ( cursorBlock, restOfTree ) =
    ( ListZipper.insertAfter [ command ] cursorBlock, restOfTree )


insertBefore : Command -> MathWithCursor -> MathWithCursor
insertBefore command ( cursorBlock, restOfTree ) =
    ( ListZipper.insertBefore [ command ] cursorBlock, restOfTree )


deleteParentCommand : MathWithCursor -> Maybe MathWithCursor
deleteParentCommand ( cursorBlock, restOfTree ) =
    --TODO I Think this can be clearer
    case restOfTree of
        parentBlockWithHole :: xs ->
            let
                reassembledBlockWithCursor =
                    case parentBlockWithHole.commandWithBlockHole of
                        --TODO Make this better explained
                        CosWithHole ->
                            parentBlockWithHole.restOfBlock
                                |> ListZipper.nest cursorBlock

                        DivWithTopHole bot ->
                            parentBlockWithHole.restOfBlock
                                |> ListZipper.insertAfter bot
                                |> ListZipper.nest cursorBlock

                        DivWithBotHole top ->
                            parentBlockWithHole.restOfBlock
                                |> ListZipper.insertBefore top
                                |> ListZipper.nest cursorBlock
            in
            Just ( reassembledBlockWithCursor, xs )

        [] ->
            Nothing


deleteBefore : MathWithCursor -> Maybe MathWithCursor
deleteBefore ( cursorBlock, restOfTree ) =
    case ListZipper.getBefore cursorBlock of
        Just (Cos operands) ->
            enterCommandToLeft ( cursorBlock, restOfTree )

        Just (Div top bottom) ->
            enterBottomOfCommandToLeft ( cursorBlock, restOfTree )

        Just _ ->
            Just ( ListZipper.removeBefore cursorBlock, restOfTree )

        Nothing ->
            Nothing


deleteAfter : MathWithCursor -> Maybe MathWithCursor
deleteAfter ( cursorBlock, restOfTree ) =
    case ListZipper.getAfter cursorBlock of
        Just (Cos operands) ->
            Just ( ListZipper.insertAfter operands cursorBlock, restOfTree )

        Just (Div top bottom) ->
            enterTopOfCommandToRight ( cursorBlock, restOfTree )

        Just _ ->
            Just ( ListZipper.removeAfter cursorBlock, restOfTree )

        Nothing ->
            Nothing



-- }}}


setCursorBlock : BlockWithCursor -> MathWithCursor -> MathWithCursor
setCursorBlock newCursorBlock ( _, restOfTree ) =
    ( newCursorBlock, restOfTree )


setCommandBeingEdited : CommandWithBlockHole -> MathWithCursor -> MathWithCursor
setCommandBeingEdited newCommand ( cursorBlock, restOfTree ) =
    case restOfTree of
        parent :: grandparents ->
            ( cursorBlock
            , TreeWithBlockHole.changeCommand newCommand parent :: grandparents
            )

        [] ->
            ( cursorBlock
            , [ { restOfBlock = ListZipper.empty
                , commandWithBlockHole = newCommand
                }
              ]
            )


removeCursor : BlockWithCursor -> Block
removeCursor cursorBlock =
    ListZipper.toList cursorBlock


placeCursorOnRight : Block -> BlockWithCursor
placeCursorOnRight =
    ListZipper.fromListEnd


placeCursorOnLeft : Block -> BlockWithCursor
placeCursorOnLeft =
    ListZipper.fromList


placeCursorAtHole : BlockWithBlockHole -> BlockWithCursor
placeCursorAtHole blockWithBlockHole =
    blockWithBlockHole.restOfBlock
