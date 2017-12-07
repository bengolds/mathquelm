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


exitCurrentBlockLeftward : MathWithCursor -> Maybe MathWithCursor
exitCurrentBlockLeftward ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithBotHole top) ->
            moveCursorToTopOfFraction ( cursorBlock, restOfTree )

        Just _ ->
            exitCurrentCommandLeftward ( cursorBlock, restOfTree )

        Nothing ->
            Nothing


exitCurrentBlockRightward : MathWithCursor -> Maybe MathWithCursor
exitCurrentBlockRightward ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithTopHole bottom) ->
            moveCursorToBottomOfFraction ( cursorBlock, restOfTree )

        Just _ ->
            exitCurrentCommandRightward ( cursorBlock, restOfTree )

        _ ->
            Nothing


exitCurrentBlockUpward : MathWithCursor -> Maybe MathWithCursor
exitCurrentBlockUpward ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithBotHole top) ->
            moveCursorToTopOfFraction ( cursorBlock, restOfTree )

        Just _ ->
            exitCurrentCommandLeftward ( cursorBlock, restOfTree )
                |> Maybe.andThen exitCurrentBlockUpward

        Nothing ->
            Nothing


exitCurrentBlockDownward : MathWithCursor -> Maybe MathWithCursor
exitCurrentBlockDownward ( cursorBlock, restOfTree ) =
    case getCommandBeingEdited restOfTree of
        Just (DivWithTopHole bot) ->
            moveCursorToBottomOfFraction ( cursorBlock, restOfTree )

        Just _ ->
            exitCurrentCommandLeftward ( cursorBlock, restOfTree )
                |> Maybe.andThen exitCurrentBlockDownward

        Nothing ->
            Nothing


exitCurrentCommandLeftward : MathWithCursor -> Maybe MathWithCursor
exitCurrentCommandLeftward ( cursorBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            Just
                (( placeCursorAtHole parentBlockWithHole
                 , grandparents
                 )
                    |> insertRightOfCursor
                        (fillCommandHole
                            parentBlockWithHole.commandWithBlockHole
                            (removeCursor cursorBlock)
                        )
                )

        _ ->
            Nothing


exitCurrentCommandRightward : MathWithCursor -> Maybe MathWithCursor
exitCurrentCommandRightward ( cursorBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            Just
                (( placeCursorAtHole parentBlockWithHole
                 , grandparents
                 )
                    |> insertLeftOfCursor
                        (fillCommandHole
                            parentBlockWithHole.commandWithBlockHole
                            (removeCursor cursorBlock)
                        )
                )

        _ ->
            Nothing


moveCursorToBottomOfFraction : MathWithCursor -> Maybe MathWithCursor
moveCursorToBottomOfFraction ( cursorBlock, restOfTree ) =
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


moveCursorToTopOfFraction : MathWithCursor -> Maybe MathWithCursor
moveCursorToTopOfFraction ( cursorBlock, restOfTree ) =
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
-- Insertion with Cursor {{{


insertRightOfCursor : Command -> MathWithCursor -> MathWithCursor
insertRightOfCursor command ( cursorBlock, restOfTree ) =
    ( ListZipper.insertAfter [ command ] cursorBlock, restOfTree )


insertLeftOfCursor : Command -> MathWithCursor -> MathWithCursor
insertLeftOfCursor command ( cursorBlock, restOfTree ) =
    ( ListZipper.insertBefore [ command ] cursorBlock, restOfTree )



-- }}}
-- Deletion with Cursor {{{


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


deleteLeftOfCursor : MathWithCursor -> Maybe MathWithCursor
deleteLeftOfCursor ( cursorBlock, restOfTree ) =
    case ListZipper.getBefore cursorBlock of
        Just (Cos operands) ->
            enterCommandToLeft ( cursorBlock, restOfTree )

        Just (Div top bottom) ->
            enterBottomOfCommandToLeft ( cursorBlock, restOfTree )

        Just _ ->
            Just ( ListZipper.removeBefore cursorBlock, restOfTree )

        Nothing ->
            Nothing


deleteRightOfCursor : MathWithCursor -> Maybe MathWithCursor
deleteRightOfCursor ( cursorBlock, restOfTree ) =
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
