module Mathquelm.EditableMath exposing (..)

import Mathquelm.Digit as Digit exposing (Digit)
import Mathquelm.ListZipper as ListZipper exposing (ListZipper, getAllAfter, getAllBefore)
import Mathquelm.Math as Math exposing (Math)


-- Movement Actions {{{


goLeft : MathBeingEdited -> Maybe MathBeingEdited
goLeft mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            enterCommandToLeft mathWithCursor
                |> orElse (jumpCommandToLeft mathWithCursor)
                |> orElse (exitCurrentBlockLeftward mathWithCursor)
                |> Maybe.map Cursor

        _ ->
            Nothing


goRight : MathBeingEdited -> Maybe MathBeingEdited
goRight mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            enterCommandToRight mathWithCursor
                |> orElse (jumpCommandToRight mathWithCursor)
                |> orElse (exitCurrentBlockRightward mathWithCursor)
                |> Maybe.map Cursor

        _ ->
            Nothing


goUp : MathBeingEdited -> Maybe MathBeingEdited
goUp mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            enterTopOfCommandToRight mathWithCursor
                |> orElse (enterTopOfCommandToLeft mathWithCursor)
                |> orElse (exitCurrentBlockUpward mathWithCursor)
                |> Maybe.map Cursor

        _ ->
            Nothing


goDown : MathBeingEdited -> Maybe MathBeingEdited
goDown mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            enterBottomOfCommandToRight mathWithCursor
                |> orElse (enterBottomOfCommandToLeft mathWithCursor)
                |> orElse (exitCurrentBlockDownward mathWithCursor)
                |> Maybe.map Cursor

        _ ->
            Nothing



-- }}}
-- Deletion Actions {{{


deleteLeft : MathBeingEdited -> Maybe MathBeingEdited
deleteLeft mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            deleteLeftOfCursor mathWithCursor
                |> orElse (deleteParentCommand mathWithCursor)
                |> Maybe.map Cursor

        Selection mathWithSelection ->
            Just (Cursor (deleteInsideSelection mathWithSelection))


deleteRight : MathBeingEdited -> Maybe MathBeingEdited
deleteRight mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            deleteRightOfCursor mathWithCursor
                |> orElse (deleteParentCommand mathWithCursor)
                |> Maybe.map Cursor

        Selection mathWithSelection ->
            Just (Cursor (deleteInsideSelection mathWithSelection))



-- }}}
-- Selection Actions {{{


selectRight : MathBeingEdited -> Maybe MathBeingEdited
selectRight mathBeingEdited =
    case mathBeingEdited of
        Cursor mathWithCursor ->
            turnCursorIntoSelection Right mathWithCursor
                |> Selection
                |> selectRight

        Selection mathWithSelection ->
            --WHAT DO I DOOO
            --You can either exit the block
            --Or extend the block if it's a right selection
            --Or shrink the block if it's a left selection
            Nothing


selectLeft : MathBeingEdited -> Maybe MathBeingEdited
selectLeft mathBeingEdited =
    Nothing


selectUp : MathBeingEdited -> Maybe MathBeingEdited
selectUp mathBeingEdited =
    Nothing


selectDown : MathBeingEdited -> Maybe MathBeingEdited
selectDown mathBeingEdited =
    Nothing



-- }}}
-- Commands and Blocks {{{


type Command
    = Var String
    | Digit Digit
    | Div Block Block
    | Cos Block
    | Plus


type alias Block =
    List Command


blockToMath : Block -> Math
blockToMath block =
    List.foldl
        insertRight
        Math.Empty
        block


insertRight : Command -> Math -> Math
insertRight cmd math =
    case cmd of
        Digit digit ->
            let
                asInt =
                    Digit.parse digit

                replaceOrAppend rightmost =
                    case rightmost of
                        Math.Empty ->
                            Just (Math.Num asInt)

                        Math.Num num ->
                            Just (Math.Num (num * 10 + asInt))

                        _ ->
                            Nothing
            in
            updateRightMost replaceOrAppend math
                |> Maybe.withDefault (multiplyBelowPluses (Math.Num asInt) math)

        Var name ->
            appendRight (Math.Var name) math

        Div top bot ->
            appendRight (Math.Div (blockToMath top) (blockToMath bot)) math

        Cos x ->
            appendRight (Math.Cos (blockToMath x)) math

        Plus ->
            Math.Plus math Math.Empty


multiplyBelowPluses : Math -> Math -> Math
multiplyBelowPluses toMultiply math =
    case math of
        Math.Plus left right ->
            Math.Plus left (multiplyBelowPluses toMultiply right)

        _ ->
            Math.Mul math toMultiply


updateRightMost : (Math -> Maybe Math) -> Math -> Maybe Math
updateRightMost fn tree =
    case tree of
        Math.Empty ->
            fn Math.Empty

        Math.Num val ->
            fn (Math.Num val)

        Math.Var name ->
            fn (Math.Var name)

        Math.Mul left right ->
            updateRightMost fn right
                |> Maybe.map (Math.Mul left)

        Math.Plus left right ->
            updateRightMost fn right
                |> Maybe.map (Math.Plus left)

        _ ->
            Nothing


appendRight : Math -> Math -> Math
appendRight toAppend math =
    let
        replaceEmpty math =
            case math of
                Math.Empty ->
                    Just toAppend

                _ ->
                    Nothing
    in
    updateRightMost replaceEmpty math
        |> Maybe.withDefault (multiplyBelowPluses toAppend math)



-- }}}
-- MathBeingEdited {{{


type MathBeingEdited
    = Cursor MathWithCursor
    | Selection MathWithSelection


startEditing : Block -> MathBeingEdited
startEditing block =
    Cursor ( placeCursorOnLeft block, [] )


stopEditing : MathBeingEdited -> Block
stopEditing mathBeingEdited =
    List.foldl
        fillBlockHole
        (getBlockBeingEdited mathBeingEdited)
        (getRestOfTree mathBeingEdited)


toMath : MathBeingEdited -> Math.Math
toMath mathBeingEdited =
    stopEditing mathBeingEdited
        |> blockToMath


getBlockBeingEdited : MathBeingEdited -> Block
getBlockBeingEdited mathBeingEdited =
    case mathBeingEdited of
        Cursor ( cursorBlock, _ ) ->
            removeCursor cursorBlock

        Selection ( selectionBlock, _ ) ->
            removeSelection selectionBlock


getRestOfTree : MathBeingEdited -> TreeWithBlockHole
getRestOfTree mathBeingEdited =
    case mathBeingEdited of
        Cursor ( _, restOfTree ) ->
            restOfTree

        Selection ( _, restOfTree ) ->
            restOfTree



-- }}}
-- Cursor editing  {{{


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
                , push
                    (BlockWithBlockHole
                        (ListZipper.removeBefore cursorBlock)
                        CosWithHole
                    )
                    restOfTree
                )

        Just (Div top bot) ->
            Just
                ( placeCursorOnRight bot
                , push
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
                , push
                    (BlockWithBlockHole
                        (ListZipper.removeAfter cursorBlock)
                        CosWithHole
                    )
                    restOfTree
                )

        Just (Div top bot) ->
            Just
                ( placeCursorOnLeft top
                , push
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
                , push
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
                , push
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
                , push
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
                , push
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
                ( placeCursorAtHole parentBlockWithHole
                    |> insertRightOfCursor
                        (fillCommandHole
                            parentBlockWithHole.commandWithBlockHole
                            (removeCursor cursorBlock)
                        )
                , grandparents
                )

        _ ->
            Nothing


exitCurrentCommandRightward : MathWithCursor -> Maybe MathWithCursor
exitCurrentCommandRightward ( cursorBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            Just
                ( placeCursorAtHole parentBlockWithHole
                    |> insertLeftOfCursor
                        (fillCommandHole
                            parentBlockWithHole.commandWithBlockHole
                            (removeCursor cursorBlock)
                        )
                , grandparents
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


insertRightOfCursor : Command -> BlockWithCursor -> BlockWithCursor
insertRightOfCursor command cursorBlock =
    ListZipper.insertAfter [ command ] cursorBlock


insertLeftOfCursor : Command -> BlockWithCursor -> BlockWithCursor
insertLeftOfCursor command cursorBlock =
    ListZipper.insertBefore [ command ] cursorBlock



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
            , changeCommand newCommand parent :: grandparents
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


turnCursorIntoSelection : LeftRight -> MathWithCursor -> MathWithSelection
turnCursorIntoSelection direction ( cursorBlock, restOfTree ) =
    ( { restOfBlock = cursorBlock
      , selected = []
      , direction = direction
      }
    , restOfTree
    )


placeCursorOnRight : Block -> BlockWithCursor
placeCursorOnRight =
    ListZipper.fromListEnd


placeCursorOnLeft : Block -> BlockWithCursor
placeCursorOnLeft =
    ListZipper.fromList



-- }}}
-- MathWithSelection {{{


type alias MathWithSelection =
    ( BlockWithSelection, TreeWithBlockHole )


type LeftRight
    = Left
    | Right


type alias BlockWithSelection =
    { restOfBlock : ListZipper Command
    , selected : Block
    , direction : LeftRight
    }


removeSelection : BlockWithSelection -> Block
removeSelection selectionBlock =
    selectionBlock.restOfBlock
        |> ListZipper.insertAfter selectionBlock.selected
        |> ListZipper.toList


deleteInsideSelection : MathWithSelection -> MathWithCursor
deleteInsideSelection ( selectionBlock, restOfTree ) =
    ( selectionBlock.restOfBlock
    , restOfTree
    )



-- }}}
-- TreeWithBlockHole {{{


type alias TreeWithBlockHole =
    List BlockWithBlockHole


type CommandWithBlockHole
    = CosWithHole
    | DivWithTopHole Block
    | DivWithBotHole Block


type alias BlockWithBlockHole =
    { restOfBlock : ListZipper Command
    , commandWithBlockHole : CommandWithBlockHole
    }


fillBlockHole : BlockWithBlockHole -> Block -> Block
fillBlockHole blockWithBlockHole fillerBlock =
    blockWithBlockHole.restOfBlock
        |> ListZipper.insertBefore
            [ fillCommandHole
                blockWithBlockHole.commandWithBlockHole
                fillerBlock
            ]
        |> ListZipper.toList


placeCursorAtHole : BlockWithBlockHole -> BlockWithCursor
placeCursorAtHole blockWithBlockHole =
    blockWithBlockHole.restOfBlock


fillCommandHole : CommandWithBlockHole -> Block -> Command
fillCommandHole commandWithBlockHole fillerBlock =
    case commandWithBlockHole of
        CosWithHole ->
            Cos fillerBlock

        DivWithTopHole bot ->
            Div fillerBlock bot

        DivWithBotHole top ->
            Div top fillerBlock


getCommandBeingEdited : TreeWithBlockHole -> Maybe CommandWithBlockHole
getCommandBeingEdited restOfTree =
    case restOfTree of
        [] ->
            Nothing

        parentBlockWithHole :: _ ->
            Just parentBlockWithHole.commandWithBlockHole


changeCommand : CommandWithBlockHole -> BlockWithBlockHole -> BlockWithBlockHole
changeCommand newCommand blockWithHole =
    { blockWithHole | commandWithBlockHole = newCommand }


push : BlockWithBlockHole -> TreeWithBlockHole -> TreeWithBlockHole
push blockWithBlockHole restOfTree =
    blockWithBlockHole :: restOfTree



-- }}}
-- Util {{{


orElse : Maybe a -> Maybe a -> Maybe a
orElse second first =
    case first of
        Nothing ->
            second

        _ ->
            first



-- }}}
