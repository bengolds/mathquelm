module Mathquelm.EditableMath exposing (..)

import Mathquelm.Digit as Digit exposing (Digit)
import Mathquelm.Math as Math exposing (Math)


type Command
    = Var String
    | Digit Digit
    | Div Block Block
    | Cos Block
    | Plus


type alias Block =
    List Command


type MathBeingEdited
    = Cursor MathWithCursor
    | Selection MathWithSelection


type alias MathWithCursor =
    ( BlockWithCursor, TreeWithBlockHole )


type alias MathWithSelection =
    ( BlockWithSelection, TreeWithBlockHole )


type alias TreeWithBlockHole =
    List BlockWithBlockHole


type alias BlockWithBlockHole =
    { left : Block
    , commandWithBlockHole : CommandWithBlockHole
    , right : Block
    }


type alias BlockWithCursor =
    { left : Block
    , right : Block
    }


type alias BlockWithSelection =
    { left : Block
    , selected : Block
    , right : Block
    , direction : LeftRight
    }


type LeftRight
    = Left
    | Right


type CommandWithBlockHole
    = CosWithHole
    | DivWithTopHole Block
    | DivWithBotHole Block


toMath : MathBeingEdited -> Math.Math
toMath mathBeingEdited =
    stopEditingMath mathBeingEdited
        |> blockToMath


stopEditingMath : MathBeingEdited -> Block
stopEditingMath mathBeingEdited =
    List.foldl
        reassembleBlock
        (stopEditingBlock mathBeingEdited)
        (getRestOfTree mathBeingEdited)


stopEditingBlock : MathBeingEdited -> Block
stopEditingBlock mathBeingEdited =
    case mathBeingEdited of
        Cursor ( cursorBlock, _ ) ->
            removeCursor cursorBlock

        Selection ( selectionBlock, _ ) ->
            removeSelection selectionBlock


reassembleBlock : BlockWithBlockHole -> Block -> Block
reassembleBlock blockWithBlockHole fillerBlock =
    List.reverse blockWithBlockHole.left
        ++ [ reassembleCommand
                blockWithBlockHole.commandWithBlockHole
                fillerBlock
           ]
        ++ blockWithBlockHole.right


orElse : Maybe a -> Maybe a -> Maybe a
orElse second first =
    case first of
        Nothing ->
            second

        _ ->
            first


getRestOfTree : MathBeingEdited -> TreeWithBlockHole
getRestOfTree mathBeingEdited =
    case mathBeingEdited of
        Cursor ( _, restOfTree ) ->
            restOfTree

        Selection ( _, restOfTree ) ->
            restOfTree


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


enterCommandToLeft : MathWithCursor -> Maybe MathWithCursor
enterCommandToLeft ( cursorBlock, restOfTree ) =
    case cursorBlock.left of
        (Cos blockToEnter) :: restOfLeft ->
            Just
                ( placeCursorOnRight blockToEnter
                , push
                    (BlockWithBlockHole
                        restOfLeft
                        CosWithHole
                        cursorBlock.right
                    )
                    restOfTree
                )

        (Div top blockToEnter) :: restOfLeft ->
            Just
                ( placeCursorOnRight blockToEnter
                , push
                    (BlockWithBlockHole
                        restOfLeft
                        (DivWithBotHole top)
                        cursorBlock.right
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterCommandToRight : MathWithCursor -> Maybe MathWithCursor
enterCommandToRight ( cursorBlock, restOfTree ) =
    case cursorBlock.right of
        (Cos blockToEnter) :: restOfRight ->
            Just
                ( placeCursorOnLeft blockToEnter
                , push
                    (BlockWithBlockHole
                        cursorBlock.left
                        CosWithHole
                        restOfRight
                    )
                    restOfTree
                )

        (Div blockToEnter bot) :: restOfRight ->
            Just
                ( placeCursorOnLeft blockToEnter
                , push
                    (BlockWithBlockHole
                        cursorBlock.left
                        (DivWithTopHole bot)
                        restOfRight
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterTopOfCommandToLeft : MathWithCursor -> Maybe MathWithCursor
enterTopOfCommandToLeft ( cursorBlock, restOfTree ) =
    case cursorBlock.left of
        (Div blockToEnter bot) :: restOfLeft ->
            Just
                ( placeCursorOnRight blockToEnter
                , push
                    (BlockWithBlockHole
                        restOfLeft
                        (DivWithTopHole bot)
                        cursorBlock.right
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterTopOfCommandToRight : MathWithCursor -> Maybe MathWithCursor
enterTopOfCommandToRight ( cursorBlock, restOfTree ) =
    case cursorBlock.right of
        (Div blockToEnter bot) :: restOfRight ->
            Just
                ( placeCursorOnLeft blockToEnter
                , push
                    (BlockWithBlockHole
                        cursorBlock.left
                        (DivWithTopHole bot)
                        restOfRight
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterBottomOfCommandToLeft : MathWithCursor -> Maybe MathWithCursor
enterBottomOfCommandToLeft ( cursorBlock, restOfTree ) =
    case cursorBlock.left of
        (Div top blockToEnter) :: restOfLeft ->
            Just
                ( placeCursorOnRight blockToEnter
                , push
                    (BlockWithBlockHole
                        restOfLeft
                        (DivWithBotHole top)
                        cursorBlock.right
                    )
                    restOfTree
                )

        _ ->
            Nothing


enterBottomOfCommandToRight : MathWithCursor -> Maybe MathWithCursor
enterBottomOfCommandToRight ( cursorBlock, restOfTree ) =
    case cursorBlock.right of
        (Div top blockToEnter) :: restOfRight ->
            Just
                ( placeCursorOnLeft blockToEnter
                , push
                    (BlockWithBlockHole
                        cursorBlock.left
                        (DivWithBotHole top)
                        restOfRight
                    )
                    restOfTree
                )

        _ ->
            Nothing


jumpCommandToLeft : MathWithCursor -> Maybe MathWithCursor
jumpCommandToLeft ( cursorBlock, restOfTree ) =
    case cursorBlock.left of
        commandToLeft :: restOfLeft ->
            Just
                ( { left = restOfLeft
                  , right = commandToLeft :: cursorBlock.right
                  }
                , restOfTree
                )

        _ ->
            Nothing


jumpCommandToRight : MathWithCursor -> Maybe MathWithCursor
jumpCommandToRight ( cursorBlock, restOfTree ) =
    case cursorBlock.right of
        commandToRight :: restOfRight ->
            Just
                ( { left = commandToRight :: cursorBlock.left
                  , right = restOfRight
                  }
                , restOfTree
                )

        _ ->
            Nothing


removeCursor cursorBlock =
    List.reverse cursorBlock.left ++ cursorBlock.right


removeSelection selectionBlock =
    List.reverse selectionBlock.left
        ++ selectionBlock.selected
        ++ selectionBlock.right


placeCursorAtHole blockWithBlockHole =
    { left = blockWithBlockHole.left
    , right = blockWithBlockHole.right
    }


reassembleCommand commandWithBlockHole fillerBlock =
    case commandWithBlockHole of
        CosWithHole ->
            Cos fillerBlock

        DivWithTopHole bot ->
            Div fillerBlock bot

        DivWithBotHole top ->
            Div top fillerBlock


insertRightOfCursor command cursorBlock =
    { left = cursorBlock.left
    , right = command :: cursorBlock.right
    }


insertLeftOfCursor command cursorBlock =
    { left = command :: cursorBlock.left
    , right = cursorBlock.right
    }


getCommandBeingEdited restOfTree =
    case restOfTree of
        [] ->
            Nothing

        parentBlockWithHole :: _ ->
            Just parentBlockWithHole.command


exitCurrentBlockLeftward : MathWithCursor -> Maybe MathWithCursor
exitCurrentBlockLeftward ( cursorBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            case parentBlockWithHole.commandWithBlockHole of
                DivWithBotHole top ->
                    Just <|
                        moveToTopOfFraction
                            top
                            cursorBlock
                            parentBlockWithHole
                            grandparents

                _ ->
                    Just <|
                        exitCurrentCommandLeftward
                            cursorBlock
                            parentBlockWithHole
                            grandparents

        [] ->
            Nothing


exitCurrentBlockRightward : MathWithCursor -> Maybe MathWithCursor
exitCurrentBlockRightward ( cursorBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            case parentBlockWithHole.commandWithBlockHole of
                DivWithTopHole bottom ->
                    Just <|
                        moveToBottomOfFraction
                            bottom
                            cursorBlock
                            parentBlockWithHole
                            grandparents

                _ ->
                    Just <|
                        exitCurrentCommandRightward
                            cursorBlock
                            parentBlockWithHole
                            grandparents

        [] ->
            Nothing


exitCurrentBlockUpward : MathWithCursor -> Maybe MathWithCursor
exitCurrentBlockUpward ( cursorBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            case parentBlockWithHole.commandWithBlockHole of
                DivWithBotHole top ->
                    Just <|
                        moveToTopOfFraction
                            top
                            cursorBlock
                            parentBlockWithHole
                            grandparents

                _ ->
                    exitCurrentCommandLeftward cursorBlock parentBlockWithHole grandparents
                        |> exitCurrentBlockUpward

        [] ->
            Nothing


exitCurrentBlockDownward : MathWithCursor -> Maybe MathWithCursor
exitCurrentBlockDownward ( cursorBlock, restOfTree ) =
    case restOfTree of
        parentBlockWithHole :: grandparents ->
            case parentBlockWithHole.commandWithBlockHole of
                DivWithTopHole bot ->
                    Just <|
                        moveToBottomOfFraction
                            bot
                            cursorBlock
                            parentBlockWithHole
                            grandparents

                _ ->
                    exitCurrentCommandLeftward cursorBlock parentBlockWithHole grandparents
                        |> exitCurrentBlockDownward

        [] ->
            Nothing


moveToTopOfFraction : Block -> BlockWithCursor -> BlockWithBlockHole -> TreeWithBlockHole -> MathWithCursor
moveToTopOfFraction top cursorBlock parentBlockWithHole grandparents =
    ( placeCursorOnRight top
    , push
        (changeCommand
            (DivWithTopHole (removeCursor cursorBlock))
            parentBlockWithHole
        )
        grandparents
    )


moveToBottomOfFraction bot cursorBlock parentBlockWithHole grandparents =
    ( placeCursorOnLeft bot
    , push
        (changeCommand
            (DivWithBotHole (removeCursor cursorBlock))
            parentBlockWithHole
        )
        grandparents
    )


exitCurrentCommandLeftward cursorBlock parentBlockWithHole grandparents =
    ( placeCursorAtHole parentBlockWithHole
        |> insertRightOfCursor
            (reassembleCommand
                parentBlockWithHole.commandWithBlockHole
                (removeCursor cursorBlock)
            )
    , grandparents
    )


exitCurrentCommandRightward cursorBlock parentBlockWithHole grandparents =
    ( placeCursorAtHole parentBlockWithHole
        |> insertLeftOfCursor
            (reassembleCommand
                parentBlockWithHole.commandWithBlockHole
                (removeCursor cursorBlock)
            )
    , grandparents
    )


changeCommand : CommandWithBlockHole -> BlockWithBlockHole -> BlockWithBlockHole
changeCommand newCommand blockWithHole =
    { blockWithHole | commandWithBlockHole = newCommand }


placeCursorOnRight : Block -> BlockWithCursor
placeCursorOnRight block =
    { left = List.reverse block, right = [] }


placeCursorOnLeft : Block -> BlockWithCursor
placeCursorOnLeft block =
    { left = [], right = block }


push : BlockWithBlockHole -> TreeWithBlockHole -> TreeWithBlockHole
push blockWithBlockHole restOfTree =
    blockWithBlockHole :: restOfTree


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


turnCursorIntoSelection direction ( cursorBlock, restOfTree ) =
    ( { left = cursorBlock.left
      , selected = []
      , right = cursorBlock.right
      , direction = direction
      }
    , restOfTree
    )


selectLeft : MathBeingEdited -> Maybe MathBeingEdited
selectLeft mathBeingEdited =
    Nothing


selectUp : MathBeingEdited -> Maybe MathBeingEdited
selectUp mathBeingEdited =
    Nothing


selectDown : MathBeingEdited -> Maybe MathBeingEdited
selectDown mathBeingEdited =
    Nothing



{--
  -exitRight : EditableMath -> Maybe EditableMath
  -exitRight ( editable, crumbs ) =
  -    case ( editable, crumbs ) of
  -        ( Cursor _ [], { left, command, right } :: xs ) ->
  -            case command of
  -                TDivCr bot ->
  -                    Just
  -                        ( Cursor [] bot
  -                        , { left = left, command = BDivCr (rebuildBlock editable), right = right } :: xs
  -                        )
  -
  -                _ ->
  -                    Just ( Cursor (rebuildCommand (rebuildBlock editable) command :: left) right, xs )
  -
  -        _ ->
  -            Nothing
  -
  -
  -goUp : EditableMath -> Maybe EditableMath
  -goUp ( editable, crumbs ) =
  -    case editable of
  -        Cursor left right ->
  -            goLeftUp ( editable, crumbs )
  -                |> orElse (goRightUp ( editable, crumbs ))
  -                |> orElse (exitUp ( editable, crumbs ))
  -
  -        _ ->
  -            Nothing
  -
  -
  -goLeftUp : EditableMath -> Maybe EditableMath
  -goLeftUp ( editable, crumbs ) =
  -    case editable of
  -        Cursor ((Div top bot) :: left) right ->
  -            Just
  -                ( Cursor (List.reverse top) []
  -                , { left = left
  -                  , command = TDivCr bot
  -                  , right = right
  -                  }
  -                    :: crumbs
  -                )
  -
  -        _ ->
  -            Nothing
  -
  -
  -goRightUp : EditableMath -> Maybe EditableMath
  -goRightUp ( editable, crumbs ) =
  -    case editable of
  -        Cursor left ((Div top bot) :: right) ->
  -            Just
  -                ( Cursor [] top
  -                , { left = left
  -                  , command = TDivCr bot
  -                  , right = right
  -                  }
  -                    :: crumbs
  -                )
  -
  -        _ ->
  -            Nothing
  -
  -
  -exitUp : EditableMath -> Maybe EditableMath
  -exitUp ( editable, crumbs ) =
  -    case editable of
  -        Cursor _ _ ->
  -            case crumbs of
  -                { left, command, right } :: xs ->
  -                    case command of
  -                        BDivCr top ->
  -                            Just
  -                                ( Cursor [] top
  -                                , { left = left
  -                                  , command = TDivCr (rebuildBlock editable)
  -                                  , right = right
  -                                  }
  -                                    :: xs
  -                                )
  -
  -                        _ ->
  -                            exitUp
  -                                ( Cursor [] (left ++ rebuildCommand (rebuildBlock editable) command :: right)
  -                                , xs
  -                                )
  -
  -                [] ->
  -                    Nothing
  -
  -        _ ->
  -            Nothing
  -
  -
  -goDown : EditableMath -> Maybe EditableMath
  -goDown ( editable, crumbs ) =
  -    case editable of
  -        Cursor left right ->
  -            goLeftDown ( editable, crumbs )
  -                |> orElse (goRightDown ( editable, crumbs ))
  -                |> orElse (exitDown ( editable, crumbs ))
  -
  -        _ ->
  -            Nothing
  -
  -
  -goLeftDown : EditableMath -> Maybe EditableMath
  -goLeftDown ( editable, crumbs ) =
  -    case editable of
  -        Cursor ((Div top bot) :: left) right ->
  -            Just
  -                ( Cursor (List.reverse bot) []
  -                , { left = left
  -                  , command = BDivCr top
  -                  , right = right
  -                  }
  -                    :: crumbs
  -                )
  -
  -        _ ->
  -            Nothing
  -
  -
  -goRightDown : EditableMath -> Maybe EditableMath
  -goRightDown ( editable, crumbs ) =
  -    case editable of
  -        Cursor left ((Div top bot) :: right) ->
  -            Just
  -                ( Cursor [] bot
  -                , { left = left
  -                  , command = BDivCr top
  -                  , right = right
  -                  }
  -                    :: crumbs
  -                )
  -
  -        _ ->
  -            Nothing
  -
  -
  -exitDown : EditableMath -> Maybe EditableMath
  -exitDown ( editable, crumbs ) =
  -    case editable of
  -        Cursor _ _ ->
  -            case crumbs of
  -                { left, command, right } :: xs ->
  -                    case command of
  -                        TDivCr bot ->
  -                            Just
  -                                ( Cursor [] bot
  -                                , { left = left
  -                                  , command = BDivCr (rebuildBlock editable)
  -                                  , right = right
  -                                  }
  -                                    :: xs
  -                                )
  -
  -                        _ ->
  -                            exitDown
  -                                ( Cursor left (rebuildCommand (rebuildBlock editable) command :: right)
  -                                , xs
  -                                )
  -
  -                [] ->
  -                    Nothing
  -
  -        _ ->
  -            Nothing
  -
  --}
--insert : Command -> EditableMath -> EditableMath
--deleteLeft : EditableMath -> Maybe EditableMath
--deleteRight : EditableMath -> Maybe EditableMath


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



-- Editing


startEditing : Block -> MathBeingEdited
startEditing block =
    Cursor ( placeCursorOnLeft block, [] )



{--
  -
  -top : EditableMath -> Block
  -top ( editingBlock, crumbs ) =
  -    List.foldl
  -        (\{ left, command, right } childBlock ->
  -            List.reverse left ++ rebuildCommand childBlock command :: right
  -        )
  -        (rebuildBlock editingBlock)
  -        crumbs
  -
  -
  -rebuildBlock : EditingBlock -> Block
  -rebuildBlock editingBlock =
  -    case editingBlock of
  -        Cursor left right ->
  -            List.reverse left ++ right
  -
  -        LSel left selection right ->
  -            left ++ selection ++ right
  -
  -        RSel left selection right ->
  -            left ++ selection ++ right
  -
  -
  -rebuildCommand : Block -> CommandCrumb -> Command
  -rebuildCommand block commandWithHole =
  -    case commandWithHole of
  -        CosCr ->
  -            Cos block
  -
  -        TDivCr bot ->
  -            Div block bot
  -
  -        BDivCr top ->
  -            Div top block
  --}
