module Mathquelm.EditableMath exposing (..)

import List.Extra as List
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


type alias MathBeingEdited =
    ( BlockBeingEdited, RestOfTree )


type alias RestOfTree =
    List BlockWithBlockHole


type alias BlockWithBlockHole =
    { left : Block
    , commandWithBlockHole : CommandWithBlockHole
    , right : Block
    }


type BlockBeingEdited
    = BlockWithCursor CursorInfo
    | BlockWithSelection SelectionInfo


type alias CursorInfo =
    { left : Block
    , right : Block
    }


type alias SelectionInfo =
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
stopEditingMath ( blockBeingEdited, restOfTree ) =
    List.foldl
        reassembleBlock
        (stopEditingBlock blockBeingEdited)
        restOfTree


stopEditingBlock blockBeingEdited =
    case blockBeingEdited of
        BlockWithCursor cursorBlock ->
            removeCursor cursorBlock

        BlockWithSelection selectionBlock ->
            removeSelection selectionBlock


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


getBlockBeingEdited ( blockBeingEdited, _ ) =
    blockBeingEdited


getRestOfTree ( _, restOfTree ) =
    restOfTree


goLeft : MathBeingEdited -> Maybe MathBeingEdited
goLeft math =
    case getBlockBeingEdited math of
        BlockWithCursor cursorBlock ->
            let
                restOfTree =
                    getRestOfTree math
            in
            enterCommandToLeft cursorBlock restOfTree
                |> orElse (jumpCommandToLeft cursorBlock restOfTree)
                |> orElse (exitCurrentBlockLeftward cursorBlock restOfTree)

        _ ->
            Nothing


goRight : MathBeingEdited -> Maybe MathBeingEdited
goRight math =
    case getBlockBeingEdited math of
        BlockWithCursor cursorBlock ->
            let
                restOfTree =
                    getRestOfTree math
            in
            enterCommandToRight cursorBlock restOfTree
                |> orElse (jumpCommandToRight cursorBlock restOfTree)
                |> orElse (exitCurrentBlockRightward cursorBlock restOfTree)

        _ ->
            Nothing


goUp math =
    Nothing


goDown math =
    Nothing


enterCommandToLeft cursorBlock restOfTree =
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


enterCommandToRight cursorBlock restOfTree =
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


jumpCommandToLeft cursorBlock restOfTree =
    case cursorBlock.left of
        commandToLeft :: restOfLeft ->
            Just
                ( BlockWithCursor
                    { left = restOfLeft
                    , right = commandToLeft :: cursorBlock.right
                    }
                , restOfTree
                )

        _ ->
            Nothing


jumpCommandToRight cursorBlock restOfTree =
    case cursorBlock.right of
        commandToRight :: restOfRight ->
            Just
                ( BlockWithCursor
                    { left = commandToRight :: cursorBlock.left
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


exitCurrentBlockLeftward cursorBlock restOfTree =
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


exitCurrentBlockRightward cursorBlock restOfTree =
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
        |> BlockWithCursor
    , grandparents
    )


exitCurrentCommandRightward cursorBlock parentBlockWithHole grandparents =
    ( placeCursorAtHole parentBlockWithHole
        |> insertLeftOfCursor
            (reassembleCommand
                parentBlockWithHole.commandWithBlockHole
                (removeCursor cursorBlock)
            )
        |> BlockWithCursor
    , grandparents
    )


changeCommand newCommand blockWithHole =
    { blockWithHole | commandWithBlockHole = newCommand }


placeCursorOnRight block =
    BlockWithCursor { left = List.reverse block, right = [] }


placeCursorOnLeft block =
    BlockWithCursor { left = [], right = block }


push blockWithBlockHole restOfTree =
    blockWithBlockHole :: restOfTree



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
    ( placeCursorOnLeft block, [] )



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
