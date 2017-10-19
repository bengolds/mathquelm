module Mathquelm.CursorMovement exposing (MoveDirection(..), moveCursor)

import List.Extra as List
import Mathquelm.DisplayNode exposing (..)


type MoveDirection
    = Up
    | Down
    | Left
    | Right


type ExitCommand
    = Exit
    | NoCmd


type LR
    = L
    | R


type UD
    = U
    | D



--type alias NodeAddress =
--Int


moveCursor : MoveDirection -> Block -> Block
moveCursor dir block =
    case dir of
        Left ->
            Tuple.first (moveBlockLR L block)

        Right ->
            Tuple.first (moveBlockLR R block)

        Up ->
            block

        Down ->
            block



-- {{{ Left/Right Movement


moveBlockLR : LR -> Block -> ( Block, ExitCommand )
moveBlockLR dir block =
    case List.elemIndex Cursor block of
        Just n ->
            if atEdge n dir block then
                ( block, Exit )
            else
                let
                    adjacentIndex =
                        case dir of
                            L ->
                                n - 1

                            R ->
                                n + 1
                in
                -- Try entering the adjacent node; if that fails try swapping;
                -- if that fails, give up.
                ( tryEnteringLR adjacentIndex dir block
                    |> Maybe.withDefault
                        (List.swapAt n adjacentIndex block
                            |> Maybe.withDefault block
                        )
                , NoCmd
                )

        Nothing ->
            ( -- Try to move the stuff below, and then listen for incoming exits.
              List.map (moveNodeLR dir) block
                |> List.foldr
                    (\( node, shouldExit ) acc ->
                        case shouldExit of
                            Exit ->
                                -- The thing below has told us the cursor needs to exit it. In that case, put the cursor next to that node.
                                case dir of
                                    L ->
                                        Cursor :: cleanseOfCursors node :: acc

                                    R ->
                                        cleanseOfCursors node :: Cursor :: acc

                            NoCmd ->
                                node :: acc
                    )
                    []
              --Blocks can always handle L/R movement of their kids, so we can safely say NoCmd
            , NoCmd
            )


tryEnteringLR : Int -> LR -> Block -> Maybe Block
tryEnteringLR index dir block =
    List.getAt index block
        |> Maybe.andThen (enterNodeLR dir)
        |> Maybe.andThen (\entered -> List.setAt index entered block)
        |> Maybe.map cleanseBlock


enterNodeLR : LR -> DisplayNode -> Maybe DisplayNode
enterNodeLR dir node =
    let
        _ =
            Debug.log "Trying to enter" node
    in
    case node of
        OneBlock blockType block ->
            Just <| OneBlock blockType (appendCursorToBlock dir block)

        TwoBlocks Fraction numerator denominator ->
            Just <|
                case dir of
                    L ->
                        TwoBlocks Fraction numerator (appendCursorToBlock dir denominator)

                    R ->
                        TwoBlocks Fraction (appendCursorToBlock dir numerator) denominator

        _ ->
            Nothing


moveNodeLR : LR -> DisplayNode -> ( DisplayNode, ExitCommand )
moveNodeLR dir node =
    case node of
        -- Most OneBlocks don't do anything if the cursor exits the child block, so it passes the command through.
        OneBlock nodeType block ->
            moveBlockLR dir block
                |> Tuple.mapFirst (OneBlock nodeType)

        TwoBlocks Fraction numerator denominator ->
            let
                ( firstBlock, secondBlock ) =
                    case dir of
                        L ->
                            ( denominator, numerator )

                        R ->
                            ( numerator, denominator )

                reconstruct first second =
                    case dir of
                        L ->
                            TwoBlocks Fraction second first

                        R ->
                            TwoBlocks Fraction first second
            in
            case moveBlockLR dir firstBlock of
                ( movedFirstBlock, Exit ) ->
                    -- If we try to exit left from the first block, move the cursor to the next block
                    ( reconstruct (cleanseBlock movedFirstBlock) (appendCursorToBlock dir secondBlock), NoCmd )

                ( movedFirstBlock, _ ) ->
                    -- Otherwise, just move the cursor in the second block. Exits here get handled above us.
                    let
                        ( movedSecondBlock, cmd ) =
                            moveBlockLR dir secondBlock
                    in
                    ( reconstruct movedFirstBlock movedSecondBlock, cmd )

        _ ->
            ( node, NoCmd )


appendCursorToBlock : LR -> Block -> Block
appendCursorToBlock dir block =
    case dir of
        L ->
            block ++ [ Cursor ]

        R ->
            [ Cursor ] ++ block


atEdge : Int -> LR -> Block -> Bool
atEdge n dir block =
    case dir of
        L ->
            n == 0

        R ->
            n == List.length block - 1



-- }}}
-- {{{ Cleansing Helpers


cleanseOfCursors : DisplayNode -> DisplayNode
cleanseOfCursors node =
    case node of
        OneBlock blockType block ->
            OneBlock blockType (cleanseBlock block)

        TwoBlocks blockType block1 block2 ->
            TwoBlocks Fraction (cleanseBlock block1) (cleanseBlock block2)

        _ ->
            node


cleanseBlock block =
    List.filter (\node -> node /= Cursor) block



--}}}
