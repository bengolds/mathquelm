module Mathquelm.Cursor exposing (..)

import List.Extra as List
import Mathquelm.DisplayTree exposing (..)


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


type alias Cursor =
    Int


type alias BlockAddress =
    Int


type ShouldBubble a
    = Bubble
    | Return a


type WasFound a
    = YesFound (ShouldBubble a)
    | NotFound Cursor


type BlockCrawler returnType
    = PleaseUpdate BlockAddress DisplayBlock
    | DontUpdate Cursor DisplayBlock
    | JustReturn returnType DisplayBlock


append : TreeObject -> DisplayBlock -> DisplayBlock
append obj block =
    block ++ toBlock obj


toBlock : TreeObject -> DisplayBlock
toBlock obj =
    case obj of
        Node node ->
            [ node ]

        Block block ->
            block


moveCursor : MoveDirection -> DisplayBlock -> Cursor -> Cursor
moveCursor dir rootBlock address =
    let
        clampCursor =
            clamp 0 (numCursorSpotsInBlock rootBlock - 1)
    in
    case dir of
        Left ->
            clampCursor (address - 1)

        Right ->
            clampCursor (address + 1)

        Up ->
            let
                ( _, resolved ) =
                    spelunk
                        (\obj relIndex ->
                            ( obj
                            , case obj of
                                Block block ->
                                    Return <| ( block, relIndex )

                                _ ->
                                    Bubble
                            )
                        )
                        address
                        rootBlock

                _ =
                    Debug.log "target" address

                _ =
                    Debug.log "resolved" resolved
            in
            address

        --address
        Down ->
            address


type Empty
    = Empty


addCursor : DisplayBlock -> Cursor -> DisplayBlock
addCursor rootBlock cursor =
    updateBlockAt
        (\block relIndex ->
            List.take relIndex block
                ++ [ Cursor ]
                ++ List.drop relIndex block
        )
        cursor
        rootBlock


numCursorSpotsNode : DisplayNode -> Int
numCursorSpotsNode node =
    case node of
        Cursor ->
            0

        Leaf _ ->
            1

        OneBlock _ block ->
            numCursorSpotsInBlock block + 1

        TwoBlocks _ block1 block2 ->
            numCursorSpotsInBlock block1
                + numCursorSpotsInBlock block2
                + 1


numCursorSpotsInBlock : DisplayBlock -> Int
numCursorSpotsInBlock block =
    block
        |> List.map numCursorSpotsNode
        |> List.sum
        |> (+) 1


moveUpOrDown : UD -> DisplayBlock -> Cursor -> Cursor
moveUpOrDown dir rootBlock address =
    case dir of
        U ->
            address

        D ->
            address


updateBlockAt : (DisplayBlock -> BlockAddress -> DisplayBlock) -> Cursor -> DisplayBlock -> DisplayBlock
updateBlockAt fn address rootBlock =
    spelunk
        (\obj relIndex ->
            case obj of
                Block block ->
                    ( Block <| fn block relIndex, Return Empty )

                Node node ->
                    ( obj, Bubble )
        )
        address
        rootBlock
        |> Tuple.first


type alias Iterator a =
    { obj : TreeObject
    , wasFound : WasFound a
    }


updateBlockOrReturnValueOrProgressCursor updateFn shouldUpdate =
    case shouldUpdate of
        JustReturn val changedBlock ->
            ( changedBlock, YesFound (Return val) )

        DontUpdate cursor changedBlock ->
            ( changedBlock, NotFound (cursor + 1) )

        PleaseUpdate relIndex changedBlock ->
            updateFn (Block changedBlock) relIndex
                |> Tuple.mapFirst toBlock
                |> Tuple.mapSecond YesFound


tryMatchCrawler targetCursor relativeIndex crawler =
    case crawler of
        DontUpdate cursor changedBlock ->
            if cursor == targetCursor then
                PleaseUpdate relativeIndex changedBlock
            else
                crawler

        _ ->
            crawler


addNode node relativeIndex spelunkNode crawler =
    case crawler of
        JustReturn val accBlock ->
            JustReturn val (accBlock ++ [ node ])

        DontUpdate cursor accBlock ->
            let
                ( newTreeObj, wasFound ) =
                    spelunkNode node cursor

                newBlock =
                    append newTreeObj accBlock
            in
            case wasFound of
                YesFound Bubble ->
                    PleaseUpdate relativeIndex newBlock

                YesFound (Return val) ->
                    JustReturn val newBlock

                NotFound progressedCursor ->
                    DontUpdate progressedCursor newBlock

        PleaseUpdate relIndex accBlock ->
            PleaseUpdate relIndex (accBlock ++ [ node ])


spelunk : (TreeObject -> BlockAddress -> ( TreeObject, ShouldBubble a )) -> Cursor -> DisplayBlock -> ( DisplayBlock, Maybe a )
spelunk updateFn targetCursor rootBlock =
    -- THE tree primitive! Go down the tree to find where your cursor is at. Bring back an "a", bubble up a level, and modify the tree on your way up.
    -- But there's gotta be a way to clean this up. I mean this is a hot fucking mess.
    let
        spelunkBlock : Cursor -> DisplayBlock -> ( DisplayBlock, WasFound a )
        spelunkBlock startIndex block =
            List.indexedFoldl
                -- Consider adding the spelunkNode and updateFn functions to the crawler
                (\relativeIndex node crawler ->
                    tryMatchCrawler targetCursor relativeIndex crawler
                        |> addNode node relativeIndex spelunkNode
                )
                (DontUpdate startIndex [])
                block
                |> tryMatchCrawler targetCursor (List.length block)
                |> updateBlockOrReturnValueOrProgressCursor updateFn

        spelunkNode : DisplayNode -> Cursor -> ( TreeObject, WasFound a )
        spelunkNode node startIndex =
            -- Key insight: the ONLY way to update a node is by bubbling up to it.
            let
                spelunkNextBlock block ( builder, foundYet ) =
                    case foundYet of
                        NotFound cursor ->
                            spelunkBlock cursor block
                                |> Tuple.mapFirst builder

                        _ ->
                            -- Should I make this more explicit or not?
                            ( builder block, foundYet )

                maybeUpdateNodeOrElseWrap ( spelunkedNode, foundYet ) =
                    case foundYet of
                        YesFound Bubble ->
                            updateFn (Node spelunkedNode) 0
                                |> Tuple.mapSecond YesFound

                        _ ->
                            ( Node spelunkedNode, foundYet )
            in
            case node of
                OneBlock nodeType block ->
                    ( OneBlock nodeType, NotFound (startIndex + 1) )
                        |> spelunkNextBlock block
                        |> maybeUpdateNodeOrElseWrap

                TwoBlocks nodeType block1 block2 ->
                    ( TwoBlocks nodeType, NotFound (startIndex + 1) )
                        |> spelunkNextBlock block1
                        |> spelunkNextBlock block2
                        |> maybeUpdateNodeOrElseWrap

                _ ->
                    ( Node node, NotFound <| startIndex + 1 )
    in
    spelunkBlock 0 rootBlock
        |> Tuple.mapSecond
            (\wasFound ->
                case wasFound of
                    YesFound (Return val) ->
                        Just val

                    _ ->
                        Nothing
            )
