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


type Crawler returnType objType
    = PleaseUpdate BlockAddress objType
    | DontUpdate Cursor objType
    | JustReturn returnType objType


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


updateBlockOrReturnValueOrProgressCursor spelunker shouldUpdate =
    case shouldUpdate of
        JustReturn val changedBlock ->
            ( changedBlock, YesFound (Return val) )

        DontUpdate cursor changedBlock ->
            ( changedBlock, NotFound (cursor + 1) )

        PleaseUpdate relIndex changedBlock ->
            spelunker (Block changedBlock) relIndex
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
spelunk spelunker targetCursor rootBlock =
    -- THE tree primitive! Go down the tree to find where your cursor is at. Bring back an "a", bubble up a level, and modify the tree on your way up.
    -- But there's gotta be a way to clean this up. I mean this is a hot fucking mess.
    let
        spelunkBlock : DisplayBlock -> Cursor -> ( DisplayBlock, WasFound a )
        spelunkBlock block startIndex =
            List.indexedFoldl
                -- Consider adding the spelunkNode and spelunker functions to the crawler
                (\relativeIndex node crawler ->
                    tryMatchCrawler targetCursor relativeIndex crawler
                        |> addNode node relativeIndex spelunkNode
                )
                (DontUpdate startIndex [])
                block
                |> tryMatchCrawler targetCursor (List.length block)
                |> updateBlockOrReturnValueOrProgressCursor spelunker

        spelunkNode : DisplayNode -> Cursor -> ( TreeObject, WasFound a )
        spelunkNode node startIndex =
            let
                buildNode builder ( block, wasFound ) =
                    ( builder block, wasFound )

                finalizeNode ( node, wasFound ) =
                    ( Node node, wasFound )

                updateNodeOrReturnValueOrProgressCursor ( spelunkedNode, wasFound ) =
                    case wasFound of
                        YesFound Bubble ->
                            spelunker spelunkedNode 0
                                |> Tuple.mapSecond YesFound

                        YesFound (Return val) ->
                            ( spelunkedNode, wasFound )

                        NotFound cursor ->
                            ( spelunkedNode, wasFound )

                -- Do I need a node crawler here? Or what? I'm confused
            in
            case node of
                OneBlock nodeType block ->
                    spelunkBlock block (startIndex + 1)
                        |> buildNode (OneBlock nodeType)
                        |> finalizeNode
                        |> updateNodeOrReturnValueOrProgressCursor

                TwoBlocks nodeType block1 block2 ->
                    let
                        ( afterBlock1, wasFound1 ) =
                            spelunkBlock block1 (startIndex + 1)

                        reconstructedNode =
                            Node <| TwoBlocks nodeType afterBlock1 block2
                    in
                    case wasFound1 of
                        YesFound Bubble ->
                            spelunker reconstructedNode 0
                                |> Tuple.mapSecond YesFound

                        YesFound (Return val) ->
                            ( reconstructedNode, YesFound (Return val) )

                        NotFound progressedCursor ->
                            let
                                ( afterBlock2, wasFound2 ) =
                                    spelunkBlock block2 progressedCursor

                                reconstructedNode2 =
                                    Node <| TwoBlocks nodeType afterBlock1 afterBlock2
                            in
                            case wasFound2 of
                                YesFound Bubble ->
                                    spelunker reconstructedNode2 0
                                        |> Tuple.mapSecond YesFound

                                _ ->
                                    ( reconstructedNode2, wasFound2 )

                _ ->
                    ( Node node, NotFound <| startIndex + 1 )
    in
    spelunkBlock rootBlock 0
        |> Tuple.mapSecond
            (\wasFound ->
                case wasFound of
                    YesFound (Return val) ->
                        Just val

                    _ ->
                        Nothing
            )
