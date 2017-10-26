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


type alias ResolvedAddress =
    { targetBlock : DisplayBlock
    , relativeIndex : Int
    }


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
                                    Return <| ResolvedAddress block relIndex

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


type Bot
    = Bot


addCursor : DisplayBlock -> Cursor -> DisplayBlock
addCursor rootBlock cursor =
    spelunk
        (\obj relIndex ->
            case obj of
                Block block ->
                    ( Block <|
                        List.take relIndex block
                            ++ [ Cursor ]
                            ++ List.drop relIndex block
                    , Return Bot
                    )

                Node node ->
                    ( obj, Bubble )
        )
        cursor
        rootBlock
        |> Tuple.first



--addCursorHelperBlock rootBlock 0 cursor
--|> Tuple.first


addCursorHelperBlock : DisplayBlock -> Int -> Cursor -> ( DisplayBlock, Int )
addCursorHelperBlock block startIndex targetIndex =
    if startIndex == targetIndex then
        ( [ Cursor ] ++ block, startIndex + numCursorSpotsInBlock block )
    else
        List.foldl
            (\node ( acc, currIndex ) ->
                let
                    ( processedNode, indexAfterNode ) =
                        addCursorHelperNode node currIndex targetIndex
                in
                ( if targetIndex == indexAfterNode then
                    acc ++ [ processedNode, Cursor ]
                  else
                    acc ++ [ processedNode ]
                , indexAfterNode
                )
            )
            ( [], startIndex )
            block
            |> Tuple.mapSecond ((+) 1)


addCursorHelperNode : DisplayNode -> Int -> Cursor -> ( DisplayNode, Int )
addCursorHelperNode node startIndex targetIndex =
    case node of
        Leaf _ ->
            ( node, startIndex + 1 )

        OneBlock nodeType block ->
            addCursorHelperBlock block (startIndex + 1) targetIndex
                |> Tuple.mapFirst (OneBlock nodeType)

        TwoBlocks nodeType block1 block2 ->
            let
                ( newBlock1, n ) =
                    addCursorHelperBlock block1 (startIndex + 1) targetIndex

                ( newBlock2, m ) =
                    addCursorHelperBlock block2 n targetIndex
            in
            ( TwoBlocks nodeType newBlock1 newBlock2, m )

        _ ->
            ( node, startIndex )


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


type alias BlockAddress =
    Int


type Iterator a
    = Found a
    | Unfound Cursor


andThen : (Cursor -> Iterator a) -> Iterator a -> Iterator a
andThen fn iterator =
    case iterator of
        Found _ ->
            iterator

        Unfound currCursor ->
            fn currCursor


resetForBlock : Iterator a -> Iterator a
resetForBlock iterator =
    case iterator of
        Unfound cursor ->
            Unfound cursor

        _ ->
            iterator


incrementIndex : Iterator a -> Iterator a
incrementIndex iterator =
    andThen
        (\cursor ->
            Unfound (cursor + 1)
        )
        iterator


matches : Cursor -> Iterator a -> Bool
matches targetCursor iterator =
    case iterator of
        Found _ ->
            False

        Unfound currCursor ->
            if currCursor == targetCursor then
                True
            else
                False


tryMatch : Cursor -> a -> Iterator a -> Iterator a
tryMatch targetCursor ifFound iterator =
    case iterator of
        Found _ ->
            iterator

        Unfound currCursor ->
            if currCursor == targetCursor then
                Found <| ifFound
            else
                iterator


resolveAddress : Cursor -> DisplayBlock -> Maybe ResolvedAddress
resolveAddress targetCursor rootBlock =
    let
        progressThroughBlock : DisplayBlock -> Iterator ResolvedAddress -> Iterator ResolvedAddress
        progressThroughBlock block iterator =
            let
                makeMatch relIndex iterator =
                    tryMatch targetCursor (ResolvedAddress block relIndex) iterator
            in
            List.indexedFoldl
                (\relativeIndex node accIterator ->
                    makeMatch relativeIndex accIterator
                        |> progressThroughNode node
                )
                iterator
                block
                |> makeMatch (List.length block)
                |> incrementIndex

        progressThroughNode : DisplayNode -> Iterator ResolvedAddress -> Iterator ResolvedAddress
        progressThroughNode node iterator =
            incrementIndex iterator
                |> (case node of
                        Leaf _ ->
                            identity

                        OneBlock nodeType block ->
                            progressThroughBlock block

                        TwoBlocks nodeType block1 block2 ->
                            progressThroughBlock block1
                                >> progressThroughBlock block2

                        Cursor ->
                            identity
                   )
    in
    case progressThroughBlock rootBlock (Unfound 0) of
        Found resolved ->
            Just resolved

        Unfound _ ->
            Nothing


type ShouldBubble a
    = Bubble
    | Return a


type WasFound a
    = YesFound (ShouldBubble a)
    | NotFound Cursor


type ShouldUpdate a
    = PleaseUpdate BlockAddress DisplayBlock
    | NotAnUpdate Cursor DisplayBlock
    | JustReturn a DisplayBlock


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


spelunk : (TreeObject -> Int -> ( TreeObject, ShouldBubble a )) -> Int -> DisplayBlock -> ( DisplayBlock, Maybe a )
spelunk spelunker targetCursor rootBlock =
    let
        spelunkBlock : DisplayBlock -> Cursor -> ( DisplayBlock, WasFound a )
        spelunkBlock block startIndex =
            List.indexedFoldl
                (\relativeIndex node shouldUpdate ->
                    case shouldUpdate of
                        JustReturn val accBlock ->
                            JustReturn val (accBlock ++ [ node ])

                        NotAnUpdate cursor accBlock ->
                            let
                                ( newTreeObj, wasFound ) =
                                    spelunkNode node cursor
                            in
                            if cursor == targetCursor then
                                PleaseUpdate relativeIndex (append newTreeObj accBlock)
                            else
                                case wasFound of
                                    YesFound Bubble ->
                                        PleaseUpdate relativeIndex (append newTreeObj accBlock)

                                    YesFound (Return val) ->
                                        JustReturn val (append newTreeObj accBlock)

                                    NotFound progressedCursor ->
                                        NotAnUpdate progressedCursor (append newTreeObj accBlock)

                        PleaseUpdate relIndex accBlock ->
                            PleaseUpdate relIndex (accBlock ++ [ node ])
                )
                (NotAnUpdate startIndex [])
                block
                |> (\shouldUpdate ->
                        case shouldUpdate of
                            NotAnUpdate cursor changedBlock ->
                                if cursor == targetCursor then
                                    PleaseUpdate (List.length block) changedBlock
                                else
                                    shouldUpdate

                            _ ->
                                shouldUpdate
                   )
                |> (\shouldUpdate ->
                        case shouldUpdate of
                            JustReturn val changedBlock ->
                                ( changedBlock, YesFound (Return val) )

                            NotAnUpdate cursor changedBlock ->
                                ( changedBlock, NotFound (cursor + 1) )

                            PleaseUpdate relIndex changedBlock ->
                                spelunker (Block changedBlock) relIndex
                                    |> Tuple.mapFirst toBlock
                                    |> Tuple.mapSecond YesFound
                   )

        spelunkNode : DisplayNode -> Cursor -> ( TreeObject, WasFound a )
        spelunkNode node startIndex =
            case node of
                Leaf _ ->
                    ( Node node, NotFound <| startIndex + 1 )

                OneBlock nodeType block ->
                    let
                        ( afterBlock, wasFound ) =
                            spelunkBlock block (startIndex + 1)

                        reconstructedNode =
                            Node <| OneBlock nodeType afterBlock
                    in
                    case wasFound of
                        YesFound Bubble ->
                            spelunker reconstructedNode 0
                                |> Tuple.mapSecond YesFound

                        _ ->
                            ( reconstructedNode, wasFound )

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

                Cursor ->
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
