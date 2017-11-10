module Mathquelm.EditableMath exposing (..)

import List.Extra as List
import Mathquelm.Digit as Digit exposing (Digit)
import Mathquelm.Math as Math exposing (Math)


toMath : EditableMath -> Math.Math
toMath editable =
    blockToMath (top editable)



--toRenderable : EditableMath -> Render.Renderable


orElse : Maybe a -> Maybe a -> Maybe a
orElse second first =
    case first of
        Nothing ->
            second

        _ ->
            first


goLeft : EditableMath -> Maybe EditableMath
goLeft ( editable, crumbs ) =
    case editable of
        Cursor (x :: left) right ->
            enterLeft ( editable, crumbs )
                |> orElse (swapLeft ( editable, crumbs ))

        Cursor [] right ->
            exitLeft ( editable, crumbs )

        _ ->
            Nothing


enterLeft : EditableMath -> Maybe EditableMath
enterLeft ( editable, crumbs ) =
    case editable of
        Cursor (cmd :: left) right ->
            let
                enter newBlock newCrumb =
                    Just
                        ( Cursor (List.reverse newBlock) []
                        , { left = left, command = newCrumb, right = right } :: crumbs
                        )
            in
            case cmd of
                Cos x ->
                    enter x CosCr

                Div top bot ->
                    enter bot (BDivCr top)

                _ ->
                    Nothing

        _ ->
            Nothing


swapLeft : EditableMath -> Maybe EditableMath
swapLeft ( editable, crumbs ) =
    case editable of
        Cursor (x :: left) right ->
            Just ( Cursor left (x :: right), crumbs )

        _ ->
            Nothing


exitLeft : EditableMath -> Maybe EditableMath
exitLeft ( editable, crumbs ) =
    case ( editable, crumbs ) of
        ( Cursor [] _, { left, command, right } :: xs ) ->
            case command of
                BDivCr top ->
                    Just
                        ( Cursor (List.reverse top) []
                        , { left = left
                          , command = TDivCr (rebuildBlock editable)
                          , right = right
                          }
                            :: xs
                        )

                _ ->
                    Just ( Cursor left (rebuildCommand (rebuildBlock editable) command :: right), xs )

        _ ->
            Nothing


goRight : EditableMath -> Maybe EditableMath
goRight ( editable, crumbs ) =
    case editable of
        Cursor _ _ ->
            enterRight ( editable, crumbs )
                |> orElse (swapRight ( editable, crumbs ))
                |> orElse (exitRight ( editable, crumbs ))

        _ ->
            Nothing


enterRight : EditableMath -> Maybe EditableMath
enterRight ( editable, crumbs ) =
    case editable of
        Cursor left (cmd :: right) ->
            let
                enter newBlock newCrumb =
                    Just
                        ( Cursor [] newBlock
                        , { left = left, command = newCrumb, right = right } :: crumbs
                        )
            in
            case cmd of
                Cos x ->
                    enter x CosCr

                Div top bot ->
                    enter top (TDivCr bot)

                _ ->
                    Nothing

        _ ->
            Nothing


swapRight : EditableMath -> Maybe EditableMath
swapRight ( editable, crumbs ) =
    case editable of
        Cursor left (x :: right) ->
            Just ( Cursor (x :: left) right, crumbs )

        _ ->
            Nothing


exitRight : EditableMath -> Maybe EditableMath
exitRight ( editable, crumbs ) =
    case ( editable, crumbs ) of
        ( Cursor _ [], { left, command, right } :: xs ) ->
            case command of
                TDivCr bot ->
                    Just
                        ( Cursor [] bot
                        , { left = left, command = BDivCr (rebuildBlock editable), right = right } :: xs
                        )

                _ ->
                    Just ( Cursor (rebuildCommand (rebuildBlock editable) command :: left) right, xs )

        _ ->
            Nothing


goUp : EditableMath -> Maybe EditableMath
goUp ( editable, crumbs ) =
    case editable of
        Cursor left right ->
            goLeftUp ( editable, crumbs )
                |> orElse (goRightUp ( editable, crumbs ))
                |> orElse (exitUp ( editable, crumbs ))

        _ ->
            Nothing


goLeftUp : EditableMath -> Maybe EditableMath
goLeftUp ( editable, crumbs ) =
    case editable of
        Cursor ((Div top bot) :: left) right ->
            Just
                ( Cursor (List.reverse top) []
                , { left = left
                  , command = TDivCr bot
                  , right = right
                  }
                    :: crumbs
                )

        _ ->
            Nothing


goRightUp : EditableMath -> Maybe EditableMath
goRightUp ( editable, crumbs ) =
    case editable of
        Cursor left ((Div top bot) :: right) ->
            Just
                ( Cursor [] top
                , { left = left
                  , command = TDivCr bot
                  , right = right
                  }
                    :: crumbs
                )

        _ ->
            Nothing


exitUp : EditableMath -> Maybe EditableMath
exitUp ( editable, crumbs ) =
    case editable of
        Cursor _ _ ->
            case crumbs of
                { left, command, right } :: xs ->
                    case command of
                        BDivCr top ->
                            Just
                                ( Cursor [] top
                                , { left = left
                                  , command = TDivCr (rebuildBlock editable)
                                  , right = right
                                  }
                                    :: xs
                                )

                        _ ->
                            exitUp
                                ( Cursor [] (left ++ rebuildCommand (rebuildBlock editable) command :: right)
                                , xs
                                )

                [] ->
                    Nothing

        _ ->
            Nothing


goDown : EditableMath -> Maybe EditableMath
goDown ( editable, crumbs ) =
    case editable of
        Cursor left right ->
            goLeftDown ( editable, crumbs )
                |> orElse (goRightDown ( editable, crumbs ))
                |> orElse (exitDown ( editable, crumbs ))

        _ ->
            Nothing


goLeftDown : EditableMath -> Maybe EditableMath
goLeftDown ( editable, crumbs ) =
    case editable of
        Cursor ((Div top bot) :: left) right ->
            Just
                ( Cursor (List.reverse bot) []
                , { left = left
                  , command = BDivCr top
                  , right = right
                  }
                    :: crumbs
                )

        _ ->
            Nothing


goRightDown : EditableMath -> Maybe EditableMath
goRightDown ( editable, crumbs ) =
    case editable of
        Cursor left ((Div top bot) :: right) ->
            Just
                ( Cursor [] bot
                , { left = left
                  , command = BDivCr top
                  , right = right
                  }
                    :: crumbs
                )

        _ ->
            Nothing


exitDown : EditableMath -> Maybe EditableMath
exitDown ( editable, crumbs ) =
    case editable of
        Cursor _ _ ->
            case crumbs of
                { left, command, right } :: xs ->
                    case command of
                        TDivCr bot ->
                            Just
                                ( Cursor [] bot
                                , { left = left
                                  , command = BDivCr (rebuildBlock editable)
                                  , right = right
                                  }
                                    :: xs
                                )

                        _ ->
                            exitDown
                                ( Cursor [] (left ++ rebuildCommand (rebuildBlock editable) command :: right)
                                , xs
                                )

                [] ->
                    Nothing

        _ ->
            Nothing



--insert : Command -> EditableMath -> EditableMath
--deleteLeft : EditableMath -> Maybe EditableMath
--deleteRight : EditableMath -> Maybe EditableMath


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



-- Editing


startEditing : Block -> EditableMath
startEditing block =
    ( Cursor [] block, [] )


top : EditableMath -> Block
top ( editingBlock, crumbs ) =
    List.foldl
        (\{ left, command, right } childBlock ->
            List.reverse left ++ rebuildCommand childBlock command :: right
        )
        (rebuildBlock editingBlock)
        crumbs


rebuildBlock : EditingBlock -> Block
rebuildBlock editingBlock =
    case editingBlock of
        Cursor left right ->
            List.reverse left ++ right

        LSel left selection right ->
            left ++ selection ++ right

        RSel left selection right ->
            left ++ selection ++ right


rebuildCommand : Block -> CommandCrumb -> Command
rebuildCommand block commandWithHole =
    case commandWithHole of
        CosCr ->
            Cos block

        TDivCr bot ->
            Div block bot

        BDivCr top ->
            Div top block


type alias EditableMath =
    ( EditingBlock, List SplitBlock )


type alias SplitBlock =
    { left : Block
    , command : CommandCrumb
    , right : Block
    }


type EditingBlock
    = Cursor Block Block
    | LSel Block Block Block
    | RSel Block Block Block


type CommandCrumb
    = CosCr
    | TDivCr Block
    | BDivCr Block
