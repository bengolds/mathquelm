module Mathquelm.Edit.Command exposing (..)

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
