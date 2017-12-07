module Mathquelm.Util exposing (..)


orElse : Maybe a -> Maybe a -> Maybe a
orElse second first =
    case first of
        Nothing ->
            second

        _ ->
            first
