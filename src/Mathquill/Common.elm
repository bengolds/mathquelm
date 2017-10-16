module Mathquill.Common
    exposing
        ( decodeDirection
        , ExitDirection(..)
        , NavigationDirection(..)
        )

import Json.Decode as Decode


type ExitDirection
    = Right
    | Left


type NavigationDirection
    = Up
    | Down
    | Default


decodeDirection : Decode.Decoder ExitDirection
decodeDirection =
    Decode.field "direction" Decode.int
        |> Decode.map
            (\direction ->
                if direction < 0 then
                    Left
                else
                    Right
            )
