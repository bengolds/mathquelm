module Mathquelm.Styles exposing (..)

import Color
import Element exposing (Element)
import Html
import Mathquelm.Config as Config exposing (Config)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


type Styles
    = None
    | Base
    | ScaledBlock Int
    | Divider
    | Italic
    | EmptySquare
    | CursorLine
    | DebugCenterline
    | DebugBox Int


stylesheet : Config -> StyleSheet Styles variations
stylesheet config =
    Style.styleSheet <|
        [ style None
            []
        , style Divider
            [ Border.top 1
            , Color.border Color.black
            ]
        , style Italic
            [ Font.italic
            , Font.typeface [ Font.font "Times New Roman" ]
            ]
        , style CursorLine
            [ Border.left 2
            , Color.border Color.green
            ]
        , style EmptySquare
            [ Color.background Color.gray
            ]
        , style DebugCenterline
            [ Border.top 1
            , Color.border Color.red
            ]
        , style Base
            [ Font.typeface [ Font.font "Symbola" ]
            ]
        ]
            ++ (List.range 0 config.maxDepth
                    |> List.map
                        (\depth ->
                            [ style (ScaledBlock depth)
                                [ Font.size (Config.scaled config depth)
                                ]
                            , style (DebugBox depth)
                                [ Color.background <| debugColor depth ]
                            ]
                        )
                    |> List.concat
               )


loadFont : Element Styles variation msg
loadFont =
    Element.html <|
        Html.node "style"
            []
            [ Html.text
                """
@font-face {
    font-family: "Symbola";
    src: url("/fonts/Symbola.ttf") format("truetype");
}
                """
            ]


debugColor depth =
    case depth % 6 of
        0 ->
            Color.orange

        1 ->
            Color.yellow

        2 ->
            Color.green

        3 ->
            Color.blue

        4 ->
            Color.darkBlue

        5 ->
            Color.purple

        _ ->
            Color.brown
