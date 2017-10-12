module Mathquill exposing (..)

--module Mathquill exposing (mathquill)

import Char
import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Scale as Scale


tree : DisplayNode
tree =
    Block
        [ Parens Parentheses <|
            Parens Parentheses <|
                Fraction (Character '1') <|
                    Fraction (Character '2') <|
                        Fraction (Character '3') <|
                            Character 'b'

        --Fraction (Character '1') <|
        --Fraction (Character '1') <|
        --Fraction (Character '1') <|
        --Fraction
        --(Parens Parentheses (Block [ Character 'a', Character 'b' ]))
        --(Block [ Character 'b', Character 'c' ])
        ]


mathquill : Html msg
mathquill =
    layout stylesheet <|
        column None
            []
            [ loadFont
            , render 0 tree
            ]


baseFontSize : Float
baseFontSize =
    28.8


maxDepth =
    6


scaled =
    Scale.modular baseFontSize 0.9


type Styles
    = None
    | Base
    | ScaledBlock Int
    | Divider
    | Debug
    | Italic



--type Variations
--= Italic


stylesheet : StyleSheet Styles variations
stylesheet =
    Style.styleSheet <|
        [ style None
            []
        , style Divider
            [ Border.top 1
            , Color.border Color.black
            ]
        , style Debug
            [ Border.all 1, Color.border Color.black ]
        , style Italic
            [ Font.italic
            , Font.typeface [ Font.font "Times New Roman" ]
            ]
        ]
            ++ (List.range 0 maxDepth
                    |> List.map
                        (\depth ->
                            style (ScaledBlock depth) (textStyle depth)
                        )
               )


textStyle : Int -> List (Property class variations)
textStyle depth =
    [ Font.size (scaled (depth + 1))
    , Font.lineHeight 1
    , Font.typeface [ Font.font "Symbola" ]
    ]


loadFont : Element Styles variation msg
loadFont =
    html <|
        Html.node "style"
            []
            [ Html.text
                """
@font-face {
    font-family: "Symbola";
    src: url("fonts/Symbola.ttf") format("truetype");
}
                """
            ]


type DisplayNode
    = Character Char
    | Block (List DisplayNode)
    | Parens ParensType DisplayNode
    | Diacritic DiacriticType DisplayNode
    | Fraction DisplayNode DisplayNode



--| Superscript DisplayNode
--| Subscript DisplayNode
--| SuperSubscript DisplayNode DisplayNode


type ParensType
    = Parentheses
    | Brackets
    | Curlies
    | Pipes


type DiacriticType
    = Hat
    | Dot
    | Bar


render : Int -> DisplayNode -> Element Styles variation msg
render inDepth node =
    let
        depth =
            min inDepth maxDepth
    in
    case node of
        Block nodes ->
            row (ScaledBlock depth) [ width content, verticalCenter ] (List.map (render depth) nodes)

        Character c ->
            let
                style =
                    if Char.isDigit c then
                        None
                    else
                        Italic
            in
            el style [] <| text <| String.fromChar c

        Parens parenType node ->
            let
                contentHeight =
                    getHeight depth node
            in
            row (ScaledBlock depth)
                [ verticalCenter
                , spacing 5
                ]
            <|
                [ leftParen contentHeight parenType
                , render depth node
                , rightParen contentHeight parenType
                ]

        Diacritic diacriticType node ->
            column None [] <|
                [ diacritic diacriticType
                , render depth node
                ]

        Fraction numerator denominator ->
            column (ScaledBlock depth) [ center ] <|
                [ render (depth + 1) numerator
                , divider
                , render (depth + 1) denominator
                ]



--Subscript node ->
--el Debug [ height (baseFontSize * scale) ]


getHeight : Int -> DisplayNode -> Float
getHeight depth node =
    let
        height =
            case node of
                Block nodes ->
                    List.map (getHeight depth) nodes
                        |> List.maximum
                        |> Maybe.withDefault (scaled depth)

                Character c ->
                    scaled depth

                Parens parenType node ->
                    getHeight depth node * 1.05

                Diacritic diacriticType node ->
                    baseFontSize + getHeight depth node

                Fraction numerator denominator ->
                    getHeight (depth + 1) numerator + getHeight (depth + 1) denominator + 1

        _ =
            Debug.log ("height of " ++ toString node) height
    in
    height


scaleAttr x y =
    attribute "style" ("transform: scale(" ++ toString x ++ "," ++ toString y ++ ")")


parensScale =
    1.2


parenNode nodeHeight parensString =
    let
        heightFrac =
            nodeHeight / baseFontSize

        xScale =
            min (1 + 0.2 * (heightFrac - 1)) parensScale

        yScale =
            heightFrac * parensScale

        _ =
            Debug.log "nodeHeight" nodeHeight
    in
    row None
        [ height (px <| nodeHeight * parensScale)
        , verticalCenter
        ]
        [ el None [ scaleAttr xScale yScale ] (text parensString) ]



--[ el None [] (text parensString) ]


leftParen nodeHeight parenType =
    parenNode nodeHeight <|
        case parenType of
            Parentheses ->
                "("

            Brackets ->
                "["

            Curlies ->
                "{"

            Pipes ->
                "|"


rightParen nodeHeight parenType =
    parenNode nodeHeight <|
        case parenType of
            Parentheses ->
                ")"

            Brackets ->
                "]"

            Curlies ->
                "}"

            Pipes ->
                "|"


diacritic diacriticType =
    case diacriticType of
        _ ->
            el None [] <| text "^"


divider =
    el Divider [ width fill, height (px 0) ] empty
