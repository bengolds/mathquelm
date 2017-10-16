module Mathquelm exposing (..)

import Char
import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Mathquelm.Config as Config exposing (Config)
import Mathquelm.DisplayNode exposing (..)
import Mathquelm.RenderContext as RenderContext exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


tree : DisplayNode
tree =
    Block
        [ Character 'a'
        , Subsuperscript (Fraction (Character 'z') (Character 'd')) (Character '1')
        , Parens Parentheses <|
            Fraction (Character '1') <|
                Fraction (Character '2') <|
                    Fraction (Character '3') <|
                        Fraction (Character '4') <|
                            Fraction (Character '5') <|
                                Character '3'
        , SquareRoot (Block [ Fraction (Character 'x') (Character 'y'), Character 'a', Character 'b' ])
        ]


latex : String
latex =
    toLatex tree



--mathquill : Html msg


mathquill config =
    layout (stylesheet config) <|
        column None
            []
            [ loadFont
            , render (baseContext config tree)
            ]


type Styles
    = None
    | Base
    | ScaledBlock Int
    | Divider
    | Italic
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
        , style DebugCenterline
            [ Border.top 1
            , Color.border Color.red
            ]
        ]
            ++ (List.range 0 config.maxDepth
                    |> List.map
                        (\depth ->
                            [ style (ScaledBlock depth)
                                [ Font.size (Config.scaled config depth)
                                , Font.typeface [ Font.font "Symbola" ]
                                ]
                            , style (DebugBox depth)
                                [ Color.background <| debugColor depth ]
                            ]
                        )
                    |> List.concat
               )


loadFont : Element Styles variation msg
loadFont =
    html <|
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



--| Superscript DisplayNode
--| SuperSubscript DisplayNode DisplayNode


render : RenderContext -> Element Styles variation msg
render context =
    let
        rendered =
            case context.node of
                Block nodes ->
                    let
                        blockCenter =
                            centerLine context
                    in
                    row None
                        [ width content, alignTop ]
                        (List.map
                            (\child ->
                                let
                                    childContext =
                                        enter context child

                                    rendered =
                                        render childContext

                                    childCenter =
                                        centerLine childContext
                                in
                                el None [ paddingTop (blockCenter - childCenter) ] rendered
                            )
                            nodes
                        )

                Character c ->
                    let
                        style =
                            if Char.isDigit c then
                                None
                            else
                                Italic
                    in
                    el style [] <| text (String.fromChar c)

                Parens parenType child ->
                    let
                        childContext =
                            enter context child

                        contentHeight =
                            getHeight childContext
                    in
                    row None
                        [ verticalCenter
                        , spacing 5
                        ]
                        [ leftParen context contentHeight parenType
                        , render childContext
                        , rightParen context contentHeight parenType
                        ]

                Diacritic diacriticType child ->
                    column None [] <|
                        [ diacritic diacriticType
                        , render (enter context child)
                        ]

                Fraction numerator denominator ->
                    column None
                        [ center ]
                        [ render (enter context numerator)
                        , divider
                        , render (enter context denominator)
                        ]

                Subscript child ->
                    render (enter context child)

                Superscript child ->
                    render (enter context child)

                Subsuperscript top bottom ->
                    column None
                        []
                        [ render (enter context top)
                        , render (enter context bottom)
                        ]

                SquareRoot child ->
                    let
                        childContext =
                            enter context child
                    in
                    row None
                        [ verticalCenter
                        , spacing 5
                        ]
                        [ scaledDelimiter context (getHeight childContext) 1 "√"
                        , column None
                            []
                            [ divider
                            , render childContext
                            ]
                        ]
    in
    wrapInDebug context node <|
        el (ScaledBlock context.depth) [] rendered


wrapInDebug context node rendered =
    let
        centerLineDiv =
            if context.config.showCenterLines then
                above
                    [ el DebugCenterline [ moveDown (centerLine context), width (px 16) ] empty
                    ]
            else
                identity

        outlineBox =
            if context.config.showBoxes then
                el (DebugBox context.depth) []
            else
                identity
    in
    rendered
        |> outlineBox
        |> centerLineDiv


centerLine : RenderContext -> Float
centerLine context =
    --centerLine is the distance from the top of the middle of the block
    case context.node of
        Block nodes ->
            List.map (enter context >> centerLine) nodes
                |> List.maximum
                |> Maybe.withDefault 0

        Character c ->
            getHeight context / 2

        Fraction numerator denominator ->
            getHeight (enter context numerator)

        Parens _ child ->
            let
                parenOverlap =
                    case child of
                        Parens _ _ ->
                            0.05 * getHeight (enter context child)

                        _ ->
                            0.2 * getHeight (enter context child)
            in
            centerLine (enter context child) + parenOverlap / 2

        Diacritic _ _ ->
            0

        Subscript _ ->
            0

        Superscript child ->
            getHeight (enter context child)

        Subsuperscript top _ ->
            getHeight (enter context top)

        SquareRoot child ->
            centerLine (enter context child)


heightRect : RenderContext -> ( Float, Float )
heightRect context =
    let
        height =
            getHeight context

        distanceFromTop =
            centerLine context
    in
    ( distanceFromTop, height - distanceFromTop )


mapBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth fn ( fst, snd ) =
    ( fn fst, fn snd )


getHeight : RenderContext -> Float
getHeight context =
    let
        height =
            case context.node of
                Block nodes ->
                    List.map (enter context >> heightRect) nodes
                        |> List.unzip
                        |> mapBoth (List.maximum >> Maybe.withDefault 0)
                        |> (\( distFromTop, distFromBottom ) -> distFromTop + distFromBottom)

                Character c ->
                    floor (RenderContext.fontBox context) |> toFloat

                Parens parenType child ->
                    getHeight (enter context child) * 1.05

                Diacritic diacriticType child ->
                    context.config.baseFontSize + getHeight (enter context child)

                Fraction numerator denominator ->
                    getHeight (enter context numerator) + getHeight (enter context denominator) + 1

                Subscript child ->
                    getHeight (enter context child)

                Superscript child ->
                    getHeight (enter context child)

                Subsuperscript top bottom ->
                    getHeight (enter context top) + getHeight (enter context bottom)

                SquareRoot child ->
                    getHeight (enter context child)

        _ =
            Debug.log ("height of " ++ toString context.node) height
    in
    height


scaleAttr x y =
    attribute "style" ("transform: scale(" ++ toString x ++ "," ++ toString y ++ ")")


parensScale =
    1.2


scaledDelimiter context contentHeight scale symbol =
    let
        heightFrac =
            contentHeight / context.config.baseFontSize

        xScale =
            min (1 + 0.2 * (heightFrac - 1)) scale

        yScale =
            heightFrac * scale

        _ =
            Debug.log "contentHeight" contentHeight
    in
    row None
        [ height (px <| contentHeight * scale)
        , verticalCenter
        ]
        [ el None [ scaleAttr xScale yScale ] (text symbol) ]



--[ el None [] (text parensString) ]


leftParen context nodeHeight parenType =
    scaledDelimiter context nodeHeight parensScale <|
        case parenType of
            Parentheses ->
                "("

            Brackets ->
                "["

            Curlies ->
                "{"

            Pipes ->
                "|"


rightParen context nodeHeight parenType =
    scaledDelimiter context nodeHeight parensScale <|
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
