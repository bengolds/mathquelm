module Mathquelm.Render exposing (render)

import Char
import Element exposing (..)
import Element.Attributes exposing (..)
import Mathquelm.DisplayTree exposing (..)
import Mathquelm.RenderContext as RenderContext exposing (..)
import Mathquelm.Styles exposing (..)


render : RenderContext -> Element Styles variation msg
render context =
    let
        enterNode =
            Node >> enter context

        enterBlock =
            Block >> enter context

        rendered =
            case context.target of
                Block block ->
                    case block of
                        [] ->
                            el EmptySquare
                                [ width (px <| fontSize context * 0.8)
                                , height (px <| fontSize context)
                                ]
                                empty

                        [ x ] ->
                            render (enterNode x)

                        _ ->
                            let
                                blockCenter =
                                    centerLine <| enterBlock block
                            in
                            row None
                                [ width content, alignTop ]
                                (List.map
                                    (\childNode ->
                                        let
                                            childContext =
                                                enterNode childNode
                                        in
                                        el None [ paddingTop (blockCenter - centerLine childContext) ] (render childContext)
                                    )
                                    block
                                )

                Node node ->
                    case node of
                        Cursor ->
                            el None [ width <| px 0 ] empty
                                |> within
                                    [ el CursorLine [ width (px 2), height (px (getHeight context)) ] empty
                                    ]

                        Leaf (Character c) ->
                            let
                                style =
                                    if Char.isDigit c then
                                        None
                                    else
                                        Italic
                            in
                            el style [] <| text (String.fromChar c)

                        OneBlock blockType block ->
                            let
                                contentHeight =
                                    getHeight (enterBlock block)

                                renderedBlock =
                                    render (enterBlock block)
                            in
                            case blockType of
                                Subscript ->
                                    renderedBlock

                                Parens parenType ->
                                    row None
                                        [ verticalCenter
                                        , spacing 5
                                        ]
                                        [ leftParen context contentHeight parenType
                                        , renderedBlock
                                        , rightParen context contentHeight parenType
                                        ]

                        TwoBlocks Fraction numerator denominator ->
                            -- There are two things to pad in a fraction: the insides, so the divider is a bit wider, and the outside, so they
                            -- stack nicely.
                            let
                                innerPadding =
                                    fontSize context / 8

                                outerPadding =
                                    paddingXY 3 0

                                padInsides =
                                    el None [ paddingXY innerPadding 0 ]
                            in
                            column None
                                [ center, outerPadding ]
                                [ padInsides <| render (enterBlock numerator)
                                , divider
                                , padInsides <| render (enterBlock denominator)
                                ]

        {--
  -                Diacritic diacriticType block ->
  -                    column None [] <|
  -                        [ diacritic diacriticType
  -                        , renderChildren block
  -                        ]
  -                Subscript block ->
  -                    renderChildren block
  -
  -                Superscript block ->
  -                    renderChildren block
  -
  -                Subsuperscript sub super ->
  -                    column None
  -                        []
  -                        [ renderChildren super
  -                        , renderChildren sub
  -                        ]
  -
  -                SquareRoot block ->
  -                    let
  -                        contentHeight =
  -                            getChildrenHeight block
  -                    in
  -                    row None
  -                        [ verticalCenter
  -                        ]
  -                        [ scaledDelimiter context contentHeight 1 "√"
  -                        , column None
  -                            []
  -                            [ divider
  -                            , el None [ paddingXY 8 0 ] (renderChildren block)
  -                            ]
  -                        ]
  --}
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


centerLine : RenderContext -> Float
centerLine context =
    --centerLine is the distance from the top of the middle of the block
    let
        enterNode =
            Node >> enter context

        enterBlock =
            Block >> enter context
    in
    case context.target of
        Block block ->
            case block of
                [] ->
                    fontSize context / 2

                [ x ] ->
                    centerLine <| enterNode x

                _ ->
                    List.map (enterNode >> centerLine) block
                        |> List.maximum
                        |> Maybe.withDefault 0

        Node node ->
            case node of
                Cursor ->
                    getHeight context / 2

                Leaf (Character _) ->
                    getHeight context / 2

                OneBlock nodeType block ->
                    let
                        blockHeight =
                            getHeight (enterBlock block)
                    in
                    case nodeType of
                        Parens _ ->
                            let
                                parenOverlap =
                                    --case  of
                                    --Parens _ _ ->
                                    --0.05 * getChildrenHeight block
                                    --_ ->
                                    0.2 * getHeight (enterBlock block)
                            in
                            centerLine (enterBlock block) + parenOverlap / 2

                        Subscript ->
                            0

                TwoBlocks Fraction numerator denominator ->
                    getHeight <| enterBlock numerator


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
        enterNode =
            Node >> enter context

        enterBlock =
            Block >> enter context
    in
    case context.target of
        Block block ->
            case block of
                [] ->
                    fontSize context

                [ x ] ->
                    getHeight <| enterNode x

                _ ->
                    List.map (enterNode >> heightRect) block
                        |> List.unzip
                        |> mapBoth (List.maximum >> Maybe.withDefault 0)
                        |> (\( distFromTop, distFromBottom ) -> distFromTop + distFromBottom)

        Node node ->
            case node of
                Cursor ->
                    fontSize context

                Leaf (Character c) ->
                    floor (RenderContext.fontBox context) |> toFloat

                OneBlock nodeType block ->
                    let
                        blockHeight =
                            getHeight (enterBlock block)
                    in
                    case nodeType of
                        Parens _ ->
                            blockHeight * 1.05

                        Subscript ->
                            blockHeight

                TwoBlocks Fraction numerator denominator ->
                    (getHeight <| enterBlock numerator)
                        + (getHeight <| enterBlock denominator)
                        + 1



{--
  -        Diacritic diacriticType block ->
  -            context.config.baseFontSize + getChildrenHeight block
  -        Subscript block ->
  -            getChildrenHeight block
  -
  -        Superscript block ->
  -            getChildrenHeight block
  -
  -        Subsuperscript sub super ->
  -            getChildrenHeight super + getChildrenHeight sub
  -
  -        SquareRoot block ->
  -            getChildrenHeight block + 1
  --}


scaleAttr x y =
    attribute "style" ("transform: scale(" ++ toString x ++ "," ++ toString y ++ ")")


parensScale =
    1.1


scaledDelimiter context contentHeight scale symbol =
    let
        heightFrac =
            contentHeight / context.config.baseFontSize

        xScale =
            min (1 + 0.1 * (heightFrac - 1)) scale

        yScale =
            heightFrac * scale

        --_ =
        --Debug.log "contentHeight" contentHeight
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