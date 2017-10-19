module Mathquelm exposing (..)

import Char
import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Keyboard
import Mathquelm.Config as Config exposing (Config)
import Mathquelm.CursorMovement exposing (MoveDirection(..), moveCursor)
import Mathquelm.DisplayNode exposing (..)
import Mathquelm.RenderContext as RenderContext exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


stringToNodes string =
    String.toList string
        |> List.map (Character >> Leaf)


sampleTree : Block
sampleTree =
    stringToNodes "abc"
        ++ [ OneBlock (Parens Parentheses) (stringToNodes "ac")
           , TwoBlocks Fraction (stringToNodes "adc") [ TwoBlocks Fraction (stringToNodes "a") [ Leaf <| Character 'a', OneBlock Subscript (stringToNodes "deets") ] ]
           ]
        ++ stringToNodes "dce"
        ++ [ Cursor
           ]


latex : Model -> String
latex model =
    toLatex model.rootBlock


editableQuelm : Model -> Html Msg
editableQuelm model =
    layout (stylesheet model.config) <|
        column Base
            []
            [ loadFont
            , renderBlock (baseContext model.config (Leaf <| Character 'a')) model.rootBlock
            ]


type DeleteDirection
    = DeleteLeft
    | DeleteRight


type Msg
    = Noop
    | Move MoveDirection
    | Select MoveDirection
    | Delete DeleteDirection
    | CharacterInserted Char
    | ExitBlock


type alias Model =
    { rootBlock : Block
    , config : Config
    }


defaultModel =
    { rootBlock = []
    , config = Config.default
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Move dir ->
            { model | rootBlock = moveCursor dir model.rootBlock }

        _ ->
            model


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


renderBlock : RenderContext -> Block -> Element Styles variation Msg
renderBlock context block =
    case block of
        [] ->
            el EmptySquare
                [ width (px <| fontSize context * 0.8)
                , height (px <| fontSize context)
                ]
                empty

        [ x ] ->
            renderNode (enter context x)

        _ ->
            let
                blockCenter =
                    blockCenterLine context block
            in
            row None
                [ width content, alignTop ]
                (List.map
                    (\child ->
                        let
                            childContext =
                                enter context child

                            rendered =
                                renderNode childContext

                            childCenter =
                                centerLine childContext
                        in
                        el None [ paddingTop (blockCenter - childCenter) ] rendered
                    )
                    block
                )


renderNode : RenderContext -> Element Styles variation Msg
renderNode context =
    let
        renderChildren =
            renderBlock context

        getChildrenHeight =
            getBlockHeight context

        rendered =
            case context.node of
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
                            getChildrenHeight block

                        renderedBlock =
                            renderChildren block
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
                        [ padInsides <| renderChildren numerator
                        , divider
                        , padInsides <| renderChildren denominator
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
        |> centerLineDiv


blockCenterLine : RenderContext -> Block -> Float
blockCenterLine context block =
    case block of
        [] ->
            fontSize context / 2

        [ x ] ->
            centerLine (enter context x)

        _ ->
            List.map (enter context >> centerLine) block
                |> List.maximum
                |> Maybe.withDefault 0


centerLine : RenderContext -> Float
centerLine context =
    --centerLine is the distance from the top of the middle of the block
    let
        getChildrenHeight =
            getBlockHeight context
    in
    case context.node of
        Cursor ->
            getHeight context / 2

        Leaf (Character _) ->
            getHeight context / 2

        OneBlock nodeType block ->
            let
                blockHeight =
                    getBlockHeight context block
            in
            case nodeType of
                Parens _ ->
                    let
                        parenOverlap =
                            --case  of
                            --Parens _ _ ->
                            --0.05 * getChildrenHeight block
                            --_ ->
                            0.2 * getChildrenHeight block
                    in
                    blockCenterLine context block + parenOverlap / 2

                Subscript ->
                    0

        TwoBlocks Fraction numerator denominator ->
            getChildrenHeight numerator



{--
  -        Diacritic _ _ ->
  -            0
  -
  -        Subscript _ ->
  -            0
  -
  -        Superscript block ->
  -            getChildrenHeight block
  -
  -        Subsuperscript _ super ->
  -            getChildrenHeight super
  -
  -        SquareRoot block ->
  -            blockCenterLine context block
  -
  --}


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


getBlockHeight : RenderContext -> Block -> Float
getBlockHeight context block =
    case block of
        [] ->
            fontSize context

        [ x ] ->
            getHeight (enter context x)

        _ ->
            List.map (enter context >> heightRect) block
                |> List.unzip
                |> mapBoth (List.maximum >> Maybe.withDefault 0)
                |> (\( distFromTop, distFromBottom ) -> distFromTop + distFromBottom)


getHeight : RenderContext -> Float
getHeight context =
    let
        getChildrenHeight =
            getBlockHeight context
    in
    case context.node of
        Cursor ->
            fontSize context

        Leaf (Character c) ->
            floor (RenderContext.fontBox context) |> toFloat

        OneBlock nodeType block ->
            let
                blockHeight =
                    getChildrenHeight block
            in
            case nodeType of
                Parens _ ->
                    blockHeight * 1.05

                Subscript ->
                    blockHeight

        TwoBlocks Fraction numerator denominator ->
            getChildrenHeight numerator + getChildrenHeight denominator + 1



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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs keyPressed


keyPressed : Keyboard.KeyCode -> Msg
keyPressed keyCode =
    let
        _ =
            Debug.log "keyCode" (toString keyCode)
    in
    case keyCode of
        37 ->
            Move Left

        38 ->
            Move Up

        39 ->
            Move Right

        40 ->
            Move Down

        _ ->
            Noop
