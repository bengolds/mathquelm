module Mathquelm.CursorTree exposing (..)

import Mathquelm.RawTree exposing (..)


-- {{{ Basic Display Tree


sampleTree =
    TwoBlocks Fraction
        [ OneBlock (BalancedDelimiters Parentheses) <|
            [ Character 'w', Character 'o', Character 'r', Character 'l', Character 'd' ]
        ]
        [ Character 'h', Character 'e', Character 'l', Character 'l', Character 'o' ]


toLatex : CursorTreeItem -> String
toLatex obj =
    let
        wrapBrackets contents =
            "{" ++ contents ++ "}"

        toLatexBlock : Block -> String
        toLatexBlock block =
            List.map toLatex block
                |> String.concat
    in
    case obj of
        Character c ->
            String.fromChar c

        OneBlock (BalancedDelimiters parenType) block ->
            let
                ( left, right ) =
                    case parenType of
                        Parentheses ->
                            ( "(", ")" )

                        Brackets ->
                            ( "[", "]" )

                        Curlies ->
                            ( "{", "}" )

                        Pipes ->
                            ( "|", "|" )
            in
            "\\left" ++ left ++ toLatexBlock block ++ "\\right" ++ right

        OneBlock Subscript block ->
            "_" ++ wrapBrackets (toLatexBlock block)

        TwoBlocks Fraction numerator denominator ->
            "\\frac"
                ++ wrapBrackets (toLatexBlock numerator)
                ++ wrapBrackets (toLatexBlock denominator)



-- }}}
-- {{{ BlockCursors


type alias BlockCursor =
    { left : Block
    , right : Block
    }


stitch : BlockCursor -> Block
stitch { left, right } =
    List.reverse left ++ right


moveLeft : BlockCursor -> Maybe BlockCursor
moveLeft { left, right } =
    case left of
        x :: xs ->
            Just { left = xs, right = x :: right }

        _ ->
            Nothing


moveRight : BlockCursor -> Maybe BlockCursor
moveRight { left, right } =
    case right of
        x :: xs ->
            Just { left = x :: left, right = xs }

        _ ->
            Nothing


mapCursor : (BlockCursor -> BlockCursor) -> DisplayCrumb -> DisplayCrumb
mapCursor fn crumb =
    case crumb of
        OneBlockCrumb nodeType cursor ->
            OneBlockCrumb nodeType (fn cursor)

        TwoBlocksFirstCrumb nodeType second cursor ->
            TwoBlocksFirstCrumb nodeType second (fn cursor)

        TwoBlocksSecondCrumb nodeType first cursor ->
            TwoBlocksSecondCrumb nodeType first (fn cursor)


maybeMapCursor : (BlockCursor -> Maybe BlockCursor) -> DisplayCrumb -> Maybe DisplayCrumb
maybeMapCursor fn crumb =
    case crumb of
        OneBlockCrumb nodeType cursor ->
            Maybe.map (OneBlockCrumb nodeType) (fn cursor)

        TwoBlocksFirstCrumb nodeType second cursor ->
            Maybe.map (TwoBlocksFirstCrumb nodeType second) (fn cursor)

        TwoBlocksSecondCrumb nodeType first cursor ->
            Maybe.map (TwoBlocksSecondCrumb nodeType first) (fn cursor)


insertRight : CursorTreeItem -> BlockCursor -> BlockCursor
insertRight node { left, right } =
    { left = left, right = node :: right }



-- }}}
-- {{{ Zippers


type DisplayCrumb
    = OneBlockCrumb OneBlockType BlockCursor
    | TwoBlocksFirstCrumb TwoBlocksType Block BlockCursor
    | TwoBlocksSecondCrumb TwoBlocksType Block BlockCursor


type alias DisplayZipper =
    ( DisplayCrumb, List DisplayCrumb )


getCursor : DisplayCrumb -> BlockCursor
getCursor crumb =
    case crumb of
        OneBlockCrumb _ cursor ->
            cursor

        TwoBlocksFirstCrumb _ _ cursor ->
            cursor

        TwoBlocksSecondCrumb _ _ cursor ->
            cursor


baseZipper : CursorTreeItem -> Maybe DisplayZipper
baseZipper tree =
    case tree of
        OneBlock nodeType block ->
            Just ( OneBlockCrumb nodeType { left = [], right = block }, [] )

        TwoBlocks nodeType first second ->
            Just ( TwoBlocksFirstCrumb nodeType second { left = [], right = first }, [] )

        _ ->
            Nothing


reconstruct : DisplayCrumb -> CursorTreeItem
reconstruct crumb =
    case crumb of
        OneBlockCrumb nodeType cursor ->
            OneBlock nodeType (stitch cursor)

        TwoBlocksFirstCrumb nodeType second cursor ->
            TwoBlocks nodeType (stitch cursor) second

        TwoBlocksSecondCrumb nodeType first cursor ->
            TwoBlocks nodeType first (stitch cursor)


zipLeft : DisplayZipper -> Maybe DisplayZipper
zipLeft ( focused, path ) =
    maybeMapCursor moveLeft focused
        |> Maybe.map (\moved -> ( moved, path ))


zipRight : DisplayZipper -> Maybe DisplayZipper
zipRight ( focused, path ) =
    maybeMapCursor moveRight focused
        |> Maybe.map (\moved -> ( moved, path ))


zipUp : DisplayZipper -> Maybe DisplayZipper
zipUp ( focused, path ) =
    case path of
        x :: xs ->
            Just ( mapCursor (insertRight (reconstruct focused)) x, xs )

        [] ->
            Nothing


zipDownRight : DisplayZipper -> Maybe DisplayZipper
zipDownRight ( focused, path ) =
    let
        { left, right } =
            getCursor focused
    in
    case right of
        x :: xs ->
            let
                newPath =
                    mapCursor (always { left = left, right = xs }) focused :: path
            in
            case x of
                OneBlock nodeType block ->
                    Just ( OneBlockCrumb nodeType { left = [], right = block }, newPath )

                TwoBlocks nodeType first second ->
                    Just ( TwoBlocksFirstCrumb nodeType second { left = [], right = first }, newPath )

                _ ->
                    Nothing

        _ ->
            Nothing


top : DisplayZipper -> CursorTreeItem
top ( focused, path ) =
    case path of
        [] ->
            reconstruct focused

        x :: xs ->
            ( mapCursor (insertRight (reconstruct focused)) x, xs )
                |> top



-- }}}
