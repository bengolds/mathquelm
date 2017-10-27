module Mathquelm.DisplayTree exposing (..)


type TreeObject
    = Node DisplayNode
    | Block DisplayBlock


type alias DisplayBlock =
    List DisplayNode


type DisplayNode
    = Cursor
    | Leaf LeafType
    | OneBlock OneBlockType DisplayBlock
    | TwoBlocks TwoBlocksType DisplayBlock DisplayBlock


type LeafType
    = Character Char


type OneBlockType
    = Parens ParensType
    | Subscript


type TwoBlocksType
    = Fraction



{--
      -| Diacritic DiacriticType Block
      -| Subscript Block
      -| Superscript Block
      -| Subsuperscript Block Block
      -| SquareRoot Block
      --}


type ParensType
    = Parentheses
    | Brackets
    | Curlies
    | Pipes


type DiacriticType
    = Hat
    | Dot
    | Bar


toLatex : TreeObject -> String
toLatex obj =
    let
        wrapBrackets contents =
            "{" ++ contents ++ "}"
    in
    case obj of
        Block block ->
            mapChildren toLatex block
                |> String.concat

        Node node ->
            case node of
                Cursor ->
                    ""

                Leaf (Character c) ->
                    String.fromChar c

                OneBlock (Parens parenType) block ->
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
                    "\\left" ++ left ++ toLatex (Block block) ++ "\\right" ++ right

                OneBlock Subscript block ->
                    "_" ++ wrapBrackets (toLatex (Block block))

                TwoBlocks Fraction numerator denominator ->
                    "\\frac"
                        ++ wrapBrackets (toLatex (Block numerator))
                        ++ wrapBrackets (toLatex (Block denominator))


mapChildren : (TreeObject -> a) -> DisplayBlock -> List a
mapChildren fn block =
    List.map (Node >> fn) block


mapFirstBlock fn node =
    case node of
        OneBlock nodeType block ->
            OneBlock nodeType (fn block)

        TwoBlocks nodeType block1 block2 ->
            TwoBlocks nodeType (fn block1) block2

        _ ->
            node


mapSecondBlock fn node =
    case node of
        TwoBlocks nodeType block1 block2 ->
            TwoBlocks nodeType block1 (fn block2)

        _ ->
            node



{--
  -        Diacritic diacriticType block ->
  -            ""
  -
  -        Subscript block ->
  -            "_" ++ wrapBrackets (toLatex block)
  -
  -        Superscript block ->
  -            "^" ++ wrapBrackets (toLatex block)
  -
  -        Subsuperscript sub super ->
  -            "^"
  -                ++ wrapBrackets (toLatex super)
  -                ++ "_"
  -                ++ wrapBrackets (toLatex sub)
  -
  -        SquareRoot block ->
  -            "\\sqrt" ++ wrapBrackets (toLatex block)
  -
  --}
