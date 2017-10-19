module Mathquelm.DisplayNode exposing (..)


type DisplayNode
    = Cursor
    | Leaf LeafType
    | OneBlock OneBlockType Block
    | TwoBlocks TwoBlocksType Block Block


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


type alias Block =
    List DisplayNode


type ParensType
    = Parentheses
    | Brackets
    | Curlies
    | Pipes


type DiacriticType
    = Hat
    | Dot
    | Bar


toLatex : Block -> String
toLatex nodes =
    List.map toLatexNode nodes
        |> String.concat


toLatexNode : DisplayNode -> String
toLatexNode node =
    let
        wrapBrackets contents =
            "{" ++ contents ++ "}"
    in
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
            "\\left" ++ left ++ toLatex block ++ "\\right" ++ right

        OneBlock Subscript block ->
            "_" ++ wrapBrackets (toLatex block)

        TwoBlocks Fraction numerator denominator ->
            "\\frac"
                ++ wrapBrackets (toLatex numerator)
                ++ wrapBrackets (toLatex denominator)



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
