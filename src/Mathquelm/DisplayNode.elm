module Mathquelm.DisplayNode exposing (..)


type DisplayNode
    = Character Char
    | Block (List DisplayNode)
    | Parens ParensType DisplayNode
    | Diacritic DiacriticType DisplayNode
    | Fraction DisplayNode DisplayNode
    | Subscript DisplayNode
    | Superscript DisplayNode
    | Subsuperscript DisplayNode DisplayNode
    | SquareRoot DisplayNode


type ParensType
    = Parentheses
    | Brackets
    | Curlies
    | Pipes


type DiacriticType
    = Hat
    | Dot
    | Bar


toLatex : DisplayNode -> String
toLatex node =
    let
        wrapBrackets contents =
            "{" ++ contents ++ "}"
    in
    case node of
        Character c ->
            String.fromChar c

        Block nodes ->
            String.concat (List.map toLatex nodes)

        Parens parenType node ->
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
            "\\left" ++ left ++ toLatex node ++ "\\right" ++ right

        Diacritic diacriticType node ->
            ""

        Fraction numerator denominator ->
            "\\frac"
                ++ wrapBrackets (toLatex numerator)
                ++ wrapBrackets (toLatex denominator)

        Subscript node ->
            "_" ++ wrapBrackets (toLatex node)

        Superscript node ->
            "^" ++ wrapBrackets (toLatex node)

        Subsuperscript sub super ->
            "^"
                ++ wrapBrackets (toLatex sub)
                ++ "_"
                ++ wrapBrackets (toLatex super)

        SquareRoot node ->
            "\\sqrt" ++ wrapBrackets (toLatex node)



--| Subsuperscript DisplayNode DisplayNode
--| SquareRoot DisplayNode
