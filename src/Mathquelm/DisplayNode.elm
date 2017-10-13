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


type ParensType
    = Parentheses
    | Brackets
    | Curlies
    | Pipes


type DiacriticType
    = Hat
    | Dot
    | Bar
