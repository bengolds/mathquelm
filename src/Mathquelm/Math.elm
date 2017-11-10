module Mathquelm.Math exposing (..)


type Math
    = Var String
    | Num Int
    | Mul Math Math
    | Plus Math Math
    | Div Math Math
    | Cos Math
    | Empty


toLatex : Math -> String
toLatex math =
    -- This doesn't handle OOO quite right
    case math of
        Var name ->
            name

        Num num ->
            toString num

        Plus left right ->
            toLatex left ++ "+" ++ toLatex right

        Mul left right ->
            toLatex left ++ toLatex right

        Div top bottom ->
            frac (toLatex top) (toLatex bottom)

        Cos x ->
            "\\cos " ++ toLatex x

        Empty ->
            ""


frac : String -> String -> String
frac top bottom =
    "\\frac{" ++ top ++ "}{" ++ bottom ++ "}"
