module Mathquelm.Math exposing (..)

-- Note on Selection:
-- Press SHIFT and ARROWS. As you cross expressions, they are moved into the Selection.
-- Here are some samples of SHIFT+LEFT on various expressions:
--
-- shiftLeft (RMul (Var "a") Hole)                  == Selection (Var "a")
-- shiftLeft (RMul (Mul (Var "a") (Var "b")) Hole)  == RMul (Var "a") (Selection (Var "b"))
-- shiftLeft (RMul (Var "a") (Selection (Var "b"))) == Selection (Mul (Var "a") (Var "b"))


type Math
    = Var String
    | Mul Math Math
    | Div Math Math
    | Cos Math
    | Empty


goRight : EditableMath -> EditableMath
goRight editable =
    case editable of
        LMul Hole math ->
            enterFromLMul math

        LMul eLeft math ->
            case exitRight eLeft of
                Just newLeft ->
                    RMul newLeft (enterFromLMul math)

                Nothing ->
                    LMul (goRight eLeft) math

        RMul math eRight ->
            RMul math (goRight eRight)

        TDiv eTop bottom ->
            case exitRight eTop of
                Just newTop ->
                    BDiv newTop (enterFromLeft bottom)

                Nothing ->
                    TDiv (goRight eTop) bottom

        BDiv top eBottom ->
            case exitRight eBottom of
                Just newBottom ->
                    RMul (Div top newBottom) Hole

                Nothing ->
                    BDiv top (goRight eBottom)

        _ ->
            editable


exitRight : EditableMath -> Maybe Math
exitRight editable =
    let
        _ =
            Debug.log "checking exit on" editable
    in
    case editable of
        RMul left Hole ->
            Just left

        RMul left eRight ->
            exitRight eRight
                |> Maybe.map (Mul left)

        Hole ->
            Just Empty

        {--
  -        ECos Hole ->
  -            Just (Cos Empty)
  -
  -        ECos contents ->
  -            exitRight contents
  -                |> Maybe.map Cos
  --}
        _ ->
            Nothing


enterFromLMul : Math -> EditableMath
enterFromLMul math =
    case math of
        Var name ->
            RMul (Var name) Hole

        Mul left right ->
            LMul (enterFromLMul left) right

        Div top bottom ->
            TDiv (LMul Hole top) bottom

        Cos x ->
            ECos (LMul Hole x)

        Empty ->
            Hole


enterFromLeft : Math -> EditableMath
enterFromLeft math =
    case math of
        Var name ->
            LMul Hole (Var name)

        Mul left right ->
            LMul (enterFromLeft left) right

        Div top bottom ->
            LMul Hole (Div top bottom)

        Cos x ->
            LMul Hole (Cos x)

        Empty ->
            Hole


collapse : EditableMath -> Maybe Math
collapse editable =
    case editable of
        LMul Hole right ->
            Just right

        RMul left Hole ->
            Just left

        LMul eLeft right ->
            collapse eLeft
                |> Maybe.map (flip Mul right)

        RMul left eRight ->
            collapse eRight
                |> Maybe.map (Mul left)

        TDiv eTop bottom ->
            collapse eTop
                |> Maybe.map (flip Div bottom)

        BDiv top eBottom ->
            collapse eBottom
                |> Maybe.map (Div top)

        ECos eContents ->
            collapse eContents
                |> Maybe.map Cos

        Hole ->
            Nothing

        LSel contents ->
            Just contents

        RSel contents ->
            Just contents


toLatex : EditableMath -> String
toLatex editable =
    case editable of
        LMul eLeft right ->
            toLatex eLeft ++ toLatexMath right

        RMul left eRight ->
            toLatexMath left ++ toLatex eRight

        TDiv eTop bottom ->
            frac (toLatex eTop) (toLatexMath bottom)

        BDiv top eBottom ->
            frac (toLatexMath top) (toLatex eBottom)

        ECos x ->
            "\\cos" ++ toLatex x

        Hole ->
            ""

        RSel math ->
            toLatexMath math

        LSel math ->
            toLatexMath math


toLatexMath : Math -> String
toLatexMath math =
    case math of
        Var name ->
            name

        Mul left right ->
            toLatexMath left ++ toLatexMath right

        Div top bottom ->
            frac (toLatexMath top) (toLatexMath bottom)

        Cos x ->
            "\\cos" ++ toLatexMath x

        Empty ->
            ""


frac : String -> String -> String
frac top bottom =
    "\\frac{" ++ top ++ "}{" ++ bottom ++ "}"



-- List-style


type Expr
    = BinaryOp Term Op
    | Times Term Expr
    | End Term



--type CursorExpr
--=


type Op
    = Plus Expr
    | Times Expr


type Term
    = Variable String
    | Number Float
    | Fraction Expr Expr
    | Cos Expr
    | Parentheses Expr


toLatex : Expr -> String
toLatex node =
    case node of
        BinaryOp term op ->
            toLatexTerm term ++ toLatexOp op

        Times term expr ->
            toLatexTerm term ++ toLatexExpr expr

        End term ->
            toLatexTerm term


toLatexTerm : Term -> String
toLatexTerm term =
    case term of
        Variable name ->
            name

        Number num ->
            toString num

        Fraction top bottom ->
            "\\frac{" ++ toLatex top ++ "}{" ++ toLatex bottom ++ "}"

        Cos operand ->
            "\\cos{" ++ toLatex operand ++ "}"

        Parentheses contents ->
            "\\left(" ++ toLatex contents ++ "\\right)"


toLatexOp : Op -> String
toLatexOp op =
    case op of
        Plus expr ->
            "+" ++ toLatexExpr expr

        Times expr ->
            "*" ++ toLatexExpr expr
