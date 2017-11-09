module Mathquelm.Cursor exposing (..)

import Mathquelm.RawTree exposing (..)


blah =
    2



{--
  -type MathCrumb
  -    = TimesLeftCrumb Math
  -    | TimesRightCrumb Math
  -    | CosCrumb
  -    | ParenthesesCrumb
  -
  -
  -type alias MathZipper =
  -    ( Math, List MathCrumb )
  -
  -
  -type Side
  -    = Left
  -    | Right
  -
  -
  -type alias MathCursor =
  -    ( MathZipper, Side )
  -
  -
  -zipUp : MathZipper -> Maybe MathZipper
  -zipUp ( focused, path ) =
  -    case path of
  -        parent :: xs ->
  -            Just ( reconstruct parent focused, xs )
  -
  -        _ ->
  -            Nothing
  -
  -
  -reconstruct : MathCrumb -> Math -> Math
  -reconstruct crumb focused =
  -    case crumb of
  -        TimesLeftCrumb right ->
  -            Times focused right
  -
  -        TimesRightCrumb left ->
  -            Times left focused
  -
  -        CosCrumb ->
  -            Cos focused
  -
  -        ParenthesesCrumb ->
  -            Parentheses focused
  --}
