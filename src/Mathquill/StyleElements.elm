module Mathquill.StyleElements
    exposing
        ( onEdit
        , onEnter
        , onMoveOutOf
        , onSelectOutOf
        , onDeleteOutOf
        , onUpOutOf
        , onDownOutOf
        , spaceBehavesLikeTab
        , restrictMismatchedBrackets
        , sumStartsWithNEquals
        , supSubsRequireOperand
        , autoSubscriptNumerals
        , leftRightIntoCmdGoes
        , charsThatBreakOutOfSupSub
        , autoCommands
        , autoOperatorNames
        , mathField
        , staticMath
        )

import Mathquill.Common exposing (decodeDirection, ExitDirection(..), NavigationDirection(..))
import Element exposing (Element, node, text, button)
import Element.Keyed as Keyed
import Element.Events exposing (on)
import Element.Attributes exposing (class, property, attribute)
import Json.Decode as Decode


onEdit : (String -> msg) -> Element.Attribute variation msg
onEdit msg =
    on "edit" <| Decode.map msg <| Decode.field "value" Decode.string


onEnter : msg -> Element.Attribute variation msg
onEnter msg =
    on "enter" <| Decode.succeed msg


onMoveOutOf : (ExitDirection -> msg) -> Element.Attribute a msg
onMoveOutOf msg =
    on "moveOutOf" <| Decode.map msg <| decodeDirection


onSelectOutOf : (ExitDirection -> msg) -> Element.Attribute a msg
onSelectOutOf msg =
    on "selectOutOf" <| Decode.map msg <| decodeDirection


onDeleteOutOf : (ExitDirection -> msg) -> Element.Attribute a msg
onDeleteOutOf msg =
    on "deleteOutOf" <| Decode.map msg <| decodeDirection


onUpOutOf : msg -> Element.Attribute a msg
onUpOutOf msg =
    on "upOutOf" <| Decode.succeed msg


onDownOutOf : msg -> Element.Attribute a msg
onDownOutOf msg =
    on "downOutOf" <| Decode.succeed msg


emptyAttribute_ : Element.Attribute a msg
emptyAttribute_ =
    attribute "empty-attribute-blank-ignore" ""


boolAttribute_ : String -> (Bool -> Element.Attribute a msg)
boolAttribute_ attrName =
    (\value ->
        if value then
            attribute attrName ""
        else
            emptyAttribute_
    )


spaceBehavesLikeTab : Bool -> Element.Attribute a msg
spaceBehavesLikeTab =
    boolAttribute_ "space-behaves-like-tab"


restrictMismatchedBrackets : Bool -> Element.Attribute a msg
restrictMismatchedBrackets =
    boolAttribute_ "restrict-mismatched-brackets"


sumStartsWithNEquals : Bool -> Element.Attribute a msg
sumStartsWithNEquals =
    boolAttribute_ "sum-starts-with-n-equals"


supSubsRequireOperand : Bool -> Element.Attribute a msg
supSubsRequireOperand =
    boolAttribute_ "sup-subs-require-operand"


autoSubscriptNumerals : Bool -> Element.Attribute a msg
autoSubscriptNumerals =
    boolAttribute_ "auto-subscript-numerals"


leftRightIntoCmdGoes : NavigationDirection -> Element.Attribute a msg
leftRightIntoCmdGoes dir =
    case dir of
        Up ->
            attribute "left-right-into-cmd-goes" "up"

        Down ->
            attribute "left-right-into-cmd-goes" "down"

        Default ->
            emptyAttribute_


stringAttribute_ : String -> String -> Element.Attribute a msg
stringAttribute_ name string =
    case string of
        "" ->
            emptyAttribute_

        _ ->
            attribute name string


charsThatBreakOutOfSupSub : String -> Element.Attribute a msg
charsThatBreakOutOfSupSub =
    stringAttribute_ "chars-that-break-out-of-sup-sub"


autoCommands : String -> Element.Attribute a msg
autoCommands =
    stringAttribute_ "auto-commands"


autoOperatorNames : String -> Element.Attribute a msg
autoOperatorNames =
    stringAttribute_ "auto-operator-names"


mathField : style -> List (Element.Attribute variation msg) -> Element style variation msg
mathField style attributes =
    Element.el style [] <|
        Keyed.row style (List.append attributes [ class "elm-mq-edit" ]) []


staticMath : style -> List (Element.Attribute variation msg) -> String -> Element style variation msg
staticMath style attributes content =
    Element.el style [] <|
        Keyed.row style
            (List.append attributes [ class "elm-mq-static", attribute "content" content ])
            []
