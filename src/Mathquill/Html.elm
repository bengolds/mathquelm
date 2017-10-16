module Mathquill.Html
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
import Html exposing (Html, div, text, button)
import Html.Keyed
import Html.Events exposing (on)
import Html.Attributes exposing (class, property, attribute)
import Json.Decode as Decode


onEdit : (String -> msg) -> Html.Attribute msg
onEdit msg =
    on "edit" <| Decode.map msg <| Decode.field "value" Decode.string


onEnter : msg -> Html.Attribute msg
onEnter msg =
    on "enter" <| Decode.succeed msg


onMoveOutOf : (ExitDirection -> msg) -> Html.Attribute msg
onMoveOutOf msg =
    on "moveOutOf" <| Decode.map msg <| decodeDirection


onSelectOutOf : (ExitDirection -> msg) -> Html.Attribute msg
onSelectOutOf msg =
    on "selectOutOf" <| Decode.map msg <| decodeDirection


onDeleteOutOf : (ExitDirection -> msg) -> Html.Attribute msg
onDeleteOutOf msg =
    on "deleteOutOf" <| Decode.map msg <| decodeDirection


onUpOutOf : msg -> Html.Attribute msg
onUpOutOf msg =
    on "upOutOf" <| Decode.succeed msg


onDownOutOf : msg -> Html.Attribute msg
onDownOutOf msg =
    on "downOutOf" <| Decode.succeed msg


emptyAttribute_ : Html.Attribute msg
emptyAttribute_ =
    attribute "empty-attribute-blank-ignore" ""


boolAttribute_ : String -> (Bool -> Html.Attribute msg)
boolAttribute_ attrName =
    (\value ->
        if value then
            attribute attrName ""
        else
            emptyAttribute_
    )


spaceBehavesLikeTab : Bool -> Html.Attribute msg
spaceBehavesLikeTab =
    boolAttribute_ "space-behaves-like-tab"


restrictMismatchedBrackets : Bool -> Html.Attribute msg
restrictMismatchedBrackets =
    boolAttribute_ "restrict-mismatched-brackets"


sumStartsWithNEquals : Bool -> Html.Attribute msg
sumStartsWithNEquals =
    boolAttribute_ "sum-starts-with-n-equals"


supSubsRequireOperand : Bool -> Html.Attribute msg
supSubsRequireOperand =
    boolAttribute_ "sup-subs-require-operand"


autoSubscriptNumerals : Bool -> Html.Attribute msg
autoSubscriptNumerals =
    boolAttribute_ "auto-subscript-numerals"


leftRightIntoCmdGoes : NavigationDirection -> Html.Attribute msg
leftRightIntoCmdGoes dir =
    case dir of
        Up ->
            attribute "left-right-into-cmd-goes" "up"

        Down ->
            attribute "left-right-into-cmd-goes" "down"

        Default ->
            emptyAttribute_


stringAttribute_ : String -> String -> Html.Attribute msg
stringAttribute_ name string =
    case string of
        "" ->
            emptyAttribute_

        _ ->
            attribute name string


charsThatBreakOutOfSupSub : String -> Html.Attribute msg
charsThatBreakOutOfSupSub =
    stringAttribute_ "chars-that-break-out-of-sup-sub"


autoCommands : String -> Html.Attribute msg
autoCommands =
    stringAttribute_ "auto-commands"


autoOperatorNames : String -> Html.Attribute msg
autoOperatorNames =
    stringAttribute_ "auto-operator-names"


mathField : List (Html.Attribute msg) -> Html msg
mathField attributes =
    Html.Keyed.node "div"
        (List.append attributes [ class "elm-mq-edit" ])
        []


staticMath : List (Html.Attribute msg) -> String -> Html msg
staticMath attributes content =
    Html.Keyed.node "div"
        (List.append attributes [ class "elm-mq-static", attribute "content" content ])
        []
