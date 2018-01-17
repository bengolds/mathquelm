module Mathquelm exposing (..)

import Char
import Element exposing (..)
import Html exposing (Html)
import Keyboard.Extra exposing (Key(..))
import Mathquelm.Config as Config exposing (Config)
import Mathquelm.Digit as Digit
import Mathquelm.Edit.Command as EditCommand exposing (..)
import Mathquelm.Edit.EditableMath exposing (..)
import Mathquelm.Insert exposing (Insertion(..), insert)
import Mathquelm.Math as Math
import Mathquelm.Render as Render
import Mathquelm.Styles exposing (..)
import Mathquelm.Undo as Undo exposing (StateWithHistory)
import Mathquelm.Util exposing (..)


str : String -> EditCommand.Block
str val =
    String.toList val
        |> List.map
            (\char ->
                if char == '+' then
                    EditCommand.Plus
                else
                    Digit.fromChar char
                        |> Maybe.map Digit
                        |> Maybe.withDefault (Var (String.fromChar char))
            )


sampleTree : MathBeingEdited
sampleTree =
    startEditing <|
        str "abc+"
            ++ [ Cos (str "xy+z")
               , Div [ Cos (str "hi"), Digit Digit.Nine ]
                    [ Div (str "lo") (str "lower") ]
               ]
            ++ str "de++"


latex : Model -> String
latex model =
    Math.toLatex (toMath <| getTree model)


editableQuelm : Model -> Html Msg
editableQuelm model =
    layout (stylesheet model.config) <|
        column Base
            []
            [ loadFont
            , Render.render model.config (Render.fromEditable <| getTree model)
            ]


type DeleteDirection
    = DeleteLeft
    | DeleteRight


type MoveDirection
    = Left
    | Right
    | Up
    | Down


type Msg
    = Noop
    | Move MoveDirection
    | Select MoveDirection
    | Delete DeleteDirection
    | Insert Insertion
    | Undo
    | Redo
      --| ExitBlock
    | KeyMsg Keyboard.Extra.Msg
    | Unselect
    | SelectAll


type alias Model =
    { stateHistory : StateWithHistory MathBeingEdited
    , config : Config
    , pressedKeys : List Key
    }


getTree model =
    model.stateHistory.currentState


defaultModel : Model
defaultModel =
    { stateHistory = Undo.freshHistory sampleTree
    , config = Config.default
    , pressedKeys = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        KeyMsg keyMsg ->
            { model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys }

        _ ->
            { model
                | stateHistory =
                    updateState msg model.config model.stateHistory
                        |> Maybe.withDefault model.stateHistory
            }


updateState :
    Msg
    -> Config
    -> StateWithHistory MathBeingEdited
    -> Maybe (StateWithHistory MathBeingEdited)
updateState msg config stateHistory =
    let
        tryEdit editFunc history =
            editFunc history.currentState
                |> Maybe.map
                    (\edited ->
                        Undo.setState edited history
                    )
    in
    stateHistory
        |> (case msg of
                Undo ->
                    Undo.undo

                Redo ->
                    Undo.redo

                Move Right ->
                    tryEdit goRight

                Move Left ->
                    tryEdit goLeft

                Move Up ->
                    tryEdit goUp

                Move Down ->
                    tryEdit goDown

                Select Right ->
                    tryEdit selectRight

                Select Left ->
                    tryEdit selectLeft

                Select Up ->
                    tryEdit selectUp

                Select Down ->
                    tryEdit selectDown

                SelectAll ->
                    tryEdit selectAll

                Unselect ->
                    tryEdit unselect

                Delete DeleteLeft ->
                    Undo.recordState
                        >> tryEdit deleteLeft

                Delete DeleteRight ->
                    Undo.recordState
                        >> tryEdit deleteRight

                Insert insertion ->
                    Undo.recordState
                        >> Undo.map (insert config insertion)
                        >> Just

                _ ->
                    always Nothing
           )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.Extra.subscriptions
        , Keyboard.Extra.downs (keyDown model.pressedKeys)
        ]


getAsChar : List Key -> Key -> Maybe Msg
getAsChar pressedKeys key =
    let
        char =
            Keyboard.Extra.toCode key
                |> Char.fromCode

        isAlpha x =
            Char.isUpper x || Char.isUpper x
    in
    if isAlpha char then
        Just
            (Insert
                (InsertVar
                    (if isShiftDown pressedKeys then
                        Char.toUpper char
                            |> String.fromChar
                     else
                        Char.toLower char
                            |> String.fromChar
                    )
                )
            )
    else if Char.isDigit char then
        Digit.fromChar char
            |> Maybe.map (InsertDigit >> Insert)
    else
        Nothing


getAsOp pressedKeys key =
    if (key == Add) || (isShiftDown pressedKeys && key == Equals) then
        Just (Insert InsertPlus)
    else if key == Slash || key == Divide then
        Just (Insert InsertFraction)
    else
        Nothing


isShiftDown : List Key -> Bool
isShiftDown pressedKeys =
    List.member Shift pressedKeys


isCtrlDown : List Key -> Bool
isCtrlDown pressedKeys =
    List.member Control pressedKeys


isSuperDown : List Key -> Bool
isSuperDown pressedKeys =
    List.member Super pressedKeys


keyDown : List Key -> Key -> Msg
keyDown pressedKeys key =
    let
        _ =
            Debug.log "pressedKey" key
    in
    case key of
        ArrowLeft ->
            if isShiftDown pressedKeys then
                Select Left
            else
                Move Left

        ArrowUp ->
            if isShiftDown pressedKeys then
                Select Up
            else
                Move Up

        ArrowRight ->
            if isShiftDown pressedKeys then
                Select Right
            else
                Move Right

        ArrowDown ->
            if isShiftDown pressedKeys then
                Select Down
            else
                Move Down

        CharZ ->
            -- TODO: Detect whether OS X or not
            if
                isCtrlDown pressedKeys
                    || isSuperDown pressedKeys
            then
                if isShiftDown pressedKeys then
                    Redo
                else
                    Undo
            else
                Noop

        CharA ->
            if
                isCtrlDown pressedKeys
                    || isSuperDown pressedKeys
            then
                SelectAll
            else
                Noop

        BackSpace ->
            Delete DeleteLeft

        Escape ->
            Unselect

        Keyboard.Extra.Delete ->
            Delete DeleteRight

        _ ->
            getAsChar pressedKeys key
                |> orElse (getAsOp pressedKeys key)
                |> Maybe.withDefault Noop
