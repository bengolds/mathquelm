module Mathquelm exposing (..)

import Char
import Element exposing (..)
import Html exposing (Html)
import Keyboard
import Keyboard.Extra exposing (Key(..))
import Mathquelm.Config as Config exposing (Config)
import Mathquelm.Digit as Digit
import Mathquelm.EditableMath exposing (..)
import Mathquelm.Insert exposing (Insertion(..), insert)
import Mathquelm.Math as Math
import Mathquelm.Render as Render
import Mathquelm.Styles exposing (..)


str : String -> Block
str val =
    String.toList val
        |> List.map
            (\char ->
                if char == '+' then
                    Mathquelm.EditableMath.Plus
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
    Math.toLatex (toMath model.tree)


editableQuelm : Model -> Html Msg
editableQuelm model =
    layout (stylesheet model.config) <|
        column Base
            []
            [ loadFont
            , Render.render model.config (Render.fromEditable model.tree)
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
    | ExitBlock
    | KeyMsg Keyboard.Extra.Msg


type alias Model =
    { tree : MathBeingEdited
    , config : Config
    , pressedKeys : List Key
    }


defaultModel : Model
defaultModel =
    { tree = startEditing []
    , config = Config.default
    , pressedKeys = []
    }


update : Msg -> Model -> Model
update msg model =
    let
        tryEdit editFunc =
            { model
                | tree =
                    editFunc model.tree
                        |> Maybe.withDefault model.tree
            }
    in
    case msg of
        KeyMsg keyMsg ->
            { model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys }

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

        Delete DeleteLeft ->
            tryEdit deleteLeft

        Delete DeleteRight ->
            tryEdit deleteRight

        Insert insertion ->
            { model | tree = insert insertion model.tree }

        _ ->
            model



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

        BackSpace ->
            Delete DeleteLeft

        Keyboard.Extra.Delete ->
            Delete DeleteRight

        _ ->
            getAsChar pressedKeys key
                |> orElse (getAsOp pressedKeys key)
                |> Maybe.withDefault Noop
