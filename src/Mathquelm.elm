module Mathquelm exposing (..)

import Element exposing (..)
import Html exposing (Html)
import Keyboard
import Keyboard.Extra exposing (Key(..))
import Mathquelm.Config as Config exposing (Config)
import Mathquelm.Digit as Digit
import Mathquelm.EditableMath exposing (..)
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
    | CharacterInserted Char
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

        _ ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.Extra.subscriptions
        , Keyboard.Extra.downs (keyDown model.pressedKeys)
        ]


keyDown : List Key -> Key -> Msg
keyDown pressedKeys key =
    if List.member Shift pressedKeys then
        case key of
            ArrowLeft ->
                Select Left

            ArrowUp ->
                Select Up

            ArrowRight ->
                Select Right

            ArrowDown ->
                Select Down

            _ ->
                Noop
    else
        case key of
            ArrowLeft ->
                Move Left

            ArrowUp ->
                Move Up

            ArrowRight ->
                Move Right

            ArrowDown ->
                Move Down

            _ ->
                Noop
