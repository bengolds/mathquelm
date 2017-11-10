module Mathquelm exposing (..)

import Element exposing (..)
import Html exposing (Html)
import Keyboard
import List.Extra as List
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
                    Plus
                else
                    Digit.fromChar char
                        |> Maybe.map Digit
                        |> Maybe.withDefault (Var (String.fromChar char))
            )


sampleTree : EditableMath
sampleTree =
    startEditing <|
        str "abc+"
            ++ [ Cos (str "xy+z")
               , Div [ Cos (str "hi"), Digit Digit.Nine ]
                    (str "lo")
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


type alias Model =
    { tree : EditableMath
    , config : Config
    }


defaultModel : Model
defaultModel =
    { tree = startEditing []
    , config = Config.default
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Move Right ->
            { model
                | tree =
                    goRight model.tree
                        |> Maybe.withDefault model.tree
            }

        Move Left ->
            { model
                | tree =
                    goLeft model.tree
                        |> Maybe.withDefault model.tree
            }

        Move Up ->
            { model
                | tree =
                    goUp model.tree
                        |> Maybe.withDefault model.tree
            }

        Move Down ->
            { model
                | tree =
                    goDown model.tree
                        |> Maybe.withDefault model.tree
            }

        --{ model | cursor = moveCursor dir model.rootBlock model.cursor }
        _ ->
            model



{--
  -        Diacritic _ _ ->
  -            0
  -
  -        Subscript _ ->
  -            0
  -
  -        Superscript block ->
  -            getChildrenHeight block
  -
  -        Subsuperscript _ super ->
  -            getChildrenHeight super
  -
  -        SquareRoot block ->
  -            blockCenterLine context block
  -
  --}
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs keyPressed


keyPressed : Keyboard.KeyCode -> Msg
keyPressed keyCode =
    let
        _ =
            Debug.log "keyCode" (toString keyCode)
    in
    case keyCode of
        37 ->
            Move Left

        38 ->
            Move Up

        39 ->
            Move Right

        40 ->
            Move Down

        _ ->
            Noop
