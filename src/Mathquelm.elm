module Mathquelm exposing (..)

import Element exposing (..)
import Html exposing (Html)
import Keyboard
import List.Extra as List
import Mathquelm.Config as Config exposing (Config)
import Mathquelm.Cursor exposing (..)
import Mathquelm.RawTree exposing (..)
import Mathquelm.Render as Render
import Mathquelm.Styles exposing (..)


str : String -> Math
str val =
    String.toList val
        |> List.map (String.fromChar >> Var)
        |> List.foldl1 Mul
        |> Maybe.withDefault (Var "")


sampleTree : EditableMath
sampleTree =
    LMul
        Hole
        (Mul
            (str "abc")
            (Mul
                (str "de")
                (Div
                    (str "fgh")
                    (Div
                        (str "zxy")
                        (Mul (str "j") (str "klmno"))
                    )
                )
            )
        )


latex : Model -> String
latex model =
    toLatex model.tree


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
    { tree = Hole
    , config = Config.default
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Move Right ->
            { model | tree = goRight model.tree }

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
        39 ->
            Move Right

        {--
  -        37 ->
  -            Move Left
  -
  -        38 ->
  -            Move Up
  -
  -        39 ->
  -            Move Right
  -
  -        40 ->
  -            Move Down
  --}
        _ ->
            Noop
