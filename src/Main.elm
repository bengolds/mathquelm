module Main exposing (..)

import Element exposing (..)
import Html exposing (Html)
import Mathquill exposing (mathquill)
import Style exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    {}


initialModel : Model
initialModel =
    {}



-- UPDATE


type Msg
    = Noop


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            {}



-- VIEW


type Styles
    = None


view : Model -> Html Msg
view model =
    layout stylesheet <|
        el None [] (html mathquill)


stylesheet : StyleSheet Styles variations
stylesheet =
    Style.styleSheet
        [ style None
            []
        ]
