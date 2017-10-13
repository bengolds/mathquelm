module Main exposing (..)

import Char
import Element exposing (..)
import Html exposing (Html)
import Keyboard
import Mathquelm exposing (mathquill)
import Mathquelm.Config as Config exposing (Config)
import Style exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { config : Config
    }


init : ( Model, Cmd Msg )
init =
    ( { config =
            Config.default
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Noop
    | ToggleCenterLine
    | ToggleBoxes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        Noop ->
            model

        ToggleCenterLine ->
            { model | config = Config.toggleCenterLineDisplay model.config }

        ToggleBoxes ->
            { model | config = Config.toggleBoxesDisplay model.config }
    , Cmd.none
    )



-- VIEW


type Styles
    = None


view : Model -> Html Msg
view model =
    layout stylesheet <|
        el None [] (html (mathquill model.config))


stylesheet : StyleSheet Styles variations
stylesheet =
    Style.styleSheet
        [ style None
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses
        (\code ->
            case Char.fromCode code of
                'c' ->
                    ToggleCenterLine

                'b' ->
                    ToggleBoxes

                _ ->
                    Noop
        )
