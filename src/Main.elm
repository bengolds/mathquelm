port module Main exposing (..)

import Char
import Element exposing (..)
import Element.Attributes exposing (id)
import Html exposing (Html)
import Keyboard
import Mathquelm exposing (mathquill)
import Mathquelm.Config as Config exposing (Config)
import Mathquill.StyleElements as Mathquill
import Style exposing (..)
import Style.Font as Font


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
    let
        _ =
            Debug.log "latex" Mathquelm.latex
    in
    ( { config =
            Config.default
      }
    , katexOut Mathquelm.latex
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
    | Mathquill


view : Model -> Html Msg
view model =
    layout stylesheet <|
        column None
            []
            [ html (mathquill model.config)
            , katex
            , Mathquill.staticMath Mathquill [] Mathquelm.latex
            ]


stylesheet : StyleSheet Styles variations
stylesheet =
    let
        defaultConfig =
            Config.default

        fontSize =
            defaultConfig.baseFontSize
    in
    Style.styleSheet
        [ style None
            []
        , style Mathquill
            [ Font.size fontSize
            ]
        ]


katex : Element Styles variation Msg
katex =
    el None [ id "katex" ] empty



-- SUBSCRIPTIONS


port katexOut : String -> Cmd msg


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
