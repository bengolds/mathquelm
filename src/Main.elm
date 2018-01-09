port module Main exposing (..)

import Char
import Element exposing (..)
import Element.Attributes exposing (id, padding, spacing)
import Html exposing (Html)
import Keyboard
import Mathquelm exposing (editableQuelm)
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
    Mathquelm.Model


init : ( Model, Cmd Msg )
init =
    let
        _ =
            Debug.log "latex" Mathquelm.latex

        mqModel =
            Mathquelm.defaultModel
    in
    ( mqModel
    , Cmd.none
      --, katexOut (Mathquelm.latex mqModel)
    )



-- UPDATE


type Msg
    = Noop
    | ToggleCenterLine
    | ToggleBoxes
    | MathquelmMsg Mathquelm.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        Noop ->
            model

        ToggleCenterLine ->
            { model | config = Config.toggleCenterLineDisplay model.config }

        ToggleBoxes ->
            { model | config = Config.toggleBoxesDisplay model.config }

        MathquelmMsg msg ->
            Mathquelm.update msg model
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
            [ spacing 20, padding 20 ]
            [ el None [ padding 0 ] <| html (Html.map MathquelmMsg (editableQuelm model))

            --, katex
            --, Mathquill.staticMath Mathquill [] (Mathquelm.latex model)
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
    Sub.batch
        [ Keyboard.presses
            (\code ->
                case Char.fromCode code of
                    'c' ->
                        ToggleCenterLine

                    'b' ->
                        ToggleBoxes

                    _ ->
                        Noop
            )
        , Sub.map MathquelmMsg (Mathquelm.subscriptions model)
        ]
