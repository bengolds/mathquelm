module Mathquelm exposing (..)

import Element exposing (..)
import Html exposing (Html)
import Keyboard
import Mathquelm.Config as Config exposing (Config)
import Mathquelm.Render exposing (..)
import Mathquelm.RenderContext as RenderContext exposing (..)
import Mathquelm.Styles exposing (..)
import Mathquelm.ZipperTree exposing (..)


stringToNodes string =
    String.toList string
        |> List.map Character


parens =
    OneBlock (BalancedDelimiters Parentheses)


frac =
    TwoBlocks Fraction


sub =
    OneBlock Subscript


str =
    stringToNodes


sampleTree : DisplayZipper
sampleTree =
    ( OneBlockCrumb Subscript <|
        { left = []
        , right =
            str "abc"
                ++ [ parens (str "de")
                   , frac
                        (str "fgh")
                        [ frac
                            (str "i")
                            (str "j" ++ [ sub (str "klmno") ])
                        ]
                   ]
                ++ stringToNodes "pqrs"
        }
    , []
    )


latex : Model -> String
latex model =
    toLatex (top model.tree)


editableQuelm : Model -> Html Msg
editableQuelm model =
    layout (stylesheet model.config) <|
        column Base
            []
            [ loadFont
            , render <| baseContext model.config (Node (top model.tree))
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
    { tree : DisplayZipper
    , config : Config
    }


defaultModel : Model
defaultModel =
    { tree = ( OneBlockCrumb Subscript { left = [], right = [] }, [] )
    , config = Config.default
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Move dir ->
            model

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
