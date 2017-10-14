module Mathquelm.RenderContext exposing (..)

import Mathquelm.Config exposing (..)
import Mathquelm.DisplayNode exposing (..)
import Style.Scale as Scale


type alias RenderContext =
    { config : Config
    , depth : Int
    , node : DisplayNode
    }


baseContext config node =
    { config = config
    , depth = 0
    , node = node
    }


deepen context =
    { context
        | depth = min context.config.maxDepth (context.depth + 1)
    }


hardDeepen context =
    { context
        | depth = clamp 2 context.config.maxDepth (context.depth + 1)
    }


depth context =
    context.depth


fontSize context =
    Scale.modular context.config.baseFontSize context.config.modularScale context.depth


fontBox context =
    fontSize context * 1.2


setNode : DisplayNode -> RenderContext -> RenderContext
setNode node context =
    { context | node = node }


enter context nextNode =
    context
        |> (case nextNode of
                Block _ ->
                    identity

                Character _ ->
                    identity

                Parens _ _ ->
                    identity

                Diacritic _ _ ->
                    identity

                Fraction _ _ ->
                    deepen

                Subscript _ ->
                    hardDeepen

                Superscript _ ->
                    hardDeepen

                Subsuperscript _ _ ->
                    hardDeepen

                SquareRoot _ ->
                    identity
           )
        |> setNode nextNode
