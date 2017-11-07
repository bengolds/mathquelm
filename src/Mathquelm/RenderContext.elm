module Mathquelm.RenderContext exposing (..)

import Mathquelm.Config exposing (..)
import Mathquelm.ZipperTree exposing (OneBlockType(..), TwoBlocksType(..))
import Style.Scale as Scale


type TreeObject
    = Block Mathquelm.ZipperTree.Block
    | Node Mathquelm.ZipperTree.DisplayItem


type alias RenderContext =
    { config : Config
    , depth : Int
    , target : TreeObject
    }


baseContext config target =
    { config = config
    , depth = 0
    , target = target
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


setTarget : TreeObject -> RenderContext -> RenderContext
setTarget newTarget context =
    { context | target = newTarget }


enter context nextTarget =
    context
        |> (case nextTarget of
                Node (Mathquelm.ZipperTree.TwoBlocks Fraction _ _) ->
                    deepen

                Node (Mathquelm.ZipperTree.OneBlock Subscript _) ->
                    hardDeepen

                {--
  -
  -                Superscript _ ->
  -                    hardDeepen
  -
  -                Subsuperscript _ _ ->
  -                    hardDeepen
  --}
                _ ->
                    identity
           )
        |> setTarget nextTarget
