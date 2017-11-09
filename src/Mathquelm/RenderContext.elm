module Mathquelm.RenderContext exposing (..)

import Mathquelm.Config as Config exposing (Config)
import Mathquelm.CursorTree as CursorTree
import Style.Scale as Scale


type RenderItem
    = Character Char
    | Cursor
    | OneBlock OneBlockType Block
    | TwoBlocks TwoBlocksType Block Block


type alias Block =
    List RenderTarget


type alias RenderTarget =
    { depth : Int
    , item : RenderItem
    }



--fromZipperTree : DisplayItem -> RenderItem


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
