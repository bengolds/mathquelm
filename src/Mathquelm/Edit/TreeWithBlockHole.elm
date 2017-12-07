module Mathquelm.Edit.TreeWithBlockHole exposing (..)

import Mathquelm.Edit.Command exposing (Block, Command(..))
import Mathquelm.ListZipper as ListZipper exposing (ListZipper, getAllAfter, getAllBefore)


type alias TreeWithBlockHole =
    List BlockWithBlockHole


type CommandWithBlockHole
    = CosWithHole
    | DivWithTopHole Block
    | DivWithBotHole Block


type alias BlockWithBlockHole =
    { restOfBlock : ListZipper Command
    , commandWithBlockHole : CommandWithBlockHole
    }


fillBlockHole : BlockWithBlockHole -> Block -> Block
fillBlockHole blockWithBlockHole fillerBlock =
    blockWithBlockHole.restOfBlock
        |> ListZipper.insertBefore
            [ fillCommandHole
                blockWithBlockHole.commandWithBlockHole
                fillerBlock
            ]
        |> ListZipper.toList


fillCommandHole : CommandWithBlockHole -> Block -> Command
fillCommandHole commandWithBlockHole fillerBlock =
    case commandWithBlockHole of
        CosWithHole ->
            Cos fillerBlock

        DivWithTopHole bot ->
            Div fillerBlock bot

        DivWithBotHole top ->
            Div top fillerBlock


getCommandBeingEdited : TreeWithBlockHole -> Maybe CommandWithBlockHole
getCommandBeingEdited restOfTree =
    case restOfTree of
        [] ->
            Nothing

        parentBlockWithHole :: _ ->
            Just parentBlockWithHole.commandWithBlockHole


changeCommand : CommandWithBlockHole -> BlockWithBlockHole -> BlockWithBlockHole
changeCommand newCommand blockWithHole =
    { blockWithHole | commandWithBlockHole = newCommand }


push : BlockWithBlockHole -> TreeWithBlockHole -> TreeWithBlockHole
push blockWithBlockHole restOfTree =
    blockWithBlockHole :: restOfTree
