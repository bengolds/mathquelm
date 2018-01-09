module Mathquelm.Undo exposing (..)


type alias StateWithHistory a =
    { undoStack : List a
    , redoStack : List a
    , currentState : a
    }


undo : StateWithHistory stateType -> Maybe (StateWithHistory stateType)
undo stateWithHistory =
    case stateWithHistory.undoStack of
        previousState :: restOfUndos ->
            Just
                { currentState = previousState
                , undoStack = restOfUndos
                , redoStack =
                    stateWithHistory.currentState
                        :: stateWithHistory.redoStack
                }

        _ ->
            Nothing


redo : StateWithHistory stateType -> Maybe (StateWithHistory stateType)
redo stateWithHistory =
    case stateWithHistory.redoStack of
        nextState :: restOfRedos ->
            Just
                { currentState = nextState
                , redoStack = restOfRedos
                , undoStack =
                    stateWithHistory.currentState
                        :: stateWithHistory.undoStack
                }

        _ ->
            Nothing


recordState : StateWithHistory stateType -> StateWithHistory stateType
recordState stateWithHistory =
    { stateWithHistory
        | undoStack = stateWithHistory.currentState :: stateWithHistory.undoStack
        , redoStack = []
    }


setState : stateType -> StateWithHistory stateType -> StateWithHistory stateType
setState newState stateWithHistory =
    { stateWithHistory
        | currentState = newState
    }


map : (stateType -> stateType) -> StateWithHistory stateType -> StateWithHistory stateType
map fn stateWithHistory =
    { stateWithHistory
        | currentState = fn stateWithHistory.currentState
    }


freshHistory startingState =
    { currentState = startingState
    , undoStack = []
    , redoStack = []
    }
