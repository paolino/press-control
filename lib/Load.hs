module Load where

import Control.Arrow (left)
import Machine
    ( ErrorStorage
    , Machine (Machine)
    , pickFromStorage
    , validStorage
    )
import Operation (Operation (Add), operationsToChangeRecipe)
import Recipe (ErrorRecipe, Recipe, firstFit, insert, validSequence)
import Tool (Placed (Placed), Tool, tool)

-- | Errors that can occur when loading a recipe or a tool
data ErrorLoad = ErrorLoadRecipe ErrorRecipe | ErrorLoadStorage ErrorStorage
    deriving (Eq, Show)

-- | A type synonym for a function that loads a recipe or a tool,
-- returning the operations to change the press and the new machine
type Load = Machine -> Either ErrorLoad ([Operation], Machine)

-- | Promote recipe errors to load errors
eRecipe :: Either ErrorRecipe d -> Either ErrorLoad d
eRecipe = left ErrorLoadRecipe

-- | Promote storage errors to load errors
eStorage :: Either ErrorStorage d -> Either ErrorLoad d
eStorage = left ErrorLoadStorage

-- | Load a recipe into a machine, whatever the current state of the machine
loadRecipe :: Recipe -> Load
loadRecipe r m@(Machine w p s) = do
    eRecipe $ validSequence w r
    s' <- eStorage $ validStorage m r
    pure (operationsToChangeRecipe p r, Machine w r s')

-- | Load a single tool into a machine at a given position
-- whatever the current state of the machine
loadSingle :: Placed -> Load
loadSingle x m@(Machine w p s) = do
    s' <- eStorage $ pickFromStorage (tool x) s
    let p' = insert x p
    eRecipe $ validSequence w p'
    pure ([Add x], Machine w p' s')

-- | Load a tool into a machine, finding the first available space
-- whatever the current state of the machine,
loadFree :: Tool -> Load
loadFree t m@(Machine w p s) = do
    s' <- eStorage $ pickFromStorage t s
    let pt = Placed (firstFit t p) t
        p' = insert pt p
    eRecipe $ validSequence w p'
    pure ([Add pt], Machine w p' s')
