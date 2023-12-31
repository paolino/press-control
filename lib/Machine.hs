module Machine where

import Recipe (Recipe)
import Tool (Placed (Placed, tool), Position, Tool)

-- | The storage of a machine as a list of tools
type Storage = [Tool]

-- | A machine is a width, a loaded recipe and a storage of leftover tools
data Machine = Machine
    { width :: Position
    , press :: Recipe
    , storage :: Storage
    }
    deriving (Eq, Show)

-- | The storage of a machine at rest, without a recipe loaded
machineAtRest :: Machine -> Storage
machineAtRest (Machine w p s) = s <> map tool p

-- | Errors that can occur when picking a tool from the storage
newtype ErrorStorage = ToolNotInStorage Tool
    deriving (Eq, Show)

-- | Pick a tool from the storage, leaving the rest of the storage
pickFromStorage :: Tool -> Storage -> Either ErrorStorage Storage
pickFromStorage t [] = Left $ ToolNotInStorage t
pickFromStorage t (t' : ts)
    | t == t' = Right ts
    | otherwise = (t' :) <$> pickFromStorage t ts

-- | Validate a recipe against the total storage of a machine
validStorage :: Machine -> Recipe -> Either ErrorStorage Storage
validStorage m = go (machineAtRest m)
  where
    go s [] = Right s
    go s (Placed p t : rest) = case pickFromStorage t s of
        Right s' -> go s' rest
        Left l -> Left l

-- | Create a machine with a given width and storage, without a recipe loaded
freshMachine :: Position -> Storage -> Machine
freshMachine width = Machine width []
