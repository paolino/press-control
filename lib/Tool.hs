{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tool where

newtype Tool = Tool Int
    deriving (Eq, Show, Ord)

newtype Position = Position Int
    deriving (Eq, Show, Ord, Num)

data Placed = Placed
    { position :: Position
    , tool :: Tool
    }
    deriving (Eq, Show, Ord)

-- | The end of a Placed tool
endOfPlaced :: Placed -> Position
endOfPlaced (Placed (Position p) (Tool t)) = Position $ p + t

-- | Create a Placed tool (for testing)
placed :: Int -> Int -> Placed
placed p t = Placed (Position p) $ Tool t
