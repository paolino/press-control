module Recipe where

import Tool
    ( Placed (Placed, position)
    , Position (..)
    , Tool (..)
    , endOfPlaced
    )

-- | A recipe is a sequence of placed tools, ordered by position
type Recipe = [Placed]

-- | Errors that can occur when validating a recipe
data ErrorRecipe
    = InvalidWidth Placed
    | OverlappingPlaceds Placed Placed
    deriving (Eq, Show)

-- | Validate a Placed tool against the maximum width
guardWidth :: Position -> Placed -> Either ErrorRecipe ()
guardWidth x y
    | endOfPlaced y > x = Left $ InvalidWidth y
    | otherwise = Right ()

-- | Validate 2 subsequent Placed tools against each other
guardOverlapping :: Placed -> Placed -> Either ErrorRecipe ()
guardOverlapping x y
    | endOfPlaced x >= position y = Left $ OverlappingPlaceds x y
    | otherwise = Right ()

-- | Validate a recipe against the maximum width and overlapping tools
validSequence :: Position -> Recipe -> Either ErrorRecipe ()
validSequence _ [] = Right ()
validSequence m [x] = guardWidth m x
validSequence m (x : y : rest) = do
    guardWidth m x
    guardOverlapping x y
    validSequence m $ y : rest

-- | Insert a Placed tool into a recipe, no validation
insert :: Placed -> Recipe -> Recipe
insert p [] = [p]
insert p (p' : ps)
    | position p < position p' = p : p' : ps
    | otherwise = p' : insert p ps

-- | Find the first position in a recipe where a tool fits in, no validation
firstFit :: Tool -> Recipe -> Position
firstFit t [] = 0
firstFit (Tool w) [Placed p t'] = p + Position w
firstFit t@(Tool w) (f@(Placed p' t') : Placed p'' _ : ps)
    | endOfPlaced f + Position w <= p'' = endOfPlaced f
    | otherwise = firstFit t (Placed p'' t' : ps)
