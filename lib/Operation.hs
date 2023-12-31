module Operation
    ( operationsToChangeRecipe
    , Operation (..)
    )
where

import Data.Bifunctor (second)
import Data.List (sort)
import Recipe (Recipe)
import Tool (Placed (..), Position, Tool, endOfPlaced)

-- | Operations to change the press
data Operation = Remove Placed | Move Tool Position Position | Add Placed
    deriving (Eq, Show, Ord)

-- | Operations to change the press from one recipe to another, sorted by
-- remove before add, and by position
operationsToChangeRecipe :: Recipe -> Recipe -> [Operation]
operationsToChangeRecipe cs ns =
    sort -- take care of removing before moving
        . compress -- compress remove and add on the same tool into move
        . sort -- take care of removing before adding
        $ operationsToChangeRecipe' cs ns

-- | Unsorted operations to change the press from one recipe to another
operationsToChangeRecipe'
    :: Recipe
    -- ^ Current recipe
    -> Recipe
    -- ^ New recipe
    -> [Operation]
operationsToChangeRecipe' cs [] = map Remove cs
operationsToChangeRecipe' [] ns = map Add ns
operationsToChangeRecipe' (c : cs) (n : ns)
    | c == n = operationsToChangeRecipe' cs ns
    | enoughSpace c n = Add n : operationsToChangeRecipe' (c : cs) ns
    | otherwise = Remove c : operationsToChangeRecipe' cs (n : ns)

-- check there is enough space to add a tool before the next tool on the press
enoughSpace
    :: Placed
    -- ^ Current tool
    -> Placed
    -- ^ New tool
    -> Bool
enoughSpace (Placed p' _) x = endOfPlaced x <= p'

-- compress remove and add operations on the same tool into move operations
-- we rely on the fact that operations are sorted by remove before add
compress :: [Operation] -> [Operation]
compress [] = []
compress (Add x : rest) = Add x : rest
compress (Remove (Placed p t) : rest) = case findAdd t rest of
    Nothing -> Remove (Placed p t) : compress rest
    Just (p', rest') -> Move t p p' : compress rest'

-- find the first add operation on a given tool
findAdd :: Tool -> [Operation] -> Maybe (Position, [Operation])
findAdd _ [] = Nothing
findAdd t (Add x : rest) | t == tool x = Just (position x, rest)
findAdd t (x : rest) = second (x :) <$> findAdd t rest
