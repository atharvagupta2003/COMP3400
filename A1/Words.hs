module Words (countWays) where

import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))
                          
-- Function to count the number of ways to obtain the target word from the original word
count :: String -> String -> Int
count _ [] = 1  -- If the target word is empty, there is only one way to obtain it
count [] _ = 0  -- If the original word is empty and the target is not, it's not possible
count (x:xs) (y:ys)
    | x == y    = count xs ys + count xs (y:ys)
    | otherwise = count xs (y:ys)

-- Main function to calculate the number of ways
countWays :: String -> String -> Integer
countWays original target = fromIntegral (count original target)