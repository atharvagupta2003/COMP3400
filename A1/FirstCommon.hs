module FirstCommon (firstCommon) where

import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))
                          
countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = go 0
  where
    go acc [] = acc
    go acc (y:ys)
      | x == y = go (acc + 1) ys
      | otherwise = go acc ys

maxCount :: Eq a => [a] -> Int
maxCount [] = 0
maxCount (x:xs) = maximum' (countOccurrences x (x:xs)) (maxCount xs)
  where
    maximum' a b
      | a > b = a
      | otherwise = b

mostCommonElement :: Eq a => [a] -> a
mostCommonElement [] = error "Empty list"
mostCommonElement (x:xs) =
  go x xs (countOccurrences x (x:xs)) (maxCount (x:xs))
  where
    go acc [] _ _ = acc
    go acc (y:ys) count maxCount'
      | count == maxCount' = acc
      | otherwise = go y ys (countOccurrences y (y:ys)) maxCount'

firstCommon :: Eq a => [a] -> a
firstCommon xs = mostCommonElement xs