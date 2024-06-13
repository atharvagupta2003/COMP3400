module Magic (magic, condIncr, trim, enumerate) where

import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))
                          
magic :: (a -> (b -> c)) -> ([a] -> ([b] -> [c]))
magic f xs ys = go xs ys []
  where
    go [] _ acc = acc
    go _ [] acc = acc
    go (x:xs) (y:ys) acc = go xs ys (acc ++ [f x y])

condIncr :: [Int] -> [Bool] -> [Int]
condIncr = magic cond
  where
    cond x True = (+) x 1
    cond x False = x

trim :: [a] -> [b] -> [a]
trim xs ys = magic trimHelper xs ys
  where
    trimHelper x _ = x 

enumerate :: [a] -> [(Int, a)]
enumerate xs = enumerate' xs 0
  where
    enumerate' [] _ = []
    enumerate' (x:xs) n = (n, x) : enumerate' xs (n + 1)    