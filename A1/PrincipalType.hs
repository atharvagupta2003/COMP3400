module PrincipalType (typeA, typeB, typeC, typeD, typeE) where

import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))
                          
typeA (f, a) = f a

typeB f a = (a, f (a, a))

typeC f (a, b) (z, w) = f a b z w

typeD f g a = f a (g a)

typeE f g h = [g (h . f)]
