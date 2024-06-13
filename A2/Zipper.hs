module Zipper (Zipper (..), toZipper, zip1, unzip1, fullyZip, fullyUnzip) where

data Zipper a = Zipper [(a, a)] [a] [a] deriving (Eq, Show)

z = Zipper [(1, 4), (2, 5)] [] [6]

instance Functor Zipper where
    fmap f (Zipper pairs above below) = Zipper (map (\(x, y) -> (f x, f y)) pairs) (map f above) (map f below)
toZipper :: [a] -> [a] -> Zipper a
toZipper above below = Zipper [] above below

zip1 :: Zipper a -> Zipper a
zip1 (Zipper pairs (x:xs) below) = Zipper ((x, head below):pairs) xs (tail below)
zip1 zipper = zipper

unzip1 :: Zipper a -> Zipper a
unzip1 (Zipper ((x, y):pairs) above below) = Zipper pairs (x:above) (y:below)
unzip1 zipper = zipper

fullyZip :: Zipper a -> Zipper a
fullyZip zipper@(Zipper _ [] _) = zipper
fullyZip zipper@(Zipper pairs above below) = fullyZip (zip1 zipper)

fullyUnzip :: Zipper a -> Zipper a
fullyUnzip zipper@(Zipper _ [] _) = zipper
fullyUnzip zipper@(Zipper pairs above below) = fullyUnzip (unzip1 zipper)
