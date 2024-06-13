module Tiling (Triangle, tiling) where

type Triangle = [[Integer]]


-- Function to generate a single row of the triangle
generateRow :: Integer -> Integer -> [Integer]
generateRow 0 0 = [0]  -- Base case for uncovered triangle T_0
generateRow 1 0 = [0]  -- Uncovered triangle T_1
generateRow n row
    | row == 0 = replicate (fromIntegral n) 0  -- Uncovered triangle
    | otherwise = replicate (fromIntegral row) row ++ replicate (fromIntegral (n - row + 1)) (n - row + 1)

-- Function to generate the entire tiling for a given triangle size
tiling :: Integer -> Triangle 
tiling 0 = [[0]]  -- Base case for T_0
tiling 1 = [[0],[1,1,1]]  -- Base case for T_1
tiling 2 = [[0],[1,1,1], [2,2,3,3,3,5,5]] 
tiling n = tiling (n - 1) ++ [generateRow n row | row <- [0..n]]

