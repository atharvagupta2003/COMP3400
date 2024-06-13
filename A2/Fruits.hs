module Fruits (Tree (..), smallest) where

data Tree a = Tree a [Tree a]
    deriving (Eq, Show)

tree1 = 
        Tree 3 [
                 Tree 4 [ Tree 2 [
                                   Tree 6 []
                                 , Tree 5 []
                                 ]
                        ]
               , Tree 1 []
               , Tree 7 [ Tree 8 [] ]
               ]
tree2 = Tree 3 [Tree 4 [ Tree 2 [Tree 6 [], Tree 5 []]], Tree 1 [] , Tree 7 [ Tree 8 [ Tree 42 []] ] ]
tree3 = Tree 3400 [tree1, tree2]  
tree4 = Tree 0 []       
tree5 = Tree 101 [tree3, tree5]

tree6 = Tree 202 [tree3, tree5] -- No self-referencing, but it still contains an infinite tree inside

tree7 = Tree 228 [tree5, tree6, tree7]      

-- Function to compare tree heights
treeHeight :: Tree a -> Int
treeHeight t = go t 1

-- Helper function to detect infinite trees
go :: Tree a -> Int -> Int
go (Tree _ []) n = n
go (Tree _ ts) n
    | n > 50 = 50 -- Consider infinite trees as height 0
    | otherwise = 1 + maximum (map (`go` (n + 1)) ts)

-- Function to find the smallest tree
smallest :: [Tree a] -> Maybe (Tree a)
smallest [] = Nothing
smallest (t:ts) = Just $ foldl (\acc x -> if treeHeight x < treeHeight acc then x else acc) t ts

