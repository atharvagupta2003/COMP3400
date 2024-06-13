module Polynomial (expand, simplify, Polynomial(..)) where

f = Mul (Add (Mono 2 1) (Mono 1 0)) (Add (Mono 1 2) (Mono 2 0))
g = Mul (Add (Mono 1 1) (Mono 1 0)) (Add (Mono 1 1) (Mono 1 0))

type Deg = Integer   -- precondition: 
type Coeff = Integer -- precondition: 

data Polynomial = Mono Coeff Deg | Add Polynomial Polynomial | Mul Polynomial Polynomial
    deriving Show

expand :: Polynomial -> Polynomial
expand (Mono c d) = Mono c d
expand (Add p1 p2) = Add (expand p1) (expand p2)
expand (Mul p1 p2) = expandMul (expand p1) (expand p2)
            where
                 expandMul :: Polynomial -> Polynomial -> Polynomial
                 expandMul (Mono c1 d1) (Mono c2 d2) = Mono (c1 * c2) (d1 + d2)
                 expandMul (Mono c d) (Add p1 p2) = Add (expandMul (Mono c d) p1) (expandMul (Mono c d) p2)
                 expandMul (Add p1 p2) p = Add (expandMul p1 p) (expandMul p2 p)

simplify :: Polynomial -> Polynomial
simplify = simplify' . expand

simplify' :: Polynomial -> Polynomial
simplify' (Mono c d) = Mono c d
simplify' (Add p1 p2) = simplify'' (simplify' p1) (simplify' p2)

simplify'' :: Polynomial -> Polynomial -> Polynomial
simplify'' (Mono c1 d1) (Mono c2 d2)
    | d1 == d2  = Mono (c1 + c2) d1
    | otherwise = Add (Mono c1 d1) (Mono c2 d2)
simplify'' (Mono c d) (Add (Mono c1 d1) p2)
    | d == d1   = simplify'' (Mono (c + c1) d) (simplify' p2)
    | d > d1    = Add (Mono c d) (simplify'' (Mono c1 d1) (simplify' p2))
    | otherwise = Add (Mono c1 d1) (simplify'' (Mono c d) (simplify' p2))
simplify'' (Add p1 p2) p = simplify'' (simplify' p1) (simplify'' (simplify' p2) p)
