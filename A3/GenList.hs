{-# LANGUAGE UndecidableInstances #-}

module GenList (GenList (..)) where

{--

You are familiar with the standard recursive definition of Lists:

            The container holding the rest of the elements (also a list)
                                vvvvvv
    data List a = Nil | Cons a (List a)
                             ^
                The element held by the node

We can generalise this definition and change its structure a little bit
replacing the inner container from List with an arbitrary type:

              The element held by the node
                           v
    data GenList f a = End a | GenList (f (GenList f a))
                                        ^
                The rest of the elements are now held in an abstract container f

To make it look like a regular list, we need to provide a container that holds a pair of elements:
one will hold the current element while the second will hold the "tail".

data Pair a = Pair a a

type List a = GenList Pair a

In this case, a list [1, 2, 3, 4] will look like

GenList (Pair (End 1) (GenList (Pair (End 2) (GenList (Pair (End 3) (End 4))))))

Pair can also be used to construct binary trees.

We can construct arbitrary trees by replacing Pair with a standard list.

type Tree a = GenList [] a

In this case, one of the trees from Fruits from A2 will look like

GenList [ End 3
        , GenList [ End 4
                  , GenList [ End 2
                            , GenList [End 6]
                            , GenList [End 5]
                            ]
                  ]
        , GenList [ End 1 ]
        , GenList [ End 7
                  , GenList [End 8]
                  ]
        ]

Your task will be to define Functor, Applicative and Monad instances for this type, given that the inner container is a Functor.

The expected behaviour of Applicative and Monad resembles that of List:

-- A structure resembling the list [3, 4, 5] if you squint enough
--
tree = GenList [ End 3
               , GenList [ End 4
                         , End 5
                         ]
               ]

tree >>= (\i -> GenList [End (i + 1), End (i + 2)])

would result in

GenList [ GenList [ End 4, End 5 ]
        , GenList [ GenList [End 5, End 6]
                  , GenList [End 6, End 7]
                  ]
        ]

which, again, resembles the result of

[3, 4, 5] >>= (\i -> [i + 1, i + 2]) == [4, 5, 5, 6, 6, 7]

if you squint enough.



And the same goes for Applicative:

t1 = GenList [ End 1, End 2 ]
t2 = GenList [ GenList [End 3], End 4, GenList [End 5]]

(+) <$> t1 <*> t2 == GenList [GenList [GenList [End 4],End 5,GenList [End 6]],GenList [GenList [End 5],End 6,GenList [End 7]]]

(+) <$> [1, 2] <*> [3, 4, 5] == [4,5,6,5,6,7]


The funny thing (if you share our definition of fun) is that you only need f to be a Functor to define these instances.

A general hint is, please, make use of this fact!

This problem is worth 10 POINTS.

--}

data GenList f a = End a | GenList (f (GenList f a))

deriving instance (Show a, Show (f (GenList f a))) => Show (GenList f a)

-- Your implementation goes below

-- Functor instance
instance Functor f => Functor (GenList f) where
    fmap f (End a) = End (f a)
    fmap f (GenList xs) = GenList ((f <$>) <$> xs)

-- Applicative instance
instance Functor f => Applicative (GenList f) where
    pure = End

    End f <*> gx = fmap f gx
    GenList fs <*> gx = GenList ((<*> gx) <$> fs)

-- Monad instance
instance Functor f => Monad (GenList f) where
    (End a) >>= f = f a
    (GenList xs) >>= f = GenList ((>>= f) <$> xs)