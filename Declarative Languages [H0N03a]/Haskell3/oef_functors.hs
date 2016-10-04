module Functors where

data Identity a = Identity a
                deriving Show


instance Functor Identity where
    fmap f (Identity a) = Identity (f a)



data Pair a b = Pair a b
              deriving Show

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)



data Unit a = Unit
             deriving Show

instance Functor Unit where
    fmap _ Unit = Unit
