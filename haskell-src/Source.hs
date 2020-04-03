{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}

import GHC.Num
import GHC.Show

class Functor (f :: * -> *) where
    map :: (a -> b) -> f a -> f b

class Functor m => Monad (m :: * -> *) where
    unit :: a -> m a
    join :: m (m a) -> m a

(>>=) :: Monad m => m a -> (a -> m b) -> m b
x >>= g = join (map g x)


data Bool = True | False
    deriving Show

data Maybe a = Nothing | Just a
    deriving Show

data List a = Nil | Cons a (List a)
    deriving Show

data Either a b = Left a | Right b
    deriving Show

data Pair a b = Pair a b
    deriving Show


instance Functor Maybe where
    map g Nothing = Nothing
    map g (Just x) = Just (g x)

instance Monad Maybe where
    unit x = Just x
    join Nothing = Nothing
    join (Just x) = x

instance Functor List where
    map g Nil = Nil
    map g (Cons x xs) = Cons (g x) (map g xs)

instance Functor (Either a) where
    map g (Left x) = Left x
    map g (Right x) = Right (g x)

instance Functor ((->) a) where
    map g h = g . h

id :: a -> a
id x = x

const :: a -> b -> a
const x y = x

const' :: a -> b -> a
const' x = \y -> x

const'' :: a -> b -> a
const'' = \x y -> x

(.) :: (b -> c) -> (a -> b) -> a -> c
g . f = \x -> g (f x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

cat :: List a -> List a -> List a
cat Nil l = l
cat (Cons x xs) l = Cons x (cat xs l)





