{-# LANGUAGE DeriveFunctor #-}
module Sandbox11 where

import Control.Applicative

type Name = String 
data Employee = Employee { name :: Name, phone :: String}
                deriving Show

names = ["Jack", "Sara", "Mae"]
phones = ["555-1234","444-1234","333-1234"]

-- by default, creates all possible combinations non-deterministically
employees1 = Employee <$> names <*> phones

newtype ZipList' a = ZipList' { getZipList' :: [a] }
    deriving (Eq, Show, Functor)

-- We can also zip up each name for each phone, but we must be careful because our list must always be as long as the quantity of values in context. ie, pure f <*> xs === f <$> xs
-- Since we don't know that in advance, pure is just the list repeated forever so we can always truncate at the length of xs
instance Applicative ZipList' where
    pure = ZipList' . repeat
    ZipList' fs <*> ZipList' xs = ZipList' (zipWith ($) fs xs)

employees2 = getZipList' $ Employee <$> ZipList' names <*> ZipList' phones

-- Already defined
-- instance Functor ((->) e) where -- 'reader', reads from the environment, e
--     fmap = (.) -- function application

-- Already defined
-- instance Applicative ((->) e) where
--     pure = const -- returns the same value. so if pure :: a -> f a and f == ((->) e). Then a -> ((->) e a) or a -> (e -> a) or a -> e -> a and since we want to return a and const :: a -> b -> a, then const satisfies a -> e -> a
--     f <*> x = \e -> (f e) (x e)

data BigRecord = BR { getName         :: Name
                    , getSSN          :: String
                    , getSalary       :: Integer
                    , getPhone        :: String
                    , getLicensePlate :: String
                    , getNumSickDays  :: Int
                    }

r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2

-- we can use a big record to make an employee type of the person from that record by lifting from one context into another. very cool
getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone

ex01 = getEmp r

pair :: Applicative f => f a -> f b -> f (a,b)
-- pair fa fb = (,) <$> fa <*> fb
pair = liftA2 (,)

applyr :: Applicative f => f a -> f b -> f b
applyr = liftA2 seq 

applyl :: Applicative f => f a -> f b -> f a
applyl = liftA2 (flip seq)

a1 = Just 5
a2 = Just 6
a3 = Nothing

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b]) -- Remember, this is the same syntax as map, just obfuscated. same as (a -> f b) -> [a] -> f [b]
mapA _ [] = pure []
mapA f (x:xs) = liftA2 (:) (f x) (mapA f xs)
-- mapA = sequenceA' . map -- we can also just define it easily using this way as well once we have sequenceA'

sequenceA' :: Applicative f => [f a] -> f [a] -- lift all values recursively from a list of f context to an expanded list where each operation leave f [a]
-- sequenceA' [] = pure []
-- sequenceA' (x:xs) = liftA2 (:) x (sequenceA' xs)
sequenceA' = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n fa = sequenceA' (replicate n fa)

