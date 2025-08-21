module Sandbox9 where
import Data.Maybe (fromMaybe)

data Funny f a = Funny a (f a)

-- define a class with a method to functor map; ie, apply a function to the value inside the type contructor instance provided
class Functor' f where
    fmap' :: (a->b) -> f a -> f b

-- make an instance of our functor' class that works with the Maybe type constructor. define how it's fmap' method needs to be used
instance Functor' Maybe where
    fmap' _ Nothing = Nothing
    fmap' f (Just a) = Just (f a)

instance Functor' [] where
    -- fmap' _ [] = []
    -- fmap' f (x:xs) = f x : fmap' f xs

    -- waitttt. I could just do
        fmap' = map

test = Just 5
test2 = Nothing
test3 = Just 4

testList = [test,test2,test3]

-- now I can check if a Maybe Integer is even right away!
evenMaybe :: Maybe Integer -> Maybe Bool
evenMaybe = fmap' even

-- since I don't have a standard double function, define one here using a type class for practice
double :: Num a => a -> a
double  = (*2)

-- Now I can double a Maybe Integer right away!
doubleMaybe :: Maybe Integer -> Maybe Integer
doubleMaybe = fmap' double

-- double if even, ignore if not
doubleIfEven :: Maybe Integer -> Maybe Integer
doubleIfEven x = if fromMaybe False (evenMaybe x) then doubleMaybe x else x

-- Function to combine them. Of course a lot of this is redundanct but it shows how the abstractions build, just wanted to implement my own Functor class
doubleEvenMaybe :: [Maybe Integer] -> [Maybe Integer]
doubleEvenMaybe = fmap' doubleIfEven -- map is just an fmap for lists. ie, map doubleIfEven does the same thing

instance Functor' ((->) e) where
    fmap' = (.) -- So cool.