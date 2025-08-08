module Fibonacci where
import Data.List

---------------- Exercise 1

-- simple fibonacci sequence
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- infinite list of fibonaccis
fibs1 :: [Integer]
fibs1 = map fib [0..]


---------------- Exercise 2
 
-- creates a list of [(a,b)] with the first element being (0,1). Each subsequent element = (y, x+y), with the first item in that pair being the current fib num. Then you extract it and add to [Integer]
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x,y) -> (y, x+y)) (0,1)


---------------- Exercise 3

data Stream a = Cons a (Stream a)
    
instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

---------------- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- Same as iterate
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

---------------- Exercise 5

-- My original attempts are commented out. They do not work with infinite data structures, and thus caused stack overflows. Had to look up the correct ways. It's weird to think about recursion without base cases. I still haven't fully gotten it yet

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)
-- interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys)) -- this is wrong. Because both streams are infinite, I never am able to evaluate Cons y ys and thus stack overflow

nats :: Stream Integer
nats = streamFromSeed (+1) 0

level :: Integer -> Stream Integer
level n = interleaveStreams (streamRepeat n) (level (n + 1)) -- Once I saw this, it looked so obviously elegant.
-- level 0 = streamRepeat 0 -- wrong for similar reasons. having a base recursion case for an infinite data type is a red flag.
-- level n = interleaveSteams (level n-1) (StreamRepeat n) -- must go towards infinity, not towards a limit like 0

ruler :: Stream Integer
ruler = level 0


