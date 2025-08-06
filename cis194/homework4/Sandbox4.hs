module Sandbox4 where

add :: Int -> Int -> Int
--add x y = x + y
--add x = \y -> x+y
add = \x -> (\y -> x+y)
--add x = (x +)
--add = (+) -- WHAT WHAT WHAT

doubleList :: [Int] -> [Int]
doubleList = map (\x -> 2*x)
-- ghci> map (\x -> 2*x) [1,2,3]
-- [2,4,6]

doubleList2 :: [Int] -> [Int]
doubleList2 xs = map (*2) xs
-- ghci> map (*2) [1,2,3]
-- [2,4,6]

foobar :: [Int] -> Int
foobar = sum . map (\x -> 7*x + 2) . filter (>3)

fold0 :: b -> (a -> b -> b) -> [a] -> b
fold0 z f [] = z
fold0 z f (x:xs) = f x (fold0 z f xs)

sum' = fold0 0 (+)
product' = fold0 1 (*)
length' = fold0 0 (\_ s -> 1 + s) -- _ s ignores the current value, but uses s as an accumulator to increment length
length'' = fold0 0 (\_ -> (1+))
length''' = fold0 0 (const (1+))

------------------------------ FizzBuzz Solution 1

-- random thought to implement fizz buzz. challenge: reduce to be wholemeal / more idiomatic
fizzBuzz :: [Int] -> String
fizzBuzz [] = []
fizzBuzz (x:xs)
    | x `mod` 15 == 0   = "FizzBuzz " ++ fizzBuzz xs
    | x `mod` 5 == 0    = "Buzz " ++ fizzBuzz xs
    | x `mod` 3 == 0    = "Fizz " ++ fizzBuzz xs
    | otherwise         = show x ++ " " ++ fizzBuzz xs


------------------------------- FizzBuzz Solution 2

-- Stretch exercise to use iterate and make point-free. Got it right on the next attempt. Best attempt below. Good example of why higher-order need lower functions to build off. I also know that my overcomplication and type change makes it even less so, I'm just a party animal
fizzBuzz' :: String -> [String]
fizzBuzz' = takeWhile (/= "101") . iterate (\x -> if read x `mod` 15 == 0 then "FizzBuzz" else (if read x `mod` 3 == 0 then "Fizz" else (if read x `mod` 5 == 0 then "Buzz" else x)))


------------------------------- FizzBuzz Solution 3

-- Lower order fizzBuzz that's super readable
fizzBuzz'' :: (Integral a, Show a) => a -> String
fizzBuzz'' n
    | fizz && buzz  = "FizzBuzz"
    | fizz          = "Fizz"
    | buzz          = "Buzz"
    | otherwise     = show n
        where 
            fizz = n `mod` 3 == 0
            buzz = n `mod` 5 == 0

-- Using only higher order functions to use fizzBuzz on a list of numbers with show (Int, Integer). Built on basic FizzBuzz lower order function. Starting to get this
fullFizzBuzz :: (Integral a, Show a) => [a] -> String
fullFizzBuzz = unwords . map fizzBuzz''