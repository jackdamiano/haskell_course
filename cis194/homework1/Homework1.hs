module Homework1 where

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs

-- | Convert a number to a list of digits
toDigits :: Integer -> [Integer]
toDigits n
  | n < 0 = []
  | n < 10 = [n] 
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- | Double every other digit from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse [if odd i then x * 2 else x | (i,x) <-  zip [0..] (reverse xs) ]

-- | Sum all digits in a list
sumDigits :: [Integer] -> Integer
sumDigits xs = sum [if x > 10 then x `div` 10 + x `mod` 10 else x| x <- xs]

-- | Validate a credit card number
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- Example usage:
-- validate 4012888888881881  -- Should return True
-- validate 4012888888881882  -- Should return False 

type Peg = String
type Move = (Peg,Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

