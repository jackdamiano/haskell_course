module Sandbox1 where

x :: Int
x = 3

-- Infinite loop
{-
y:: Int
y = y + 1
-}

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

n :: Integer
n = 123456789123456789000

reallyBig :: Integer
reallyBig = 2 ^ (2 ^ (2 ^ (2 ^ 2)))

numDigits :: Int
numDigits = length (show reallyBig)

d1, d2 :: Double
d1 = 4.53222
d2 = 6.3e-4

s1 :: Float
s1 = 6.3e-4

b1 :: Bool
b1 = True

b2 = False

c1, c2 :: Char
c1 = 'x'
c2 = '3'

s :: String
s = "Hello, Haskell!"

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3


isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

p :: (Int, Char)
p = (3, 'x')

sumPair:: (Int,Int) -> Int
sumPair (x,y) = x + y

f :: Int -> Int -> Int -> Int
f x y z = x + y + z

nums, range, range2 :: [Integer]
nums = [1,2,3,13]
range = [1..100]
range2 = [2,4..100]

hello, world, text :: String
hello = "Hello"
world = "World"
text = hello ++ " " ++ world

