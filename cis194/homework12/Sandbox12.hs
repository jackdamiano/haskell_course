module Sandbox12 where

import Control.Monad.Random

check :: Int -> Maybe Int
check n | n < 10 = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n = Just $ n `div` 2
        | otherwise = Nothing

ex01 = return 7 >>= check >>= halve
ex011 = check 7 >>= halve

ex02 = return 12 >>= check >>= halve

ex03 = return 12 >>= halve >>= check
ex031 = halve 12 >>= check

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1,x+2]

ex04 = [10,20,30] >>= addOneOrTwo -- ie, [11,12] : [21,22] : [31,32] concatenate values as you map k across xs

threeInts :: Rand StdGen (Int, Int, Int)
threeInts =
  getRandom >>= \i1 ->
  getRandom >>= \i2 ->
  getRandom >>= \i3 ->
  return (i1, i2, i3)

ex05 = threeInts

showThreeInts :: IO ()
showThreeInts = do
  result <- evalRandIO threeInts
  print result