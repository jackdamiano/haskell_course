module Sandbox8 where
import Data.Char

main = putStrLn "Hello World!"
    
test0 = putStr "Hello " >> putStrLn "World!"
test1 = putStrLn "Please Enter a number: " >> (readLn >>= (\n -> putStrLn (show (n + 1))))

greet :: IO ()
greet = do
    putStrLn "What is your name?"
    name <- getLine
    let uname = map toUpper name
    putStrLn ("Hello, " ++ uname ++ ".")

count :: Int -> Int -> IO ()
count n m = do
    putStrLn (show n)
    if n < m then
        count (n + 1) m
    else
        return ()


rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x :) acc) []