module Homework4 where
import Data.List

----------------------- Exercise 1

fun1_0 = [1,3,4,5,6]
fun1_1 = [1,1,1,1,16]
fun1_2 = [1,3,4,5,4,55,44,3,6,5,8,7]
fun1_3 = [1,5,3,8,6,9,7,23,54,76,18,39]

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1' xs

-- filters input by even, subtracts 2 from each remaining, then multiplies them together
fun1 :: [Integer] -> Integer
fun1 = product . map(\x -> x-2) . filter even

-- hailstone function that sums all even values in the sequence
fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n | even n    = n + fun2' (n `div` 2)
        | otherwise = fun2' (3 * n + 1)

-- iterates through and makes hailstone sequence for all values > 1, then sorts out the evens and sums them together
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1) -- Haskell is pretty rad ngl


----------------------- Exercise 2

fold_0 = [1,2,3,4,5,6]

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)


-- creates a balanced BST by sorting the list, finding the middle element of each half of the list recursively, reversing the result to work with foldr, inserting into each other and folding into one great whole
-- not gonna lie, I barely understand how this works and I wrote it. Considering I essentially make a balanced tree to make a balanced tree, theres got to be a better way.
-- When I search online, everyone skipped this. Was it excluded? It took forever.
foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insertBST Leaf . reverse . middleFirst . sort -- this blows my mind. The fact that insertBst doesn't need to specify its second input beacause foldr literally provides it as the initial state
--foldTree xs = foldr insertBST Leaf . reverse . middleFirst $ sort xs

------ old attempt. didn't work. Leaving in here to show future thought process of how I eventually got to middleFirst
-- zips together a zipped tuple pair into an interleaved list with the first half reversed. ie, ([1,2,3],[4,5,6]) -> [3,4,2,5,1,6].
-- This will create a balanced BST when inserted by a BST insert function
interleave :: ([a],[a]) -> [a]
interleave (xs,ys) = concatMap (\(x,y) -> [x,y]) (zip ys xs)

--function which always picks the middle number in a list, splits that list, and finds the middle numbers in those, and so on
middleFirst :: [a] -> [a]
middleFirst [] = []
middleFirst [x] = [x]
middleFirst xs =
        let half = length xs `div` 2
            (left, right) = splitAt half xs
        in case right of
            [] -> middleFirst left
            (m:rest)-> m : middleFirst left ++ middleFirst rest

-- inserts a new value into a BST
insertBST :: (Ord a) => a -> Tree a -> Tree a
insertBST val Leaf = Node 0 Leaf val Leaf
insertBST val (Node _ leftRoot rootVal rightRoot) 
        | val < rootVal =
            let newLeft = insertBST val leftRoot
                height = 1 + max (getHeight newLeft) (getHeight rightRoot)
            in Node height newLeft rootVal rightRoot
        | val > rootVal =
            let newRight = insertBST val rightRoot
                height = 1 + max (getHeight leftRoot) (getHeight newRight)
            in Node height leftRoot rootVal newRight
        | otherwise = Node (1 + max (getHeight leftRoot) (getHeight rightRoot)) leftRoot rootVal rightRoot -- no duplicates allowed; node simply goes into same spot on tree


-- Gets the height of a tree
getHeight :: Tree a -> Integer
getHeight Leaf = -1 -- since we start at index 0
getHeight (Node h _ _ _) = h


----------------------- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (==) True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y ->f x:y) []

----------------------- Exercise 4 skipped for the sake of time. 

-- sieveSundaram :: Integer -> [Integer]
-- sieveSundaram x = 2: map(\n -> 2 * n + 1)

-- cartProd:: [a] -> [b] -> [(a,b)]
-- cartProd xs ys = [(x,y) | x <- xs, y <- ys]
