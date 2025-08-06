module Golf where
import Safe
import Data.List 

test0 = [True,False]
test1 = "Hello!"
test2 = [1]
test4 =[1,2,3,4,5,6]
test5 = [True,False,True,True]

-- every list entry outputs nth type from the input list. ie, "Hello" returns "Hello", "e,l", "l", "l", "o". Output list of list should be the same length as input list
skips :: [a] -> [[a]]
skips xs = [match i xs | i <- [1..length xs]] -- creates a numbered list to count # of elements in the list
    where match i xs = [x | (n,x) <- zip [1..] xs, n `mod` i == 0] -- match outputs [a] for each a. It checks if the currIndex has no remainder when divided by its position in the list of lists. essentially acts as counting every nth element

max0 = [2,9,5,6,1]
max1 = [2,3,4,1,5]
max2 = [1,2,3,4,5]
max3 = [2,9,5,6,1,4,3,5,5,7,8,6,9,7]

-- finds all local maxima in a list. Since the edges of local maxima can never themselves be local maxima, only check those in middle.
-- otherwise, check to see if the rightmost (x3) is a local maxima by calling the function from the second element of the input array, onward
localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
    | x2 > x1 && x2 > x3   = x2 : localMaxima (x3:xs)
    | otherwise            = localMaxima (x2:x3:xs)
localMaxima _ = []

hist0 = [1,1,1,5]
hist1 = [1,4,5,4,6,6,3,4,2,4,9]
hist2 = [3,5]

-- takes in a list of integers 0-9 and outputs a histogram for the quantity of each number
-- sorts the occurences of each number and then goes backwards from highest quantity to lowest, outputting a line. Then we undo all the lines and put it as a fat string
-- logic takes the max + 1 (due to spacing) and applies the max value gathered from occurences output from the maximum on down for each line.
histogram :: [Integer] -> String
histogram xs = unlines (map (drawLine occ) [max+1,max..1]) ++ "==========\n0123456789\n" --max+1 is required to match spacing. Also, hlint had me change (\m -> drawLine occ m). Still not sure on partial application
    where 
        occ = occurrences xs
        max = maximum occ -- TODO: is this a bad partial function?


-- returns a line with the correct plotted *'s
-- takes in a list where each index corresponds to [0..9] and each value is the quantity of each. if value at x matches, then put a * for that row
drawLine :: [Int] -> (Int -> String)
drawLine xs max = [if x >= max then '*' else ' ' | x <- xs]


--Make a list of all numbers 0-9 and put the quantities each number appears in each. This was a bender
occurrences :: [Integer] -> [Int]
occurrences xs = map (\n -> length (filter (== n) xs)) [0..9]
