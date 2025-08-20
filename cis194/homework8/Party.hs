{-# LANGUAGE FlexibleInstances #-}
module Party where
import Employee
import Data.Tree
import Data.List (sortOn)


------------ Exercise 1

-- Adds employee to guest list
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f1) (GL l f2) = GL (e : l) (f2 + f1)

testEmp = Emp "Jill" 21

testGL = GL [Emp "Stan" 9, Emp "Jack" 20] 29
testGL2 = GL [Emp "Raytheon" 0, Emp "Anduril" 17] 17

(+++) :: GuestList -> GuestList -> GuestList
(+++) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Semigroup GuestList where
    (<>) = (+++)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
            | gl1 > gl2 = gl1
            | otherwise = gl2

------------ Exercise 2

testTree = Node 1 [Node 2 [], Node 3 [Node 4 []]] :: Tree Int

-- to test fold function
sumTree :: Tree Int -> Int
sumTree = treeFold (\i is -> i + sum is)

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v l) = f v (map (treeFold f) l) -- do the function by providing the current value and a list of b by mapping the treeFold function across each child

--combines list of GuestLists and returns whichever is more fun, the big boss or the combined list; flawed logic and setup so this is technically wrong.
combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e = moreFun (GL [e] (empFun e)) . foldr (+++) mempty 

------------ Exercise 3

-- ARTIFACT: This, while valid syntax, is not needed since I already have an instance monoid and semigroup of GuestList. However, it still wouldnt be needed because I misread the question haha. But was so fun I had to keep it

-- instance Semigroup (GuestList,GuestList) where
--     (<>) = (+++!+++)

-- instance Monoid (GuestList,GuestList) where
--     mempty = (GL [] 0, GL [] 0)

-- -- IDK if this is good practice or not but it seems like a lot of fun. In haskell, what you abstract can't hurt you, right?
-- (+++!+++) :: (GuestList, GuestList) -> (GuestList, GuestList) -> (GuestList, GuestList)
-- (+++!+++) (GL l1 f1,GL r1 rf1) (GL l2 f2, GL r2 rf2) = (GL (l1 ++ l2) (f1 + f2), GL (r1 ++ r2) (rf1 + rf2))

-- The craziest part is the append and mempty are already defined for pairs as long as both objects in that pair are semigroups and monoids. Haskell is SO WILD
-- ie, foldr (+++) mempty [(GL,GL)] == foldr (+++!+++) mempty [(GL,GL)] FOR FREE
-- idk how I'm going to remember all this. Can't wait for it to just click


--TODO: this function isn't super readable to me. I had something different but implemented hlints suggestions. How do I have enough taste to know what is truly idiomatic?
-- returns the GuestList of the boss and all 
nextLevel :: Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)
nextLevel e gls = (withBoss, withoutBoss) where
                withBoss = glCons e (foldr ((+++) . snd) mempty gls) -- adds boss to guestList of all subtree guestLists that do not include subBoss
                withoutBoss = foldr ((+++) . uncurry moreFun) mempty gls -- uncurry takes a function and a tuple and passes (a,b) as a -> b inputs. useful for processing pairs individually. finds funnest subtree for each sub-boss



------------ Exercise 4

-- Again, simply WILD. so hard to trust and surrender to the abstraction
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


-- In summary, we defined a way to fold any function of (a -> [b] -> b)  of Tree a into value b.
-- We also defined how to take an employee and a list of guestList pairs and output the best GuestList with and without them
-- That function, called nextLevel, follows the (a -> [b] -> b) format, and thus eligible to be combined with our treeFold
-- so we just need to fold a provided tree with that function and select the moreFun of the pair. uncurry allows us to turn a -> b into (a,b)

------------ Exercise 5

generateList :: [Employee] -> String
generateList = unlines . map empName

main :: IO()
main = do
    employeeList <- readFile "homework8/company.txt"
    let bestGuestList = maxFun (read employeeList)
        GL employees funScore = bestGuestList
        sortedEmployees = generateList (sortOn empName employees)
    putStrLn ("Total Fun: " ++ show funScore)
    putStrLn sortedEmployees