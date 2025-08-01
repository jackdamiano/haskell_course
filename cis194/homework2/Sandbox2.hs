module Sandbox2 where

data BookInfo = Book Int String [String]
                deriving (Show)

-- difference detween data x = c and c :: x?

data List a = Cons a (List a)
            | Nil
              deriving(Show) 

fromList (x:xs) = Cons x (fromList xs) -- doing it this way means fromList can only pattern match with List type
fromList [] = Nil

toList (Cons x xs) = x : (toList xs)
toList Nil = []

randomList (x:xs) = Cons x (randomList xs)
randomList [] = Nil

newList (x:xs) = x : (newList xs)
newList [] = []

data MyType = IntVal Int
            | CharVal Char
              deriving(Show)

x = IntVal 3
y = CharVal 'a'

--polyFunc :: a -> b -> c (wrong)
polyFunc (IntVal a) b = [b + a] -- actually inferred MyType -> Int -> [Int] on its own. 
polyFunc (CharVal a) b = [b]


data ListType = IntList [Int]
              | CharList [Char]
                deriving (Show)


polyComplex :: MyType -> Int -> ListType
polyComplex (IntVal a) b = IntList [b + fromIntegral a]
polyComplex (CharVal a) b = CharList [a]

data Tree a = Node a (Tree a) (Tree a)
            | Empty
             deriving(Show)

-- Maybe is pre-defined
-- data Maybe a = Just a
--              | Nothing

data SimpleTree a = SimpleNode a (Maybe (SimpleTree a)) (Maybe (SimpleTree a)) -- Need to specify (Just x) when specifying children
                    deriving(Show)

child = SimpleNode "Child" Nothing Nothing --for convenience

--simplerTree :: a -> SimpleTree a -> SimpleTree a -> SimpleTree a --optional
simplerTree parent c1 c2 = SimpleNode parent (Just c1) (Just c2) -- This fixes the annoyances of referencing the value constructor directly

pluralize :: String -> [Int] -> [String]
pluralize word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"
        
-- data IntList = Const Int IntList
--              | Empty
             