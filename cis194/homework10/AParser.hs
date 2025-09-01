{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------


----------------------------------- Exercise 1

first :: (a->b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

-- A newtype thats a function and not a value has been a bit difficult to wrap my mind around.
-- Essentially, fmap is not returning Just (first f aPair) or Nothing,
-- It's returning the function of checking runParser :: Parser a -> String -> Maybe (a,String). The case statement acts as the return function
-- ie, it takes a Parser and a String xs and returns a Maybe (a, String). The case statement is what allows for it to return a function and not a value only.
-- I think.
-- fmap :: (a->b) -> f a -> f b
instance Functor Parser where
  fmap f pa = Parser pb
    where
      pb xs = case runParser pa xs of
              Nothing -> Nothing
              Just aPair -> Just (first f aPair)


----------------------------------- Exercise 2

--  pure :: a -> f a
-- (<*>) :: f (a->b) -> f a -> f b
instance Applicative Parser where
  pure a = Parser (\input -> Just (a,input))
  pf <*> pa = Parser pb
    where
      pb input =  case runParser pf input of --unwrap the function with the input
        Nothing -> Nothing
        Just (f, rest) -> runParser (fmap f pa) rest -- we get a function, and the rest. apply fmap to apply that context free function to pa with the rest of the string and thus output the remaining case of Parser b


----------------------------------- Exercise 3

makePair :: Char -> Char -> (Char, Char)
makePair a b = (a,b)

-- We can still use satisfy since it's a primitive. I was wracking my brain how to build this without it
abParser :: Parser (Char, Char)
abParser = makePair <$> char 'a' <*> char 'b'

abCheck :: Char -> Char -> ()
abCheck a b = ()

abParser_ :: Parser ()
abParser_ = abCheck <$> char 'a' <*> char 'b'

makeIntPair :: Integer -> Integer -> [Integer]
makeIntPair x y = [x,y]

removeSpace:: Char -> Integer -> Integer
removeSpace c i = i

intPair :: Parser [Integer]
intPair = makeIntPair <$> posInt <*> (removeSpace <$> char ' ' <*> posInt)


----------------------------------- Exercise 4

-- instance Alternative Maybe where
--     empty = Nothing
--     Just a <|> _ = Just a
--     Nothing <|> b = b

-- empty :: f a
-- (<|>) :: f a -> f a -> f a
instance Alternative Parser where
  empty = Parser e
    where
      e input = Nothing
  pa <|> pb = Parser pc
    where
      pc input = case runParser pa input of
        Just (a, rest) -> Just (a, rest) -- if Just case for pa, return it
        Nothing -> runParser pb input -- else return whatever pb evaluates to


----------------------------------- Exercise 5

makeEmptyTuple :: a -> ()
makeEmptyTuple _ = ()

intOrUppercase :: Parser ()
intOrUppercase = (makeEmptyTuple <$> posInt) <|> (makeEmptyTuple <$> satisfy isUpper)