{-# LANGUAGE FlexibleInstances #-}
module Main where -- Adjusted to be used as executable in Cabal file
import Sized
import Scrabble
import Buffer
import Editor
import Control.Monad.State

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving(Show, Eq)


------------------------ Exercise 1

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b


------------------------ Exercise 2

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

-- finds the JoinList element at the specified index
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty      = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing -- edge case where the tree indexes are out of bounds
indexJ i (Append _ l r)
                | i < leftSize = indexJ i l
                | otherwise = indexJ (i - leftSize) r
                where leftSize = getSize . size $ tag l

-- removes the first n items, returning the rest
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n x | n <= 0 = x -- returns just the list if you aren't dropping positive, non-zero n
dropJ _ (Single _ _) = Empty -- if we need to drop something > 0 and we're in a Single, we can only drop ourselves
dropJ n (Append _ l r)
                    | n == leftSize = r -- if already at our drop point, just return the rest of the list
                    | n < leftSize = dropJ n l +++ r -- if n < our current spot, keep going left until we find out spot (append everything r)
                    | otherwise = dropJ (n - leftSize) r -- if n > our current spot, drop everything to the left and keep chasing r
                    where leftSize = getSize . size $ tag l

-- returns the first n items
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ x@(Single _ _) = x
takeJ n (Append _ l r)
                  | n == leftSize = l -- if the left has the # you want, return it
                  | n > leftSize = l +++ takeJ (n - leftSize) r -- if you need more than what the left has to offer, append the left and take the remaining from the right
                  | otherwise = takeJ n l -- if the left size is greater than what you want, takeJ from the left subtree
                  where leftSize = getSize . size $ tag l


------------------------ Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine x = Single (scoreString x) x


------------------------ Exercise 4

instance Buffer (JoinList (Score, Size) String) where
    toString            = unlines . jlToList
    fromString          = foldr ((+++) . (\s -> Single (scoreString s, 1) s)) Empty . lines -- Need to use and see many more folds; hlints recommendations aren't natural to me yet
    line                = indexJ
    replaceLine n s jl  = takeJ n jl +++ Single(scoreString s, 1) s +++ dropJ (n+1) jl -- assumes we can only edit one line at a time. This is what description in Buffer calls for
    numLines            = getSize . size . tag
    value               = getScore . fst . tag -- assuming we want the value to reflect the scrabble score, not num of words

main = runEditor editor (fromString $ unlines
         [ "This *New & Improved* buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: (JoinList (Score, Size) String))