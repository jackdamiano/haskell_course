{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import Data.Char

newtype Score = Score Int
    deriving (Show, Eq, Ord, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = Score 0

--pointRef = zip [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10] ['A'..'Z']

score :: Char -> Score
score x
    | c `elem` "aeilnorstu" = Score 1
    | c `elem` "dg"         = Score 2
    | c `elem` "bcmp"       = Score 3
    | c `elem` "fhvwy"      = Score 4
    | c `elem` "k"          = Score 5
    | c `elem` "jx"         = Score 8
    | c `elem` "qz"         = Score 10
    | otherwise             = Score 0
    where c = toLower x

scoreString :: String -> Score
scoreString = foldr (\x s -> score x + s) (Score 0)