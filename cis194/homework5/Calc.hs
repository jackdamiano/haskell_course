{-# LANGUAGE FlexibleInstances #-} --Using flexible instances instead since we are making complex type instances
{-# OPTIONS_GHC -Wall #-}
module Calc where
import ExprT
import Parser (parseExp)
import qualified StackVM as VM


------------------------ Exercise 1

eval :: ExprT -> Integer
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Lit x) = x


------------------------ Exercise 2

evalStr :: String-> Maybe Integer
evalStr =  fmap eval . parseExp Lit Add Mul --Am I supposed to know fmap yet? fmap evaluates the underlying function, then applies the wrapper. ie, compute whats inside maybe
--evalStr x =  eval <$> parseExp Lit Add Mul x -- Infix for fmap


------------------------ Exercise 3

-- shows what a type class of Expr is expected to be able to do
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

-- how ExprT can be included as part of the Expr family
instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- how Integer can be included as part of the Expr family
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

-- assigns any Expr a to Expr ExprT
reify :: ExprT -> ExprT
reify = id

-- assigns any Expr a to Expr Integer
reify' :: Integer -> Integer
reify' = id


------------------------ Exercise 4

-- Already did Int above, let's do the others

newtype MinMax = MinMax Integer deriving (Eq, Show) -- wish I could just add "ord"
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Bool where
    lit x
        | x > 0 = True
        | otherwise = False
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer -- added to fix GHC warning
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp


------------------------ Exercise 5

instance Expr VM.Program where
    lit i = [VM.PushI i]
    add x y = x ++ y ++ [VM.Add]
    mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

-- fmap stackVM (compile "(3 * -4) + 5") shows that this logic is correct within GHCI