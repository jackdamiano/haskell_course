module Sandbox7 where
import Data.Monoid 

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

testTree1 = Node (Node (Node Empty 1 Empty ) 2 Empty) 3 Empty
testTree2 = Node (Node (Node Empty 1 Empty ) 2 Empty) 3 (Node Empty 4 Empty)
testTree3 = Empty

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty          = e
treeFold e f (Node l x r)   = f (treeFold e f l) x (treeFold e f r)

treeSize :: Tree a -> Integer
treeSize = treeFold 0 (\l _ r -> 1 + l + r)

treeSum :: Num a => Tree a -> a
treeSum = treeFold 0 (\l x r -> l + x + r)

treeDepth :: Tree a -> Integer
treeDepth = treeFold 0 (\l _ r -> 1 + max l r)

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
         
testExprT = Mul (Add (Lit 1) (Lit 2)) (Lit 5) 

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add a1 a2) = g (exprTFold f g h a1) (exprTFold f g h a2)
exprTFold f g h (Mul a1 a2) = h (exprTFold f g h a1) (exprTFold f g h a2)

eval :: ExprT -> Integer
eval = exprTFold id (+) (*)

numLiterals :: ExprT -> Integer
numLiterals = exprTFold (const 1) (+) (+) -- add 1 for each literal, (+) for all inputs since were just adding all literals. multiplying would * 1, which wouldnt add to a count

-- instance Semigroup (Either a b) where
--     Left _ <> b = b
--     a      <> _ = a

-- newtype Sum' a = Sum' a
--     deriving (Num, Eq, Show, Ord)

-- getSum :: Sum' a -> a
-- getSum (Sum' a) = a

-- instance Num a => Monoid (Sum' a) where
--     mempty = Sum' 0
--     mappend = (+)

