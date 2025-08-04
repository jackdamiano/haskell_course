module Sandbox3 where


-- t is a type variable; polypmorphic type. designated by lowercase letter (not just t)
data List t = E | C t (List t)
  deriving (Show)

-- Because we use guards FilterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
  | p x = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (a->b) -> List a -> List b -- a->b is specified so we can use different types simultaneously. t->t would just allow for a single type to be input/output. (a->b) acts similarly to a function pointer in C. you input a function to it
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)

totalFunction :: [Int] -> Int
totalFunction [] = 0
totalFunction [_] = 0
totalFunction (x1:x2:_) = x1 + x2

-- If writing partial functions, use Maybe to indicate possible failure
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
--This function is still partial in a way, but is made safe by its explicit type requirement to acknowledge it

