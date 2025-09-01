module Sandbox10 where
import Control.Applicative

test = Just 3
test2 = Just 5
test3 = Nothing

-- Applicatives allow us to take functions in context, and apply them to arguments/values in context
-- Functors only let you take a function NOT in context, and apply it to values in context
-- Applicatives build on this by adding <*> :: f (a -> b) -> f a -> f b
-- where f == 'context' or 'container'. Thus a container of function 'a->b' and a container of value type 'a' produce a value in the context of type 'b'

-- Why would we do this? Let me think. It makes sense to use functions that are pure as much as possible, but I guess
-- There might be/are situations where we might just be given values that aren't in the pure context, 
-- the only place I've seen that is IO () which seems which seems likely and frequent. So let's try and make a dumb example:

-- function that takes values within a context. ie, a function in context
maybeAdd :: Maybe (Integer -> Integer -> Integer)
maybeAdd = Just (\x y -> x + y)

-- we can apply that function in context to values in context
testAp6 = maybeAdd <*> test <*> test2

-- or explicitly,
testAp7 = Just (+) <*> test <*> test2

-- pure lifts add to the proper context, then applies it to the other two arguments. works for all contexts, not just Maybe
testAp = pure (+) <*> test <*> test2
-- respects Nothing
testAp2 = pure (+) <*> test <*> test3

-- <$> puts the function on the left into context for the values to the right. Additional arguments are 'applied' with <*>
testAp3 = (+) <$> test <*> test2

-- fmap is an alias for <$>
testAp4 = fmap (+) test <*> test2
testAp5 = (+) `fmap` test <*> test2

{- Lots of comments to reinforce learning.

Fmap can ONLY take a pure function with one argument and output the proper context f (a->b)

if we wanted an fmap2 we'd need the <*> / 'apply' operator to apply to mulliple arguments.

This is because <*> can take a function in context and a single value in context and output the new value in context
And because of currying we can do the following:
a -> b -> c = a -> (b -> c)

but if we apply that to a context like so:
f a -> f b -> f c = f a -> f (b -> c)
we cant work with fmap since functor/fmap CANNOT be used for functions within context
but with <*> and applicatives, we can. (there are some associativity quirks I am choosing to ignor- I mean "abstract away" for now, I think)

(<*>) :: f (a -> b) -> f a -> f b

soo, we can define a function fmap2 :: (a->b->c) as the first argument which can be written as a -> (b -> c)
and in the definition of that function, we can lift 'a' into 'f a' using `fmap` or `pure` or <$> and then lift 'b->c' into 'f (b->c)' by using <*>
then, the output will be f a -> (f b -> f c) which is equal to f a -> f b -> f c

thus fmap2 is possible and defined in haskell as liftA2 because it takes a pure function with two arguments and lifts the function into the context of its arguments and then applies it
-}

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' h fa fb = fmap h fa <*> fb

testLiftA2' = liftA2' (+) test test2
testLiftA2 = liftA2 (+) test test2

{- But what about if we want fmap3/liftA3 or fmap4/liftA4

Let's walk through liftA3 as an example (contexts must be applicative typeclass)

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

this is equivalent to:
liftA3 :: Applicative f => (a -> (b -> (c -> d))) -> f a -> f b -> f c -> f d

thus the input can be read as a -> (b -> (c -> d)). remember:
(<$>) :: (a->b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b

this is the implementation where h = (a -> b -> c -> d), let's break it down:

'final' results of operators are below them. keep your mind curried, not scrambled

<$>   [   rest into <*>  ]
 |    [                  ]        
f a -> f (b -> (c -> d)))

<$>    <*>  [rest into <*>]
 |      |   [             ]
f a -> f b -> f (c -> d)))

<$>    <*>        <*>
 |      |      |-------|
f a -> f b -> f c -> f d

actual definition (follow along above):
liftA3 h fa fb fc = h <$> fa <*> fb <*> fc

now lets see liftA4 -}

liftA4' :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4' h fa fb fc fd = h <$> fa <*> fb <*> fc <*> fd

testLiftA4' = liftA4' (\a b c d -> a + b + c + d) test test test3 test2     -- 3 + 3 + Nothing + 5 = Nothing
testLiftA4'2 = liftA4' (\a b c d -> a + b + c + d) test test (pure 4) test2 -- 3 + 3 + 4 + 5 = 15

--BONUS: notice how I used pure to lift into context for a value only? Pure can be used to lift a value or a function INTO a single context.
-- It CANNOT lift functions with multiple arguments with multiple or separate contexts
-- ie, don't mix and match contexts. Use pure to put an entire something into context. Ex. (a -> b), (a -> b -> c), a

-- Will NOT do
-- pure (+) :: Maybe Int -> Maybe Int -> Maybe Int

-- Will do
-- pure (+) :: Maybe (Int -> Int -> Int)
-- Lifts (+) as a function into Maybe
maybeAdd' :: Maybe (Integer -> Integer -> Integer)
maybeAdd' = pure (+)

testAdd' = maybeAdd' <*> test <*> test2