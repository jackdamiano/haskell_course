{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List
import Data.Ord

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

-- allowed attack units per battle
numAttackers :: Battlefield -> Army
numAttackers bf
  | attackers bf > 3 = 3
  | otherwise = attackers bf - 1

-- allowed defend units per battle
numDefenders :: Battlefield -> Army
numDefenders bf
  | defenders bf >= 2 = 2
  | otherwise = defenders bf

-- takes in an army and rolls dice
generateScores :: Army -> Rand StdGen [DieValue]
generateScores n = sortBy (comparing Data.Ord.Down) <$> replicateM n die -- had reverse . sort but hlint suggested this

-- takes in the active attack units, active defense units and outputs their outcomes
commenceBloodshed :: [DieValue] -> [DieValue] -> [Bool]
commenceBloodshed = zipWith (>)

-- takes outcomes and reports on how many units were lost
determineLosses :: [Bool] -> (Army,Army)
determineLosses bs = (attackerCasualties, defenderCasualties) where
  attackerCasualties = length (filter (== True) bs)
  defenderCasualties = length (filter (== False) bs)


-- bind takes a Rand StdGen [DieValue] and returns just [DieValue] because we bind m a (in this case Rand StdGen [DieValue])
-- and output lambda function \a, which just gives us a pure [DieValue].
-- We can then take that as arguments for our bloodshed function and determine losses  from there.
-- We save the results in a let assignment pair
-- Then we return (put back into monadic context) the new battlefield
battle' :: Battlefield -> Rand StdGen Battlefield
battle' bf =
  generateScores (numAttackers bf) >>= \attackerStrength' ->
  generateScores (numDefenders bf) >>= \defenderStrength' ->
  let (attackerLosses', defenderLosses') = determineLosses $ commenceBloodshed attackerStrength' defenderStrength'
  in return $ Battlefield (attackers bf - attackerLosses') (defenders bf - defenderLosses')

-- This implementation just sugars the bind syntax into do notation. Need to work on my 'lets' 'wheres' and 'ins' practice
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  attackerStrength <- generateScores $ numAttackers bf
  defenderStrength <- generateScores $ numDefenders bf
  let (attackerLosses, defenderLosses) = determineLosses $ commenceBloodshed attackerStrength defenderStrength
  return $ Battlefield (attackers bf - attackerLosses) (defenders bf - defenderLosses)
 
doBattle :: IO()
doBattle = do
  result <- evalRandIO $ battle (Battlefield 3 3)
  print result


-------------------- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  newBattle <- battle bf
  let veteranAttackers  = attackers newBattle
      veteranDefenders  = defenders newBattle
  case (veteranAttackers, veteranDefenders) of
    (1,_) -> return newBattle
    (0,_) -> return newBattle
    (_,0) -> return newBattle
    _     -> invade newBattle


doInvasion :: IO ()
doInvasion = do
  result <- evalRandIO $ invade (Battlefield 7 4)
  print result


-------------------- Exercise 4

bf01 = Battlefield 5 5
bf02 = Battlefield 100 50
bf03 = Battlefield 50 100
bf04 = Battlefield 7 10
bf05 = Battlefield 10 7
bf06 = Battlefield 50 50

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  battleHistory <- replicateM 1000 (invade bf) -- gives a [Battlefield]. very cool how keeping everything within its own Rand StdGen context allows each result to be different
  let wins = length $ filter (\x -> defenders x == 0) battleHistory -- each element is a Battlefield, so we get the defenders from that battlefield. if 0, attackers won.
      probabilityOfSuccess = fromIntegral wins / 1000.0 
  return probabilityOfSuccess

--need more IO practice. This is good but need the muscle memory
doProbCalculation :: IO ()
doProbCalculation = do
  putStrLn "Enter 'preset' for pre-defined battles, or 'custom' for custom battles"
  mode <- getLine
  battlefield <- case mode of
    "preset" -> do
      putStrLn "Select preset 1-6"
      preset <- getLine
      return $ case preset of
        "1" -> bf01
        "2" -> bf02
        "3" -> bf03
        "4" -> bf04
        "5" -> bf05
        _ -> bf06
    "custom" -> do
      putStrLn "How many attackers do you want?"
      attackers <- readLn
      putStrLn "How many defenders do you want?"
      defenders <- readLn
      return $ Battlefield attackers defenders
  result <- evalRandIO $ successProb battlefield
  print result
  