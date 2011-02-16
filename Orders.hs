module Orders where

import Data.List (nub)

import Math.Combinat.Permutations (permuteMultiset)
import Math.Combinat.Partitions (partitionMultiset)

import Data.Random
import Data.Random.RVar
import Data.Random.Source.DevRandom
import System.IO.Unsafe

data ScaleResult a = ScaleResult 
    { srScale :: [[a]]
    , srAboveRandom :: Double
    } deriving (Show, Eq)

allScales :: Ord a => [a] -> [[[a]]]
allScales = concatMap permuteMultiset . partitionMultiset

isConsistent :: Ord a => [[a]] -> (a,a) -> Bool
isConsistent (xs:xss) (a,b)
                 | a `elem` xs = True
                 | b `elem` xs = False
                 | otherwise = isConsistent xss (a,b)

isStrictlyConsistent :: Ord a => [[a]] -> (a,a) -> Bool
isStrictlyConsistent (xs:xss) (a,b)
    | a `elem` xs && b `notElem` xs = True
    | b `elem` xs = False
    | otherwise = isConsistent xss (a,b)

numConsistent :: Ord a => ([[a]] -> (a,a) -> Bool) -> [[a]] -> [(a,a)] -> Int
numConsistent f scale = length . filter (f scale)

ratioConsistent :: (Ord a) => ([[a]] -> (a,a) -> Bool) -> [[a]] -> [(a,a)] -> Double
ratioConsistent f scale sample = consistent / total
    where
      total = fromIntegral $ length sample
      consistent = fromIntegral $ numConsistent f scale sample

ratioConsistentMinusRandom :: (Ord a) => ([[a]] -> (a,a) -> Bool) -> [[a]] -> [(a,a)] -> Double
ratioConsistentMinusRandom f scale sample = attested - random
    where
      attested = ratioConsistent f scale sample
      random = ratioConsistent f scale $ take 10000 $ randomPairs $ concat scale

randomTrialsBetter :: (Ord a) => ([[a]] -> (a,a) -> Bool) -> [[a]] -> [(a,a)] -> Double
randomTrialsBetter f scale sample = (/10000) 
                                    $ fromIntegral
                                    $ length 
                                    $ filter (> attested) 
                                    $ map randomTrial [0..10000]
    where
      n = length sample
      attested = ratioConsistent f scale sample
      randomTrial _ = ratioConsistent f scale $ take n $ randomPairs $ concat scale

randomPairs :: Ord a => [a] -> [(a,a)]
randomPairs xs = (a,b):(randomPairs xs)
    where
      a = unsafePerformIO $ runRVar (randomElement xs) DevURandom
      b = unsafePerformIO $ runRVar (randomElement xs) DevURandom