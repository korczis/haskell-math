{-# LANGUAGE FlexibleContexts #-}

module Main (
  hist,
  main
) where

-- | Global imports
import Data.List
import System.Random

import System.Environment (
    getArgs
  )

-- | Local imports
import Calculus
import Combinatorics
import Distribution
import Geometry
import Random
import Statistics
import Units
import Vec2
import Vec3

-- | Generate histogram
-- | xs - Input list
-- | n - Number of buckets
hist xs n = [floor (min (n - 1)) ((x - l) / i) | x <- xs]
  where
    s = sort xs
    l = head s
    r = last s
    range = r - l
    i = range / n

-- | Main entry point
main :: IO ()
main =  do
  [n] <- getArgs
  g <- getStdGen
  print n
  print . take (read n :: Int) $ rand (0 :: Float) (1 :: Float) g
