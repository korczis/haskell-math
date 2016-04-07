module Random (
  rand
) where

import System.Random

rand :: (RandomGen g, Random a) => a -> a -> g -> [a]
rand l r = randomRs (l, r)

randomList :: (RandomGen g, Random a) => Int -> g -> [a]
randomList g = randoms
