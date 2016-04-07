module Combinatorics (
  nfib
) where

-- | Nth Fibonacci number
-- | https://en.wikipedia.org/wiki/Fibonacci_number
nfib :: Int -> Integer
nfib 0 = 0
nfib 1 = 1
nfib n = nfib (n - 1) + nfib (n - 2)
