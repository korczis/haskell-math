module Statistics (
  geometric,
  harmonic,
  median,
  mean,
  nthroot,
  sd,
  variance,
  varianceRatio
) where

import Data.List

-- | Exported functions
-- foreign export ccall nthroot :: Float -> Float -> Float

-- | Geometric mean of values
geometric :: [Float] -> Float
geometric xs = nthroot (fromIntegral(length xs)) (product [x + 1 | x <- xs]) - 1

-- | Harmonic mean of values
harmonic :: (Floating a) => [a] -> a
harmonic xs = 1 / (sum [1 / x | x <- xs] / fromIntegral(length xs))

-- | Median
median :: (Fractional a, Ord a) => [a] -> a
median x
  | odd (length x) = s !! c
  | otherwise = mean [s !! c - 1, s !! c]
  where
    s = sort x
    c = floor(fromIntegral(length x) / 2)

-- | Arithmetic mean of values
mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral(length xs)

-- | Nth root
nthroot :: Float -> Float -> Float
nthroot n val = val ** (1 / n)

-- | Standard deviation
-- | https://en.wikipedia.org/wiki/Standard_deviation
sd :: (Floating a) => [a] -> a
sd xs = sqrt (s / fromIntegral(n - 1))
  where
    s = sum [(x - mean xs) ^ 2 | x <- xs]
    n = length xs

-- | Variance
variance :: (Floating a) => [a] -> a
variance xs = sum [(x - mean xs) ^ 2 | x <- xs] / (fromIntegral(length xs) - 1)

-- | Variance ratio
-- | TODO: Implement, requires pf() function
varianceRatio :: (Floating a, Ord a) => [a] -> [a] -> a
varianceRatio a b = v1 / v2
  where
    v1 = variance a
    v2 = variance b
    vr = max v1 v2 / min v1 v2
    df1 = if v1 > v2 then length a - 1 else length b - 1
    df2 = if v1 > v2 then length b - 1 else length a - 1
