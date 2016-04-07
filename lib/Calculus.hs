module Calculus (
  integrate
) where

-- | Integrate function of one variable
-- | f - function
-- | l - Left (lower) bound
-- | r - Right (upper) bound
-- | n - Number of steps
integrate :: (Fractional a, Ord a, Integral b) => (a -> a) -> a -> a -> b -> a
integrate f l r n
  | l < r = f l * d + integrate f (l + d) r (n - 1)
  | otherwise = 0
  where
    d = (r - l) / fromIntegral n
