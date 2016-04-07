module Vec2 (
  Vec2(..),
  addVec2,
  angleVec2,
  divVec2,
  dotVec2,
  Vec2.fromList,
  lengthVec2,
  lerpVec2,
  minusVec2,
  multVec2,
  normVec2,
  opVec2,
  subVec2,
  Vec2.toList
) where

import Data.List
import Data.Vector.Storable
import qualified Data.Vector.Storable as V
import Foreign
import Foreign.C.Types

-- Define a 2 element vector type
data Vec2 = Vec2 {
    xVec2 :: {-# UNPACK #-} !CFloat,
    yVec2 :: {-# UNPACK #-} !CFloat
} deriving(Eq, Show)

instance Storable Vec2
  where
    sizeOf _ = sizeOf (undefined :: CFloat) * 4
    alignment _ = alignment (undefined :: CFloat)

    {-# INLINE peek #-}
    peek p = do
        a <- peekElemOff q 0
        b <- peekElemOff q 1
        return (Vec2 a b)
      where
        q = castPtr p

    {-# INLINE poke #-}
    poke p (Vec2 a b) = do
        pokeElemOff q 0 a
        pokeElemOff q 1 b
      where
        q = castPtr p

-- | Add two vectors
addVec2 :: Vec2 -> Vec2 -> Vec2
{-# INLINE addVec2 #-}
addVec2 = opVec2 (+)

-- | Add two vectors
-- | Result is in radians
angleVec2 :: Vec2 -> Vec2 -> CFloat
{-# INLINE angleVec2 #-}
angleVec2 v0 v1 = acos a / b
  where
    a = dotVec2 v0 v1
    b = lengthVec2 v0 * lengthVec2 v1

-- | Divide vector by scalar
divVec2 :: CFloat -> Vec2 -> Vec2
{-# INLINE divVec2 #-}
divVec2 c = multVec2 (1/c)

-- | Dot product of two vectors
dotVec2 :: Vec2 -> Vec2 -> CFloat
{-# INLINE dotVec2 #-}
dotVec2 v0 v1 = x + y
  where
    (Vec2 x y) = opVec2 (*) v0 v1

-- | Constructs vector from List
fromList :: [CFloat] -> Vec2
{-# INLINE fromList #-}
fromList l
  | Data.List.length l == 2 = Vec2 x y
  | otherwise = error "List must have length 2"
  where
    x = Data.List.head l
    y = Data.List.last l

-- | Length of vector
lengthVec2 :: Vec2 -> CFloat
{-# INLINE lengthVec2 #-}
lengthVec2 v = sqrt ((x ** 2) + (y ** 2))
  where
    (Vec2 x y) = v

-- | Linear Interpolation
-- | https://en.wikipedia.org/wiki/Linear_interpolation
{-# INLINE lerpVec2 #-}
lerpVec2 :: CFloat -> Vec2 -> Vec2 -> Vec2
lerpVec2 c v0 v1 = addVec2 v0 (multVec2 c d)
  where
    d = subVec2 v1 v0

-- | Return oposite vector
minusVec2 :: Vec2 -> Vec2
{-# INLINE minusVec2 #-}
minusVec2 v = Vec2 (-x) (-y)
  where
    (Vec2 x y) = v

-- | Multiple vector by scalar
multVec2 :: CFloat -> Vec2 -> Vec2
{-# INLINE multVec2 #-}
multVec2 c v = Vec2 (x * c) (y * c)
  where
    (Vec2 x y) = v

-- | Normalize vector
normVec2 :: Vec2 -> Vec2
{-# INLINE normVec2 #-}
normVec2 v = Vec2 (x/l) (y/l)
  where
    (Vec2 x y) = v
    l = lengthVec2 v

-- | Binary operator between two vectors
opVec2 :: (CFloat -> CFloat -> CFloat) -> Vec2 -> Vec2 -> Vec2
{-# INLINE opVec2 #-}
opVec2 op v0 v1 = Vec2 (op x x') (op y y')
  where
    (Vec2 x y) = v0
    (Vec2 x' y') = v1

-- | Substract two vectors
subVec2 :: Vec2 -> Vec2 -> Vec2
{-# INLINE subVec2 #-}
subVec2 = opVec2 (-)

-- | Converts vector to List
toList :: Vec2 -> [CFloat]
{-# INLINE toList #-}
toList v = [x, y]
  where
    (Vec2 x y) = v
