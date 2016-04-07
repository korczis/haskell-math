module Vec3 (
  Vec3(..),
  addVec3,
  angleVec3,
  crossVec3,
  divVec3,
  dotVec3,
  Vec3.fromList,
  lengthVec3,
  lerpVec3,
  minusVec3,
  multVec3,
  normVec3,
  opVec3,
  subVec3,
  Vec3.toList
) where

import Data.List
import Data.Vector.Storable
import qualified Data.Vector.Storable as V
import Foreign
import Foreign.C.Types

-- Define a 3 element vector type
data Vec3 = Vec3 {
    xVec3 :: {-# UNPACK #-} !CFloat,
    yVec3 :: {-# UNPACK #-} !CFloat,
    zVec3 :: {-# UNPACK #-} !CFloat
} deriving(Eq, Show)

instance Storable Vec3
  where
    sizeOf _ = sizeOf (undefined :: CFloat) * 4
    alignment _ = alignment (undefined :: CFloat)

    {-# INLINE peek #-}
    peek p = do
        a <- peekElemOff q 0
        b <- peekElemOff q 1
        c <- peekElemOff q 2
        return (Vec3 a b c)
      where
        q = castPtr p

    {-# INLINE poke #-}
    poke p (Vec3 a b c) = do
        pokeElemOff q 0 a
        pokeElemOff q 1 b
        pokeElemOff q 2 c
      where
        q = castPtr p

-- | Add two vectors
addVec3 :: Vec3 -> Vec3 -> Vec3
{-# INLINE addVec3 #-}
addVec3 = opVec3 (+)

-- | Add two vectors
-- | Result is in radians
angleVec3 :: Vec3 -> Vec3 -> CFloat
{-# INLINE angleVec3 #-}
angleVec3 v0 v1 = acos a / b
  where
    a = dotVec3 v0 v1
    b = lengthVec3 v0 * lengthVec3 v1

-- | Cross product of two vectors
crossVec3 :: Vec3 -> Vec3 -> Vec3
{-# INLINE crossVec3 #-}
crossVec3 v0 v1 = Vec3 (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')
  where
    (Vec3 x y z) = v0
    (Vec3 x' y' z') = v1

-- | Divide vector by scalar
divVec3 :: CFloat -> Vec3 -> Vec3
{-# INLINE divVec3 #-}
divVec3 c = multVec3 (1/c)

-- | Dot product of two vectors
dotVec3 :: Vec3 -> Vec3 -> CFloat
{-# INLINE dotVec3 #-}
dotVec3 v0 v1 = x + y + z
  where
    (Vec3 x y z) = opVec3 (*) v0 v1

-- | Constructs vector from List
fromList :: [CFloat] -> Vec3
{-# INLINE fromList #-}
fromList l
  | Data.List.length l == 3 = Vec3 x y z
  | otherwise = error "List must have length 3"
  where
    x = Data.List.head l
    y = l !! 1
    z = Data.List.last l

-- | Length of vector
lengthVec3 :: Vec3 -> CFloat
{-# INLINE lengthVec3 #-}
lengthVec3 v = sqrt ((x ** 2) + (y ** 2) + (z ** 2))
  where
    (Vec3 x y z) = v

-- | Linear Interpolation
-- | https://en.wikipedia.org/wiki/Linear_interpolation
{-# INLINE lerpVec3 #-}
lerpVec3 :: CFloat -> Vec3 -> Vec3 -> Vec3
lerpVec3 c v0 v1 = addVec3 v0 (multVec3 c d)
  where
    d = subVec3 v1 v0

-- | Return oposite vector
minusVec3 :: Vec3 -> Vec3
{-# INLINE minusVec3 #-}
minusVec3 v = Vec3 (-x) (-y) (-z)
  where
    (Vec3 x y z) = v

-- | Multiple vector by scalar
multVec3 :: CFloat -> Vec3 -> Vec3
{-# INLINE multVec3 #-}
multVec3 c v = Vec3 (x * c) (y * c) (z * c)
  where
    (Vec3 x y z) = v

-- | Normalize vector
normVec3 :: Vec3 -> Vec3
{-# INLINE normVec3 #-}
normVec3 v = Vec3 (x/l) (y/l) (z/l)
  where
    (Vec3 x y z) = v
    l = lengthVec3 v

-- | Binary operator between two vectors
opVec3 :: (CFloat -> CFloat -> CFloat) -> Vec3 -> Vec3 -> Vec3
{-# INLINE opVec3 #-}
opVec3 op v0 v1 = Vec3 (op x x') (op y y') (op z z')
  where
    (Vec3 x y z) = v0
    (Vec3 x' y' z') = v1

-- | Substract two vectors
subVec3 :: Vec3 -> Vec3 -> Vec3
{-# INLINE subVec3 #-}
subVec3 = opVec3 (-)

-- | Converts vector to List
toList :: Vec3 -> [CFloat]
{-# INLINE toList #-}
toList v = [x, y, z]
  where
    (Vec3 x y z) = v
