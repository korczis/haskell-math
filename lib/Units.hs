module Units (
  deg2rad,
  onerad,
  rad2deg
) where

-- | Convert degrees to radians
deg2rad :: (Floating a) => a -> a
deg2rad a = a / onerad

-- | One radian in degrees
onerad :: (Floating a) => a
onerad = 180 / pi

-- | Convert radians to radians
rad2deg :: (Floating a) => a -> a
rad2deg a = a * onerad
