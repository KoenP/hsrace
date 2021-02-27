module Angle where

import GHC.Generics

-- 0 rad points upwards
-- rotation is clockwise

newtype Radians a = Radians { _unRadians :: a} deriving (Show, Eq, Ord, Functor, Generic)
newtype Degrees a = Degrees { _unDegrees :: a} deriving (Show, Eq, Ord, Functor, Generic)

instance Applicative Radians where
  pure                        = Radians
  (Radians f) <*> (Radians x) = Radians (f x)

instance Num a => Num (Radians a) where
  a + b       = (+) <$> a <*> b
  a - b       = (-) <$> a <*> b
  _ * _       = error "Multiplying angles is meaningless"
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = Radians . fromInteger

instance Fractional a => Fractional (Radians a) where
  a / b        = (/) <$> a <*> b
  recip        = fmap recip
  fromRational = Radians . fromRational
  
instance Floating a => Floating (Radians a) where
  pi    = Radians pi
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  acos  = fmap acos
  atan  = fmap atan
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

type Angle    = Radians Double
type PicAngle = Degrees Float

radiansApply :: (a -> b) -> Radians a -> b
radiansApply f (Radians x) = f x

rsin :: Floating a => Radians a -> a
rsin = radiansApply sin

rcos :: Floating a => Radians a -> a
rcos = radiansApply cos

rtan :: Floating a => Radians a -> a
rtan = radiansApply tan

toPicAngle :: Angle -> PicAngle
toPicAngle = fmap realToFrac . radToDeg

fromPicAngle :: PicAngle -> Angle
fromPicAngle = degToRad . fmap realToFrac

radToDeg :: Floating a => Radians a -> Degrees a
radToDeg (Radians t) = let factor = 180 / pi in Degrees (t * factor)

degToRad :: Floating a => Degrees a -> Radians a
degToRad (Degrees t) = let factor = pi / 180 in Radians (t * factor)
