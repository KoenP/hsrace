{-# options_ghc -Wno-orphans #-}

module Vec where

-- module Vec ( Vec
--            -- , Pose (..)
--            -- , V2(..)
--            , ViewPort (ViewPort)
--            , WindowVec (WindowVec)
--            , upVec
--            , downVec
--            -- , applyVec
--            , rotVec
--            , vecAngle
--            , roundVec
--            , clampVec
--            , scalarProjectionOnto
--            , windowCoordsToWorldCoords
--            , between
--            , clamp
--            , (<->)
--            -- , L.perp
--            -- , L.sumV
--            -- , L.distance
--            ) where

--------------------------------------------------------------------------------
-- import Linear (V2(..))
-- import qualified Linear as L
-- import qualified Linear.Affine as LA
-- import qualified Linear.Metric as LM

import Angle

import Data.List
import Data.Coerce
-- import VectorSpace ( (^*), VectorSpace(..) )
--------------------------------------------------------------------------------

-- Phantom type variable `w` keeps track of the coordinate system.
-- TODO: make it a newtype.
-- type Vec w = V2 Double
data Vec w = Vec { _x :: !Double , _y :: !Double }
  deriving (Eq, Ord, Show, Read)

infixl 6 ^+^
infixl 6 ^-^
infix 7 ^*
infix 7 *^

class VectorSpace v a | v -> a where
  zeroVec   :: v
  (^+^)     :: v -> v -> v
  (^-^)     :: v -> v -> v
  (*^)      :: a -> v -> v
  (^/)      :: v -> a -> v
  neg       :: v -> v
  dot       :: v -> v -> a
  norm      :: v -> a
  normalize :: v -> v

instance VectorSpace (Vec w) Double where
  zeroVec                   = Vec 0 0
  (^+^)                     = zipWithVec (+)
  (^-^)                     = zipWithVec (-)
  v ^/ a                    = mapVec (/a) v
  a *^ v                    = mapVec (a*) v
  neg                       = mapVec negate
  Vec x1 y1 `dot` Vec x2 y2 = x1*x2 + y1*y2
  norm (Vec x y)            = sqrt (x*x + y*y)
  normalize v               = let norm_v = norm v
                              in if nearZero norm_v then zeroVec else  v ^/ norm_v
instance VectorSpace Int Int where
  zeroVec   = 0
  (^+^)     = (+)
  (^-^)     = (-)
  (*^)      = (*)
  (^/)      = div
  neg       = negate
  dot       = (*)
  norm      = id
  normalize = const 1

instance VectorSpace Double Double where
  zeroVec   = 0
  (^+^)     = (+)
  (^-^)     = (-)
  (*^)      = (*)
  (^/)      = (/)
  neg       = negate
  dot       = (*)
  norm      = id
  normalize = const 1

instance Fractional a => VectorSpace (Radians a) a where
  zeroVec            = 0
  (^+^)              = (+)
  (^-^)              = (-)
  a *^ Radians theta = Radians (a * theta)
  Radians theta ^/ a = Radians (theta / a)
  neg                = negate
  dot theta1 theta2  = coerce theta1 * coerce theta2
  norm               = coerce
  normalize          = const 1

(^*) :: VectorSpace v a => v -> a -> v
(^*) = flip (*^)

sumV :: Traversable t => t (Vec w) -> Vec w
sumV = foldl (^+^) zeroVec

toTup :: Fractional a => Vec w -> (a,a)
toTup (Vec x y) = (realToFrac x, realToFrac y)

toTupIntegral :: Integral a => Vec w -> (a,a)
toTupIntegral (Vec x y) = (round x, round y)

fromTup :: Real a => (a,a) -> Vec w
fromTup (x,y) = Vec (realToFrac x) (realToFrac y)

data Window
data World

nearZero :: Double -> Bool
nearZero x = x <= 1e-12

-- | Pairs a position with a heading.
-- The `_poseHdg` field should contain a normalized vector.
-- data Pose = Pose { _posePos :: !Vec, _poseHdg :: !Vec }
--   deriving (Generic, Show, Eq, Ord)

upVec, downVec, leftVec, rightVec :: Vec w
upVec    = Vec   0    1
downVec  = Vec   0  (-1)
leftVec  = Vec (-1)   0
rightVec = Vec   1    0

mapVec :: (Double -> Double) -> Vec w -> Vec w
mapVec f (Vec x y) = Vec (f x) (f y)

zipWithVec :: (Double -> Double -> Double) -> Vec w -> Vec w -> Vec w
zipWithVec f (Vec x1 y1) (Vec x2 y2) = Vec (f x1 x2) (f y1 y2)


-- Rotate a vector CLOCKWISE, given an angle in radians.
rotVec :: Angle -> Vec w -> Vec w
rotVec (Radians theta) (Vec x y) = Vec
  (x * cos theta' - y * sin theta')
  (x * sin theta' + y * cos theta')
  where theta' = -theta

-- tan theta = y / x
-- atan (y/x) = theta
vecAngle :: Vec w -> Angle
vecAngle (Vec x y) = Radians (pi/2 - atan2 y x)

lerp :: VectorSpace v a => v -> v -> a -> v
lerp v1 v2 = let delta = v2 ^-^ v1
             in \t -> v1 ^+^ t *^ delta

internalAngle :: Vec w -> Vec w -> Angle
internalAngle v w = Radians $ acos $ (v `dot` w) / (norm v * norm w)

signedInternalAngle :: Vec w -> Vec w -> Angle
signedInternalAngle v w = vecAngle w - vecAngle v

-- | Computes a unit vector from an angle respective to the y axis, clockwise.
unitvecFromAngle :: Angle -> Vec w
unitvecFromAngle theta = Vec (rcos alpha) (rsin alpha)
  where alpha = pi/2 - theta

roundVec :: Vec w -> Vec w
roundVec = mapVec (fromIntegral . (round :: Double -> Int))

scalarProjectionOnto :: Vec w -> Vec w -> Double
scalarProjectionOnto b a = (a `dot` b) / norm a

-- TODO recomputes norm
projectOnto :: Vec w -> Vec w -> Vec w
projectOnto b a = (b `scalarProjectionOnto` a) *^ normalize a

-- clampedIntegral :: (Vec, Vec) -> Vec -> SF Vec Vec
-- clampedIntegral bounds init = iterFrom f init
--   where
--     f _ lastIn dt lastOut = clampVec bounds (lastOut ^+^ realToFrac dt *^ lastIn)

-- Given a bottom left and a top right, takes the integral of a vector sf
-- clamped between the bottom left and top right
-- clampedIntegral :: Vec -> Vec -> Vec -> SF Vec Vec
-- clampedIntegral bl tr init = (arr id &&& arr (clamp bl tr)) >>> resettableIntegral init

clamp :: Ord a => (a, a) -> a -> a
clamp (low,high) x | x <= low  = low
                   | x >= high = high
                   | otherwise = x

clampVec :: (Vec w, Vec w) -> Vec w -> Vec w
clampVec (Vec l b,Vec r t) (Vec x y) = Vec (clamp (l,r) x) (clamp (b,t) y)

-- unused and untested
-- clampEvent :: Vec -> Vec -> Vec -> Event Vec
-- clampEvent (V2 xl yl) (V2 xh yh) (V2 x y) =
--   let xs = x : maybeToList ((guard (x < xl) >> Just xl) <|> (guard (x > xh) >> Just xh))
--       ys = y : maybeToList ((guard (y < yl) >> Just yl) <|> (guard (y > yh) >> Just yh))
--   in maybeToEvent $ guard (length xs + length ys > 2) >> return (V2 (last xs) (last ys))

data ViewPort = ViewPort { _vpPos  :: Vec World
                         , _vpRot  :: Angle
                         , _vpZoom :: Double
                         }

defaultViewPort :: ViewPort
defaultViewPort = ViewPort zeroVec 0 1

updateViewPort :: Vec Window -> Angle -> Double -> ViewPort -> ViewPort
updateViewPort (Vec dx dy) drot dzoom (ViewPort pos rot zoom)
  = ViewPort (pos ^+^ Vec dx dy) (rot + drot) (zoom + dzoom)

windowCoordsToWorldCoords :: ViewPort -> Vec Window -> Vec World
windowCoordsToWorldCoords (ViewPort pos rot zoom) (Vec wx wy) =
  pos ^+^ ((1/zoom) *^ rotVec rot (Vec wx wy))

-- | Check whether a position is inside the rectangle defined by
-- the two other vectors. TODO kind of a bad name
between :: Vec w -> (Vec w, Vec w) -> Bool
between (Vec x y) (Vec l b,Vec r t) = x >= l && x <= r && y >= b && y <= t

-- | Measure the distance between two points in space.
(<->) :: Vec w -> Vec w -> Double
u <-> v = norm (v ^-^ u)

perp :: Vec w -> Vec w
perp (Vec x y) = Vec (-y) x

mirrorOn :: Vec w -> Vec w -> Vec w
u `mirrorOn` v =  v ^+^ (v ^-^ u)

ignoreCoordinateSystem :: Vec u -> Vec w
ignoreCoordinateSystem (Vec x y) = Vec x y

unzipVecs :: Fractional a => [Vec w] -> ([a] , [a])
unzipVecs = unzip . map toTup

boundingBox :: [Vec w] -> (Vec w , Vec w)
boundingBox (v:vs) = foldl'
  (\(Vec xl yl,Vec xh yh) (Vec x y)
   -> (Vec (min xl x) (min yl y) , Vec (max xh x) (max yh y)))
  (v,v)
  vs

snapAwayFrom :: Vec w -> Vec w -> Vec w
snapAwayFrom (Vec x y) (Vec x' y') = Vec newX newY
  where
    newX | x < x'    = realToFrac $ floor x
         | x > x'    = realToFrac $ ceiling x
         | otherwise = realToFrac $ round x
    newY | y < y'    = realToFrac $ floor y
         | y > y'    = realToFrac $ ceiling y
         | otherwise = realToFrac $ round y

fromPolar :: Double -> Angle -> Vec w
fromPolar magnitude theta = magnitude *^ unitvecFromAngle theta
