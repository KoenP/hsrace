module VectorSpace where

--------------------------------------------------------------------------------
import qualified Linear as L
--------------------------------------------------------------------------------

-- TODO credit yampa


class (Eq a, Floating a) => VectorSpace v a | v -> a where
  zeroV :: v
  (^+^) :: v -> v -> v
  (*^) :: a -> v -> v
  (^/) :: v -> a -> v
  neg :: v -> v
  dot :: v -> v -> a
  norm :: v -> a
  normalize :: v -> v

  (^-^) :: v -> v -> v
  v ^-^ u = v ^+^ neg u

(^*) :: VectorSpace v a => v -> a -> v
(^*) = flip (*^)

instance VectorSpace Float Float where
  zeroV       = 0
  (^+^)       = (+)
  (*^)        = (*)
  (^/)        = (/)
  neg x       = (-x)
  dot         = (*)
  norm        = id
  normalize _ = fromInteger 1

instance VectorSpace Double Double where
  zeroV       = 0
  (^+^)       = (+)
  (*^)        = (*)
  (^/)        = (/)
  neg x       = (-x)
  dot         = (*)
  norm        = id
  normalize _ = fromInteger 1

-- instance (Eq a, Floating a, Num a, L.Epsilon a) => VectorSpace (L.V2 a) a where
--   x *^ v      = fmap (*x) v
--   v ^/ x      = fmap (/x) v
--   dot         = L.dot
--   norm        = L.norm
--   normalize   = L.normalize
