module Track.Bezier where

-- Support for Bezier curves to plot roads.

--------------------------------------------------------------------------------
import Vec
import SF

import Data.List
import Prelude hiding (id, (.))
--------------------------------------------------------------------------------

type Anchors w = (Vec w, Vec w)
type ControlPoints w = (Vec w, Vec w)
data CubicBezier w = CubicBezier (Anchors w) (ControlPoints w)

quadraticCurve :: Vec w -> Vec w -> Vec w -> (Double -> Vec w)
quadraticCurve v1 v2 v3 =
  -- v1 ^+^ (2*t)*^(v2^-^ v1) ^+^ (t*t)*^(v3 ^-^ 2*^v2 ^+^ v1)
  let lerp1 = lerp v1 v2
      lerp2 = lerp v2 v3
  in \t -> lerp (lerp1 t) (lerp2 t) t

cubicCurve :: CubicBezier w -> (Double -> Vec w)
cubicCurve (CubicBezier (e1,e2) (c1,c2)) =
  let
    curve1 = quadraticCurve e1 c1 c2
    curve2 = quadraticCurve c1 c2 e2
  in
    \t -> lerp (curve1 t) (curve2 t) t

cubicCurveDerivative :: CubicBezier w -> (Double -> Vec w)
cubicCurveDerivative (CubicBezier (e1,e2) (c1,c2)) t
  =   e1 ^* ((-3)*(1-t)**2)
  ^+^ c1 ^* (9*t*t - 12*t + 3)
  ^+^ c2 ^* (3 * (2 - 3*t) * t)
  ^+^ e2 ^* (3*t*t)

-- | Sample a curve uniformly `nSamples` times.
sampleCurve :: Int -> (Double -> Vec w) -> [Vec w]
sampleCurve nSamples curve =
  let
    nSamples' = fromIntegral nSamples
    samplePoints = map (/nSamples') [0..nSamples']
  in
    map curve samplePoints

-- | Approximate the length of a curve (on the domain 0 to 1) by
--   summing up the distances between samples.
--   This process is repeated with an increasing number of samples
--   until the difference in accuracy is less than `epsilon`.
--   We then return both the number of samples used and the measurement.
iterativelyApproximateCurveLength :: Double -> (Double -> Vec w) -> (Int, Double)
iterativelyApproximateCurveLength epsilon curve =
  let
    approximations = [(nSamples, approximateCurveLength nSamples curve) | nSamples <- iterate (*2) 16]
    pairs = approximations `zip` tail approximations
    test ((_,apx1), (_,apx2)) = apx2 - apx1 <= epsilon * apx2
  in
    fst $ head $ dropWhile (not . test) pairs

-- | Approximate the length of a curve (on the domain 0 to 1) by
--   summing up the distances between `nSamples` samples.
approximateCurveLength :: Int -> (Double -> Vec w) -> Double
approximateCurveLength nSamples curve =
  let samplePoints = sampleCurve nSamples curve
  in foldl' (^+^) zeroVec $ zipWith (<->) samplePoints (tail samplePoints)
  

