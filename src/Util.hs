module Util (module Util, module Debug.Trace) where

--------------------------------------------------------------------------------
import Vec
import Angle

import Graphics.Gloss

import Debug.Trace
--------------------------------------------------------------------------------

traceResult :: Show a => a -> a
traceResult x = traceShow x x

translatePic :: Vec w -> Picture -> Picture
translatePic (Vec x y) = translate (realToFrac x) (realToFrac y)

rotatePic :: Angle -> Picture -> Picture
rotatePic (Radians theta) = rotate (realToFrac $ 180 * theta / pi)

scalePic :: Real a => a -> Picture -> Picture
scalePic a = let a' = realToFrac a in scale a' a'

linePic :: [Vec w] -> Picture
linePic = line . map toTup

arcPic :: Angle -> Angle -> Double -> Picture
arcPic a1 a2 rad = let Degrees d1 = toPicAngle a1
                       Degrees d2 = toPicAngle a2
                   in arc d1 d2 (realToFrac rad)

applyViewPort :: ViewPort -> Picture -> Picture
applyViewPort (ViewPort v rot zoom)
  = scalePic zoom . rotatePic (- rot) . translatePic (neg v)

-- | Repeat the last thing mentioned endlessly.
nag :: [a] -> [a]
nag []      = []
nag [x]    = repeat x
nag (x:xs) = x : nag xs

interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave xs     []     = xs
interleave []     ys     = ys

mergeMaybes :: (Maybe a, Maybe b) -> Maybe (Either a b)
mergeMaybes (Just a , _     ) = Just (Left a)
mergeMaybes (Nothing, Just b) = Just (Right b)
mergeMaybes _                 = Nothing
