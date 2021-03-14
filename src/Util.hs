module Util where

--------------------------------------------------------------------------------
import Vec

import Graphics.Gloss

import Debug.Trace
--------------------------------------------------------------------------------

traceResult :: Show a => a -> a
traceResult x = traceShow x x

translateVec :: Vec w -> Picture -> Picture
translateVec (Vec x y) = translate (realToFrac x) (realToFrac y)

-- | Repeat the last thing mentioned endlessly.
nag :: [a] -> [a]
nag []      = []
nag [x]    = repeat x
nag (x:xs) = x : nag xs
