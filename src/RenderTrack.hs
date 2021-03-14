module RenderTrack where

---------------------------------------------------------------------------------
import Vec
import Track
import Angle hiding (radToDeg)
import Util

import Graphics.Gloss
import qualified Graphics.Gloss as G
import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg)
import Data.Maybe (isJust, maybeToList)
import Control.Lens (over, _1)
---------------------------------------------------------------------------------

renderLayout :: Layout -> Picture
renderLayout (Layout track pillars)
  = pictures (renderTrack track : map renderPillar pillars)
  where
    renderPillar (pos , rad) = translateVec pos $ color white $ circle (realToFrac rad)

renderTrack :: Track -> Picture
renderTrack =
  color (dim $ dim azure)
  . pictures
  . map (\(TrackSegment vs) -> polygon $ map toTup vs)

renderPoint :: Color -> Vec World -> Picture
renderPoint col (Vec x y)
  = color col $ translate (realToFrac x) (realToFrac y) (circle 7)

-- Rotate picture clockwise, given an angle in radians.
rotatePic :: Float -> Picture -> Picture
rotatePic = G.rotate . radToDeg

applyViewPort :: ViewPort -> Picture -> Picture
applyViewPort (ViewPort (Vec x y) (Radians rot) zoom)
  = scale (realToFrac zoom) (realToFrac zoom)
  . rotate (realToFrac (- rot))
  . translate (realToFrac (-x)) (realToFrac (-y))
