module RenderTrack where

---------------------------------------------------------------------------------
import Vec
import Track
import Angle hiding (radToDeg)

import Graphics.Gloss
import qualified Graphics.Gloss as G
import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg)
import Data.Maybe (isJust, maybeToList)
import Control.Lens (over, _1)
---------------------------------------------------------------------------------

-- renderTrack' :: Vec -> Track -> Picture
-- renderTrack' pt =
--   pictures
--   . ((translate (-100) (-100) . scale 0.2 0.2 . color white . text . show . roundVec) pt:)
--   . map (\(TrackSegment pg) ->
--            let pt' = closestPointOnConvexPolygon pt pg
--            in pictures $ [ color col $ polygon $ map (applyVec (,)) pg
--                          , color cyan $ applyVec translate pt' $ circle 5
--                          , (translate (-100) (-170) . scale 0.2 0.2 . color white . text . show . roundVec) pt'
--                          ])
--                        -- ++ (maybeToList $ fmap
--                        --                   (\p -> color cyan $ applyVec translate p $ circle 5)
--                        --                   pt'))
--   where 
--         col  = blue

renderTrack :: Track -> Picture
renderTrack =
  color (dim $ dim azure)
  . pictures
  . map (\(TrackSegment vs) -> polygon $ map toTup vs)

-- Rotate picture clockwise, given an angle in radians.
rotatePic :: Float -> Picture -> Picture
rotatePic = G.rotate . radToDeg

applyViewPort :: ViewPort -> Picture -> Picture
applyViewPort (ViewPort (Vec x y) (Radians rot) zoom)
  = scale (realToFrac zoom) (realToFrac zoom)
  . rotate (realToFrac (- rot))
  . translate (realToFrac (-x)) (realToFrac (-y))
