module Track.Render where

---------------------------------------------------------------------------------
import Vec
import Track
import Track.Road
import Util

import Graphics.Gloss
---------------------------------------------------------------------------------

renderTrack :: Track -> Picture
renderTrack (Track track pillars)
  = pictures (renderRoad track : map renderPillar pillars)
  where
    renderPillar (pos , rad) = translatePic pos $ color white $ circle (realToFrac rad)

renderRoad :: Road -> Picture
renderRoad =
  color (dim $ dim azure)
  . pictures
  . map (\(RoadSegment vs) -> polygon $ map toTup vs)

renderPoint :: Color -> Vec World -> Picture
renderPoint col (Vec x y)
  = color col $ translate (realToFrac x) (realToFrac y) (circle 7)
