module Editor.Waypoint where

--------------------------------------------------------------------------------
import Vec
import Util
import Track

import Graphics.Gloss

import Data.Ratio
--------------------------------------------------------------------------------

gridCellSize :: Double
gridCellSize = 50

newtype WaypointID = WaypointID (Ratio Int)
  deriving (Show, Eq, Ord)

nextWaypointID :: WaypointID -> WaypointID
nextWaypointID (WaypointID n) = WaypointID (n + 1)

betweenWaypointIDs :: WaypointID -> WaypointID -> WaypointID
betweenWaypointIDs (WaypointID a) (WaypointID b) = WaypointID ((a + b) / 2)

data WaypointsAction
  = NoWaypointAction | DragWaypoint | PlaceNewWaypoint | DeleteWaypoint
  deriving (Eq, Ord)

defaultWaypointWidth :: Double
defaultWaypointWidth = 250

-- | The rendered size of anchors and control points.
nodeRadius :: Double
nodeRadius = 35

-- | Renders the waypoint. If the cursor is close enough to a node,
--   that node is higlighted by rendering it as a solid rather than as
--   an empty circle.
--   This is a very quick hack, but I'm too lazy to write something nicer (:
renderWaypoint :: Waypoint -> Bool -> (WaypointComponent -> Picture) -> Picture
renderWaypoint wp@(Waypoint _ (offset,_) _) highlightLine fn
  = let positions@[_,cp1,cp2,w1,w2] = wpVecsAbsolute wp

        markerLength = 16
        whiskerDelta = markerLength *^ normalize offset
        widthMarkerAt pos = linePic [pos ^+^ whiskerDelta, pos ^-^ whiskerDelta]
        whiskers = color (if highlightLine then white else greyN 0.6)
          $ pictures
          $ linePic [w1, w2] : if highlightLine
                               then [widthMarkerAt w1, widthMarkerAt w2]
                               else []
    in pictures
       $ color (if highlightLine then white else greyN 0.6) (linePic [cp1,cp2])
       : whiskers
       : [ translatePic pos (fn component)
         | (component, pos) <- wpComponents `zip` positions
         ]


unhighlighted :: WaypointComponent -> Picture
unhighlighted c = case c of
  Anchor             -> color red (circlePic nodeRadius)
  ControlPoint _     -> color green (circlePic nodeRadius)
  Whisker _          -> blank

highlighting :: WaypointComponent -> WaypointComponent -> Picture
highlighting wc wc' | wc == wc' = case wc of
                        Anchor            -> color red (circleSolidPic nodeRadius)
                        ControlPoint _    -> color green (circleSolidPic nodeRadius)
                        Whisker _ -> blank
                    | otherwise = unhighlighted wc'
