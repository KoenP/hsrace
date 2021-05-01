module Editor.Waypoint where

--------------------------------------------------------------------------------
import Vec
import SF
import Util
import Track.Bezier

import Graphics.Gloss

import Prelude hiding ((.))
import Control.Monad
import Data.List
import Data.Function hiding ((.))
--------------------------------------------------------------------------------

gridCellSize :: Double
gridCellSize = 50

-- | Waypoints control the shape of the road.
--   Each waypoint consists of an anchor, which the road passes through,
--   as well as the offsets relative to the anchor (so not absolute
--   positions) of two control points, which determine the curvature of the road.
--   The first control point in a waypoint is the second controller of
--   the road shape before the waypoint, the second control point in a
--   waypoint is the first controller of the road shape after the
--   waypoint.
data Waypoint = Waypoint { anchor :: Vec World, controlPointsOffsets :: ControlPoints World }
  deriving Show

newtype WaypointID = WaypointID Int
  deriving (Show, Eq, Ord)

-- | Return the absolute positions of the anchor and control points
--   in a waypoint.
wpVecsAbsolute :: Waypoint -> [Vec World]
wpVecsAbsolute (Waypoint anchor (cp1,cp2)) = [anchor, cp1 ^+^ anchor, cp2 ^+^ anchor]

-- | Compute the absolute positions of the control points in a waypoint.
controlPointsAbsolute :: Waypoint -> (Vec World, Vec World)
controlPointsAbsolute (Waypoint anchor (e1,e2)) = (anchor ^+^ e1, anchor ^+^ e2)

flipWaypointControlPoints :: Waypoint -> Waypoint
flipWaypointControlPoints (Waypoint anchor (e1,e2)) = Waypoint anchor (e2,e1)

waypointsToBezier :: Waypoint -> Waypoint -> CubicBezier World
waypointsToBezier (Waypoint anchor1 (_, offset1)) (Waypoint anchor2 (offset2, _))
  = CubicBezier (anchor1, anchor2) (anchor1 ^+^ offset1, anchor2 ^+^ offset2)
    
-- | The rendered size of anchors and control points.
nodeRadius :: Double
nodeRadius = 15

-- | Signal function that controls the positions of the nodes of a
--   waypoint in space, based on user mouse inputs.
waypoint :: Waypoint -> ((Vec World, Bool) ~> (Waypoint, Picture))
waypoint seg0 = runMode (notSelectedMode seg0)
  where
    -- This mode runs when none of the nodes of this waypoint are selected
    -- for dragging.
    notSelectedMode  :: Waypoint -> Mode (Vec World, Bool) (Waypoint, Picture)
    notSelectedMode seg@(Waypoint anchor _) =
      Mode $ proc (cursorWorldPos, selecting) -> do
        let
          (e1,e2)   = controlPointsAbsolute seg
          offsets   = [v ^-^ cursorWorldPos | v <- [e1, e2, anchor]]
          inRange   = any ((<= nodeRadius) . norm) offsets

          -- Here we check which nodes the cursor is in range of.
          -- Based on that, we determine what the next mode should be
          -- (if the user starts dragging, see the `event` variable),
          -- and which node should be highlighted.
          (_, nextMode, highlighted)  = minimumBy
              (compare `on` (norm . fst3))
              [ (offset, f offset, id)
              | (offset, f, id) <- zip3 offsets
                                        [ dragControlPointMode False seg
                                        , dragControlPointMode True  (flipWaypointControlPoints seg) 
                                        , dragAnchorMode seg
                                        ]
                                        [1,2,0] -- this is just horrible
              ]

        selectingRisingEdge <- risingEdge -< selecting

        -- When the user starts dragging while the cursor is in range
        -- of one of the nodes, switch to the appropriate dragging state.
        event <- sampleOnRisingEdge -< (selectingRisingEdge && inRange, nextMode)

        returnA -< (event, (seg, render seg (guard inRange >> Just highlighted)))

    -- Dragging the anchor is straightforward: we just move the anchor
    -- and don't change the offsets.
    -- The `offset` variable keeps track of how far from the anchor the
    -- mouse cursor was when the user started dragging.
    dragAnchorMode :: Waypoint -> Vec World -> Mode (Vec World, Bool) (Waypoint, Picture)
    dragAnchorMode seg0 offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let anchor = cursorWorldPos ^+^ offset
      let seg    = seg0 { anchor = anchor }

      -- Return to `notSelectedMode` if the user stops dragging.
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode seg)
      returnA -< (event, (seg, render seg (Just 0)))

    -- Dragging control points is a bit more involved.
    -- We calculate the rotation caused by the movement relative to the anchor.
    -- That rotation is mirrorred for the other control node.
    -- To save us some duplicated code, this function always assumes that
    -- the first control node is the one being dragged. If the `flip` parameter
    -- equals `True`, the caller indicates that they flipped the control
    -- points, and that they want this mode to flip them back.
    dragControlPointMode :: Bool -> Waypoint -> Vec World -> Mode (Vec World, Bool) (Waypoint, Picture)
    dragControlPointMode flip (Waypoint anchor (e1init,e2init)) offset
      = Mode $ proc (cursorWorldPos, selecting) -> do
          let
            e1Absolute = cursorWorldPos ^+^ offset
          d_e1Absolute <- delay (e1init ^+^ anchor) -< e1Absolute
          let
            e1  = e1Absolute  ^-^ anchor
            e1_ = d_e1Absolute ^-^ anchor
            dTheta = e1 `signedInternalAngle` e1_
          theta <- cumsum -< (-dTheta)
          let
            e2  =  theta `rotVec` e2init
    
          let segment | flip      = Waypoint anchor (e2,e1)
                      | otherwise = Waypoint anchor (e1,e2)
          event <- sampleOnRisingEdge -< (not selecting, notSelectedMode segment)
          returnA -< (event, (segment, render segment (Just (1 + boolToInt flip))))

    -- Renders the waypoint. If the cursor is close enough to a node,
    -- that node is higlighted by rendering it as a solid rather than as
    -- an empty circle.
    -- This is a very quick hack, but I'm too lazy to write something nicer (:
    render :: Waypoint -> Maybe Int -> Picture
    render wp Nothing =
      let [anchor,c1,c2] = wpVecsAbsolute wp
      in pictures [ color (greyN 0.6) (linePic [c1,c2])
                  , translatePic anchor (color red (circlePic nodeRadius))
                  , translatePic c1 (color green (circlePic nodeRadius))
                  , translatePic c2 (color green (circlePic nodeRadius))
                  ]
    render wp (Just n) =
      let [anchor,c1,c2] = wpVecsAbsolute wp
      in pictures $ color white (linePic [c1,c2]) : zipWith ($)
         [ \f -> translatePic anchor (color red (f nodeRadius))
         , \f -> translatePic c1 (color green (f nodeRadius))
         , \f -> translatePic c2 (color green (f nodeRadius))
         ]
         (replicate n circlePic ++ [circleSolidPic] ++ repeat circlePic)
