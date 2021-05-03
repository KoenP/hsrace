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
data Waypoint = Waypoint
  { anchor :: Vec World
  , controlPointsOffsets :: ControlPoints World
  , width :: Double
  }
  deriving (Show, Read)

-- TODO: kinda bad naming
data WaypointComponent = Anchor | ControlPoint Int | Whisker Int
  deriving Eq

newtype WaypointID = WaypointID Int
  deriving (Show, Eq, Ord)

defaultWaypointWidth :: Double
defaultWaypointWidth = 250

-- | Return the absolute positions of the anchor and control points
--   in a waypoint.
wpVecsAbsolute :: Waypoint -> [Vec World]
wpVecsAbsolute wp@(Waypoint anchor (cp1,cp2) _ )
  = [anchor, cp1 ^+^ anchor, cp2 ^+^ anchor, wc1, wc2]
  where
    (wc1, wc2) = widthControllerPositions wp

wpComponents :: [WaypointComponent]
wpComponents = [Anchor, ControlPoint 1, ControlPoint 2, Whisker 1, Whisker 2]

widthControllerPositions :: Waypoint -> (Vec World, Vec World)
widthControllerPositions (Waypoint anchor (cpoffset,_) width) = 
  let wcoffset = width *^ perp (normalize cpoffset)
  in (anchor ^-^ wcoffset, anchor ^+^ wcoffset)
    

-- | Compute the absolute positions of the control points in a waypoint.
controlPointsAbsolute :: Waypoint -> (Vec World, Vec World)
controlPointsAbsolute (Waypoint anchor (e1,e2) _ ) = (anchor ^+^ e1, anchor ^+^ e2)

flipWaypointControlPoints :: Waypoint -> Waypoint
flipWaypointControlPoints (Waypoint anchor (e1,e2) width)
  = Waypoint anchor (e2,e1) width

waypointsToBezier :: Waypoint -> Waypoint -> CubicBezier World
waypointsToBezier (Waypoint anchor1 (_, offset1) _) (Waypoint anchor2 (offset2, _) _)
  = CubicBezier (anchor1, anchor2) (anchor1 ^+^ offset1, anchor2 ^+^ offset2)

-- | The rendered size of anchors and control points.
nodeRadius :: Double
nodeRadius = 35

-- | Signal function that controls the positions of the nodes of a
--   waypoint in space, based on user mouse inputs.
waypoint :: Waypoint -> ((Vec World, Bool) ~> (Waypoint, Picture))
waypoint = runMode . notSelectedMode
  where
    -- This mode runs when none of the nodes of this waypoint are selected
    -- for dragging.
    notSelectedMode  :: Waypoint -> Mode (Vec World, Bool) (Waypoint, Picture)
    notSelectedMode wp@(Waypoint anchor _ _) =
      Mode $ proc (cursorWorldPos, selecting) -> do
        let
          -- (e1,e2)   = controlPointsAbsolute wp
          vecs    = wpVecsAbsolute wp
          offsets = map (^-^ cursorWorldPos) vecs
          inRange = any ((<= nodeRadius) . norm) offsets

          -- Here we check which nodes the cursor is in range of.
          -- Based on that, we determine what the next mode should be
          -- (if the user starts dragging, see the `event` variable),
          -- and which node should be highlighted.
          (_, nextMode, nodeRenderFn)  = minimumBy
              (compare `on` (norm . fst3))
              [ (offset, f offset, id)
              | (offset, f, id) <- zip3 offsets
                  [ dragAnchorMode wp
                  , dragControlPointMode False wp
                  , dragControlPointMode True  (flipWaypointControlPoints wp) 
                  , dragWhisker 1 wp
                  , dragWhisker 2 wp
                  ]
                  [ highlighting Anchor
                  , highlighting (ControlPoint 1)
                  , highlighting (ControlPoint 2)
                  , highlighting (Whisker 1)
                  , highlighting (Whisker 2)
                  ]
              ]

        selectingRisingEdge <- risingEdge -< selecting

        -- When the user starts dragging while the cursor is in range
        -- of one of the nodes, switch to the appropriate dragging state.
        event <- sampleOnRisingEdge -< (selectingRisingEdge && inRange, nextMode)

        let pic = renderWaypoint wp inRange $ if inRange
              then nodeRenderFn
              else unhighlighted

        returnA -< ( event , (wp, pic) )

    -- Dragging the anchor is straightforward: we just move the anchor
    -- and don't change the offsets.
    -- The `offset` variable keeps track of how far from the anchor the
    -- mouse cursor was when the user started dragging.
    dragAnchorMode :: Waypoint -> Vec World -> Mode (Vec World, Bool) (Waypoint, Picture)
    dragAnchorMode wp0 offset = release $ proc cursorWorldPos -> do
      let anchor = cursorWorldPos ^+^ offset
      let wp    = wp0 { anchor = anchor }

      -- Return to `notSelectedMode` if the user stops dragging.
      returnA -< (wp, renderWaypoint wp True (highlighting Anchor))

    -- Dragging control points is a bit more involved.
    -- We calculate the rotation caused by the movement relative to the anchor.
    -- That rotation is mirrorred for the other control node.
    -- To save us some duplicated code, this function always assumes that
    -- the first control node is the one being dragged. If the `flip` parameter
    -- equals `True`, the caller indicates that they flipped the control
    -- points, and that they want this mode to flip them back.
    dragControlPointMode :: Bool -> Waypoint -> Vec World
                         -> Mode (Vec World, Bool) (Waypoint, Picture)
    dragControlPointMode flip (Waypoint anchor (e1init,e2init) width) offset
      = release $ proc cursorWorldPos -> do
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
    
          let wp | flip      = Waypoint anchor (e2,e1) width
                 | otherwise = Waypoint anchor (e1,e2) width

          let pic | flip      = renderWaypoint wp True (highlighting (ControlPoint 2))
                  | otherwise = renderWaypoint wp True (highlighting (ControlPoint 1))
          returnA -< (wp, pic)

    release :: (Vec World ~> (Waypoint, Picture))
            -> Mode (Vec World, Bool) (Waypoint, Picture)
    release sf = Mode $ proc (cursor, dragging) -> do
      (waypoint, picture) <- sf -< cursor
      let event = sample (not dragging) (notSelectedMode waypoint)
      returnA -< (event, (waypoint, picture))

    dragWhisker :: Int -> Waypoint -> Vec World
                -> Mode (Vec World, Bool) (Waypoint, Picture)
    dragWhisker whiskerID (Waypoint anchor (offset1,offset2) _) cursorOffset =
      let rail          = normalize (perp offset1)
          fWidth cursor
            = abs ((cursor ^+^ cursorOffset ^-^ anchor) `scalarProjectionOnto` rail)
          render wp     = renderWaypoint wp True (highlighting (Whisker whiskerID))
          output cursor = let wp = Waypoint anchor (offset1,offset2) (fWidth cursor)
                          in (wp , render wp)
          
      in release (arr output)


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
       : [ translatePic pos (fn component)
         | (component, pos) <- wpComponents `zip` positions
         ]
       ++ [whiskers]


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
  
-- renderWaypoint :: Waypoint -> Maybe Int -> Picture
-- renderWaypoint wp Nothing =
--   let [anchor,c1,c2] = wpVecsAbsolute wp
--   in pictures [ color (greyN 0.6) (linePic [c1,c2])
--               , translatePic anchor (color red (circlePic nodeRadius))
--               , translatePic c1 (color green (circlePic nodeRadius))
--               , translatePic c2 (color green (circlePic nodeRadius))
--               ]
-- renderWaypoint wp (Just n) =
--   let [anchor,c1,c2] = wpVecsAbsolute wp
--   in pictures $ color white (linePic [c1,c2]) : zipWith ($)
--      [ \f -> translatePic anchor (color red (f nodeRadius))
--      , \f -> translatePic c1 (color green (f nodeRadius))
--      , \f -> translatePic c2 (color green (f nodeRadius))
--      ]
--      (replicate n circlePic ++ [circleSolidPic] ++ repeat circlePic)
