module Track.Bezier where

--------------------------------------------------------------------------------
import Angle
import Vec
import SF
import Util
import Input
import Types
import Track.Render
import Editor.GUI

import Graphics.Gloss
  
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (id, (.))
import Data.Function (on)
--------------------------------------------------------------------------------

type EndPoints w = (Vec w, Vec w)
type ControlPoints w = (Vec w, Vec w)
data CubicBezier w = CubicBezier (EndPoints w) (ControlPoints w)

type PathConfig = [(Vec World, Vec World)]

addWaypointEvent :: Input -> Vec World -> Maybe (Vec World)
addWaypointEvent input pos | keyDown EditorCommit input = Just pos
                           | otherwise                  = Nothing



bezierTest :: Picture
bezierTest = renderBezier 50 $ CubicBezier (zeroVec , Vec 300 0) (Vec 100 100 , Vec 200 (-100))

-- pathConfig :: Maybe (Vec World) ~> PathConfig
-- pathConfig = proc dragPos -> do
--   
--   returnA -< _
  
draggable :: Double -> Vec World -> ((Vec World, Bool) ~> (Vec World, Bool))
draggable radius pos0 = runMode (notSelectedMode pos0)
  where
    notSelectedMode pos0 = Mode $ proc (cursorWorldPos, selecting) -> do
      let
        offset  = pos0 ^-^ cursorWorldPos
        inRange = norm offset <= radius

      -- not quite correct: draggin will "pick up" points
      event <- sampleOnRisingEdge -< (selecting && inRange, selectedMode offset)
      returnA -< (event, (pos0, inRange))

    selectedMode offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let pos = cursorWorldPos ^+^ offset
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode pos)
      returnA -< (event, (pos, True))

data Segment = Segment { anchor :: Vec World, endpointOffsets :: (Vec World, Vec World) }
  deriving Show

endpointsAbsolute :: Segment -> (Vec World, Vec World)
endpointsAbsolute (Segment anchor (e1,e2)) = (anchor ^+^ e1, anchor ^+^ e2)

flipSegmentEndpoints :: Segment -> Segment
flipSegmentEndpoints (Segment anchor (e1,e2)) = Segment anchor (e2,e1)

-- data SegmentPolar = SegmentPolar { spAnchor :: Vec World, spRotation :: Angle, spDists :: (Double, Double) }
--   deriving Show
-- 
-- segmentRotation :: Segment -> Angle
-- segmentRotation (Segment (p1,_) anchor) = vecAngle (p1 ^-^ anchor)
-- 
-- segmentDists :: Segment -> (Double, Double)
-- segmentDists (Segment (p1,p2) anchor) = (norm (p1 ^-^ anchor), norm (p2 ^-^ anchor))
-- 
-- segmentToPolar :: Segment -> SegmentPolar
-- segmentToPolar seg = SegmentPolar (anchor seg) (segmentRotation seg) (segmentDists seg)
-- 
-- polarToSegment :: SegmentPolar -> Segment
-- polarToSegment (SegmentPolar anchor rotation (d1,d2))
--   = Segment (fromPolar d1 rotation ^+^ anchor, fromPolar (-d2) rotation ^+^ anchor) anchor

draggableSegment :: Double -> Segment -> ((Vec World, Bool) ~> Segment)
draggableSegment radius seg0 = runMode (notSelectedMode seg0)
  where
    notSelectedMode  :: Segment -> Mode (Vec World, Bool) Segment
    notSelectedMode seg@(Segment anchor (p1,p2)) = Mode $ proc (cursorWorldPos, selecting) -> do
      let
        (e1,e2)   = endpointsAbsolute seg
        offsets   = [v ^-^ cursorWorldPos | v <- [e1, e2, anchor]]
        inRange   = any ((<= radius) . norm) offsets
        nextMode  = snd
          $ minimumBy
            (compare `on` (norm . fst))
            [ (offset, f offset)
            | (offset, f) <- offsets `zip` [ dragEndPointMode False seg
                                          , dragEndPointMode True  (flipSegmentEndpoints seg) 
                                          , dragAnchorMode seg
                                          ]
            ]
            -- (zipWith (\offset f -> (offset, f offset)) offsets
            --    [ dragEndPointMode False seg
            --    , dragEndPointMode True  seg 
            --    , dragAnchorMode seg
            --    ])
            -- (offsets `zip` [ 
            --                , dragEndPointMode True  seg (offsets !! 1)
            --                , dragAnchorMode seg (offsets !! 2)
            --                ])

      event <- sampleOnRisingEdge -< (selecting && inRange, nextMode)
      returnA -< (event, seg)

    dragAnchorMode :: Segment -> Vec World -> Mode (Vec World, Bool) Segment
    dragAnchorMode seg0 offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let anchor = cursorWorldPos ^+^ offset
      let seg    = seg0 { anchor = anchor }
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode seg)
      returnA -< (event, seg)

    dragEndPointMode :: Bool -> Segment -> Vec World -> Mode (Vec World, Bool) Segment
    dragEndPointMode flip (Segment anchor (e1init,e2init)) offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let
        e1Absolute = cursorWorldPos ^+^ offset
      e1Absolute_ <- delay (e1init ^+^ anchor) -< e1Absolute
      let
        e1  = e1Absolute  ^-^ anchor
        e1_ = e1Absolute_ ^-^ anchor
        dTheta = e1 `signedInternalAngle` e1_
      theta <- cumsum -< (-dTheta)
      let
        e2  =  theta `rotVec` e2init

      let segment | flip      = Segment anchor (e2,e1)
                  | otherwise = Segment anchor (e1,e2)
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode segment)
      returnA -< (event, segment)

bezierEdit :: Input ~> Output
bezierEdit = proc input -> do
  GUI _ cursorWorldPos _ overlay <- gui -< input
  -- cursorPos <- cumsum -< cursorMovement
  let select = keyDown EditorCommit input

  seg1@(Segment anchor1 _) <- draggableSegment 20 (Segment zeroVec (Vec 0 (-50) , Vec 0 50) )
    -< (cursorWorldPos, select)
  seg2@(Segment anchor2 _) <- draggableSegment 20 (Segment (Vec 100 0) (Vec 0 (-50) , Vec 0 50))
    -< (cursorWorldPos, select)

  -- let bezier = CubicBezier (anchor1, anchor2) () 
  let
    (e1,e2) = endpointsAbsolute seg1
    (f1,f2) = endpointsAbsolute seg2
    anchorPics = map (renderPoint red) [anchor1, anchor2]
    controlPointPics = map (renderPoint green) [e1, e2, f1, f2]

  returnA -< Output (overlay $ pictures (anchorPics ++ controlPointPics)) Nothing
  -- (e1, _) <- draggable 30 zeroVec          -< (cursorWorldPos, select)
  -- (e2, _) <- draggable 30 (Vec 300 0)      -< (cursorWorldPos, select)
  -- (c1, _) <- draggable 30 (Vec 100 100)    -< (cursorWorldPos, select)
  -- (c2, _) <- draggable 30 (Vec 200 (-100)) -< (cursorWorldPos, select)
  -- let bezier = CubicBezier (e1, e2) (c1, c2)
  -- returnA -< Output (overlay $ renderBezier 50 bezier) Nothing
  
quadraticCurve :: Vec w -> Vec w -> Vec w -> (Double -> Vec w)
quadraticCurve v1 v2 v3 = let lerp1 = lerp v1 v2
                              lerp2 = lerp v2 v3
                          in \t -> lerp (lerp1 t) (lerp2 t) t

cubicCurve :: CubicBezier w -> (Double -> Vec w)
cubicCurve (CubicBezier (e1,e2) (c1,c2)) =
  let
    curve1 = quadraticCurve e1 c1 c2
    curve2 = quadraticCurve c1 c2 e2
  in
    \t -> lerp (curve1 t) (curve2 t) t

renderBezier :: Int -> CubicBezier w -> Picture
renderBezier nSamples bezier@(CubicBezier (e1,e2) (c1,c2)) =
  let
    curve         = cubicCurve bezier
    samplePoints  = map (/fromIntegral nSamples) [0..fromIntegral nSamples]
    samples       = map curve samplePoints
    path          = linePic samples
    endPoints     = map (renderPoint red) [e1, e2]
    controlPoints = map (renderPoint blue) [c1, c2]
  in
    pictures (color white path : endPoints ++ controlPoints)
