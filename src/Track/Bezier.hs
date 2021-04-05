module Track.Bezier where

--------------------------------------------------------------------------------
import Vec
import SF
import Util
import Input
import Types
import Track.Render
import Editor.GUI

import Graphics.Gloss
--------------------------------------------------------------------------------

type EndPoints w = (Vec w, Vec w)
type ControlPoints w = (Vec w, Vec w)
data CubicBezier w = CubicBezier (EndPoints w) (ControlPoints w)

bezierTest :: Picture
bezierTest = renderBezier 50 $ CubicBezier (zeroVec , Vec 300 0) (Vec 100 100 , Vec 200 (-100))
  
draggable :: Double -> Vec World -> ((Vec World, Bool) ~> (Vec World, Bool))
draggable radius pos0 = runMode (notSelectedMode pos0)
  where
    notSelectedMode pos0 = Mode $ proc (cursorWorldPos, selecting) -> do
      let
        offset  = pos0 ^-^ cursorWorldPos
        inRange = norm offset <= radius
      event <- sampleOnRisingEdge -< (selecting && inRange, selectedMode offset)
      returnA -< (event, (pos0, inRange))

    selectedMode offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let pos = cursorWorldPos ^+^ offset
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode pos)
      returnA -< (event, (pos, True))


bezierEdit :: Input ~> Output
bezierEdit = proc input -> do
  GUI _ cursorWorldPos _ overlay <- gui -< input
  -- cursorPos <- cumsum -< cursorMovement
  let select = keyDown EditorCommit input
  (e1, _) <- draggable 30 zeroVec          -< (cursorWorldPos, select)
  (e2, _) <- draggable 30 (Vec 300 0)      -< (cursorWorldPos, select)
  (c1, _) <- draggable 30 (Vec 100 100)    -< (cursorWorldPos, select)
  (c2, _) <- draggable 30 (Vec 200 (-100)) -< (cursorWorldPos, select)
  let bezier = CubicBezier (e1, e2) (c1, c2)
  returnA -< Output (overlay $ renderBezier 50 bezier) Nothing
  
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
