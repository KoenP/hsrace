module Editor.Render where

--------------------------------------------------------------------------------
import Util
import Vec
import Editor.TrackState
import Editor.LayoutState
import Editor.Overlay
import RenderTrack
import Track

import Graphics.Gloss

import Control.Lens
--------------------------------------------------------------------------------

renderEditor :: OverlayState -> LayoutState -> Picture
renderEditor
  OverlayState { _os_pointerPos = pointerPos
               , _os_viewPort   = viewPort
               }
  LayoutState  { _ls_trackState = trackState
               , _ls_mode       = mode
               } =
  let
    renderedTrack = renderEditorTrack viewPort pointerPos trackState mode
    Vec ptrX ptrY = pointerPos
    pointer = translate (realToFrac ptrX) (realToFrac ptrY) $ color white $ plusPicture 4
  in
    pictures [applyViewPort viewPort renderedTrack, pointer]

renderEditorTrack :: ViewPort -> Vec Window -> TrackState -> EditingMode -> Picture
renderEditorTrack viewPort pointerPos trackState PlacingTrack { _em_pt_curWidth = curWidth } = 
  let
    virtualWaypoint = windowCoordsToWorldCoords viewPort pointerPos
    TS segmentsR (leftCornersR, rightCornersR) _ _
      = addWaypoint trackState virtualWaypoint curWidth
    lastRealCorners | Just l0 <- revLast leftCornersR , Just r0 <- revLast rightCornersR
                    = (l0 , r0)
                    | otherwise
                    = (Vec (-100) 0 , Vec 100 0) -- TODO
    heading = vecAngle $ virtualWaypoint ^-^ fst (unsafeRevLast $ view ts_waypointsR trackState)
    vsegment = virtualSegment lastRealCorners virtualWaypoint curWidth heading
    cornerCircles = pictures
                    $  map (renderPoint red) (revKeepReversed leftCornersR)
                    ++ map (renderPoint green) (revKeepReversed rightCornersR)
  in
    pictures [ renderLayout $ Layout (vsegment : revKeepReversed segmentsR) (view ts_pillars trackState)
             , cornerCircles
             ]
renderEditorTrack
  viewPort
  mousePos
  trackState
  PlacingPillar { _em_pt_pillarRadius = rad } =
  let
    virtualWaypoint = windowCoordsToWorldCoords viewPort mousePos
    TS segmentsR _ _ pillars
      = addWaypoint trackState virtualWaypoint 0
  in 
    renderLayout
    $ Layout (revKeepReversed segmentsR) ((windowCoordsToWorldCoords viewPort mousePos , rad) : pillars)
             -- , translateVec (windowCoordsToWorldCoords viewPort mousePos) $ color white $ circle (realToFrac rad)

plusPicture :: Float -> Picture
plusPicture size = scale size size $ pictures [line [(-1,0),(1,0)] , line [(0,-1),(0,1)]]
