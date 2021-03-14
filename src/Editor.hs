module Editor where

--------------------------------------------------------------------------------
import Vec
import Angle
import Input
import Track
import RenderTrack
import State
import Polar
import Util

import Graphics.Gloss

import Control.Lens
import qualified Data.Set as Set
import Data.Bifunctor
import Debug.Trace
--------------------------------------------------------------------------------

data EditorInput = EditorInput
  { _input_pointer :: Vec Window
  , _input_dir :: Vec World
  }
makeLenses 'EditorInput

updateEditor :: Double -> Input -> EditorState -> ProgramState
updateEditor _ input state
  | keyTriggered Mode input
  = Left $ initialGameState (view es_trackState state)
updateEditor _ input state
  | keyTriggered EditorNextMode input
  = Right (nextEditorMode state)
updateEditor
  _
  input
  state@EditorState { _es_mode = PlacingTrack {} }
  = Right $ updatePlacingTrackMode input state
updateEditor
  _
  input
  state@EditorState { _es_mode = PlacingPillar {} }
  = Right $ updatePlacingPillarMode input state

--   (EditorState viewPort1 trackState1 pointerPos1 writeToFile mode1)
--   where
--     -- Viewport
--     adjusting     = keyDown EditorAdjustWidth input
--     mouseMovement = _input_mouseMovement input
--     viewPort1     = updateEditorViewPort input viewPort0
--     pointerPos1   | not adjusting = pointerPos0 ^+^ (0.1 *^ ignoreCoordinateSystem mouseMovement)
--                   | otherwise     = pointerPos0
--     pointerWorldPos = windowCoordsToWorldCoords viewPort1 pointerPos1
--     writeToFile = keyTriggered EditorSave input

updateCursor :: Vec Window -> Vec Window -> Vec Window    
updateCursor mouseMovement cursorPos0 = cursorPos0 ^+^ (0.1 *^ ignoreCoordinateSystem mouseMovement)
 
updatePlacingTrackMode :: Input -> EditorState -> EditorState
updatePlacingTrackMode
  input@Input { _input_mouseMovement = mouseMovement
              }
  EditorState { _es_trackState       = trackState0
              , _es_mode             = PlacingTrack curWidth0
              , _es_pointerPos       = pointerPos0
              , _es_viewPort         = viewPort0
              } =
  let
    adjustingWidth = keyDown EditorAdjustWidth input
    curWidth1 | adjustingWidth = let (Vec _ y) = mouseMovement in (curWidth0 + y) `max` 0
              | otherwise      = curWidth0
    viewPort1     = updateEditorViewPort input viewPort0
    adjusting     = keyDown EditorAdjustWidth input
    pointerPos1   | not adjusting = updateCursor mouseMovement pointerPos0
                  | otherwise     = pointerPos0
    trackState1
      | keyTriggered EditorCommit input
      = addWaypoint trackState0 (windowCoordsToWorldCoords viewPort1 pointerPos1) curWidth1
      | otherwise
      = trackState0

    saveToFile = keyTriggered EditorSave input
  in
    EditorState viewPort1 trackState1 pointerPos1 saveToFile (PlacingTrack curWidth1)

updatePlacingPillarMode :: Input -> EditorState -> EditorState
updatePlacingPillarMode
  input@Input { _input_mouseMovement = mouseMovement
              }
  EditorState { _es_trackState       = trackState0
              , _es_mode             = PlacingPillar radius0
              , _es_pointerPos       = pointerPos0
              , _es_viewPort         = viewPort0
              } =
  let
    -- TODO get rid of code duplication
    adjustingWidth = keyDown EditorAdjustWidth input
    radius1 | adjustingWidth = let (Vec _ y) = mouseMovement in (radius0 + y) `max` 0
            | otherwise      = radius0
    viewPort1     = updateEditorViewPort input viewPort0
    adjusting     = keyDown EditorAdjustWidth input
    pointerPos1   | not adjusting = updateCursor mouseMovement pointerPos0
                  | otherwise     = pointerPos0
    trackState1
      | keyTriggered EditorCommit input
      = addPillar trackState0 (windowCoordsToWorldCoords viewPort1 pointerPos1 , radius1)
      | otherwise
      = trackState0
    saveToFile = keyTriggered EditorSave input
  in
    EditorState viewPort1 trackState1 pointerPos1 saveToFile (PlacingPillar radius1)


updateEditorViewPort :: Input -> ViewPort -> ViewPort
updateEditorViewPort input = updateViewPort dv drot dzoom 
  where
    dv    = 10 *^ direction input
    drot  = 0
    dzoom = 0

-- | Add a pillar to the track state.
addPillar :: TrackState -> Pillar -> TrackState
addPillar trackState pillar = over ts_pillars (pillar:) trackState

-- | Extend the track state by placing down a new waypoint.
addWaypoint :: TrackState -> Vec World -> Double -> TrackState
addWaypoint (TS segmentsR (leftCornersR , rightCornersR) waypointsR pillars) newPos curWidth
  -- General case for placing down any but the first track segment.
  | [wLast,wLast2] <- revTakeFromEnd 2 waypointsR
  = let
      wNew = (newPos , curWidth)
      newWaypoints = waypointsR `revSnoc` wNew
      [headingBefore , headingAfter] = headings [wLast2,wLast,wNew]
      (newLeftCorner , newRightCorner) = waypointCorners wLast headingBefore headingAfter
      newSegment = TrackSegment [ unsafeRevLast leftCornersR
                                , newLeftCorner
                                , newRightCorner
                                , unsafeRevLast rightCornersR
                                ]
    in
      TS (segmentsR `revSnoc` newSegment)
         (leftCornersR `revSnoc` newLeftCorner , rightCornersR `revSnoc` newRightCorner)
         newWaypoints
         pillars 

  -- Special case for placing down the very first track segment.
  | Just (waypoint , width) <- revLast waypointsR
  = let
      headingBefore  = 0
      headingAfter   = vecAngle (newPos ^-^ waypoint)
      (left , right) = waypointCorners (waypoint,width) headingBefore headingAfter
      ts             = TS revEmpty
                          (leftCornersR `revSnoc` left , rightCornersR `revSnoc` right)
                          (waypointsR `revSnoc` (newPos,curWidth))
                          pillars
    in
      ts
  | otherwise
  = error "addWaypoint expects at least one waypoint"

nextEditorMode :: EditorState -> EditorState
nextEditorMode = over es_mode $ \case
  PlacingTrack {}  -> placingPillarMode
  PlacingPillar {} -> placingTrackMode

-- | Compute the "phantom" segment from the last placed track waypoint to the mouse cursor.
virtualSegment :: (Vec World , Vec World) -> Vec World -> Double -> Angle -> TrackSegment
virtualSegment (l0 , r0) newPos width heading = TrackSegment [l0 , l1 , r1 , r0]
  where
    l1 = rotVec heading (Vec (-width) 0) ^+^ newPos
    r1 = rotVec heading (Vec   width  0) ^+^ newPos

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

renderEditorState :: EditorState -> Picture
renderEditorState EditorState { _es_viewPort   = viewPort
                              , _es_trackState = trackState
                              , _es_pointerPos = pointerPos
                              , _es_mode       = mode
                              } =
  let
    renderedTrack = renderEditorTrack viewPort pointerPos trackState mode
    Vec ptrX ptrY = pointerPos
    pointer = translate (realToFrac ptrX) (realToFrac ptrY) $ color white $ plusPicture 4
  in
    pictures [applyViewPort viewPort renderedTrack, pointer]

plusPicture :: Float -> Picture
plusPicture size = scale size size $ pictures [line [(-1,0),(1,0)] , line [(0,-1),(0,1)]]


-- (EditorState viewPort trackState mousePos@(Vec ptrX ptrY) curWidth _) =
--   let
--     renderedTrack = renderTrack $ vsegment : revKeepReversed segmentsR
--   in

extractTrackDescription :: [(Polar, Vec World, Radians Double)] -> TrackDescription
extractTrackDescription = TrackDescription . reverse . map (\(p,_,_) -> p)

--------------------------------------------------------------------------------
