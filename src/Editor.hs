module Editor where

--------------------------------------------------------------------------------
import Vec
import Angle
import Input
import Track
import RenderTrack
import State
import Polar

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
updateEditor _ (Input _ keysTriggered _) state
  | Mode `Set.member` keysTriggered
  = Left $ initialGameState (view es_trackState state)
updateEditor
  _
  input
  (EditorState viewPort0 trackState0 pointerPos0 curWidth0 _)
  = Right (EditorState viewPort1 trackState1 pointerPos1 curWidth1 writeToFile)
  where
    adjustingWidth = keyDown EditorAdjustWidth input

    -- Viewport
    mouseMovement = _input_mouseMovement input
    dv            = 10 *^ direction input
    drot          = 0
    dzoom         = 0
    viewPort1     = updateViewPort dv drot dzoom viewPort0
    pointerPos1   | not adjustingWidth = pointerPos0 ^+^ (0.1 *^ ignoreCoordinateSystem mouseMovement)
                  | otherwise = pointerPos0

    curWidth1 | adjustingWidth = let (Vec _ y) = mouseMovement in curWidth0 + y
              | otherwise      = curWidth0

    trackState1
      | keyTriggered EditorPlaceTrack input
      = updateTrackState trackState0 (windowCoordsToWorldCoords viewPort1 pointerPos1) curWidth1
      | otherwise
      = trackState0

    writeToFile = keyTriggered EditorSave input

-- | Extend the track state by placing down a new waypoint.
updateTrackState :: TrackState -> Vec World -> Double -> TrackState
updateTrackState (TS segmentsR (leftCornersR , rightCornersR) waypointsR) newPos curWidth
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

  -- Special case for placing down the very first track segment.
  | Just (waypoint , width) <- revLast waypointsR
  = let
      headingBefore  = 0
      headingAfter   = vecAngle (newPos ^-^ waypoint)
      (left , right) = waypointCorners (waypoint,width) headingBefore headingAfter
      ts             = TS revEmpty
                          (leftCornersR `revSnoc` left , rightCornersR `revSnoc` right)
                          (waypointsR `revSnoc` (newPos,curWidth))
    in
      ts
  | otherwise
  = error "updateTrackState expects at least one waypoint"

-- | Compute the "phantom" segment from the last placed track waypoint to the mouse cursor.
virtualSegment :: (Vec World , Vec World) -> Vec World -> Double -> Angle -> TrackSegment
virtualSegment (l0 , r0) newPos width heading = TrackSegment [l0 , l1 , r1 , r0]
  where
    l1 = rotVec heading (Vec (-width) 0) ^+^ newPos
    r1 = rotVec heading (Vec   width  0) ^+^ newPos

renderEditorState :: EditorState -> Picture
renderEditorState (EditorState viewPort trackState mousePos@(Vec ptrX ptrY) curWidth _) =
  let
    virtualWaypoint = windowCoordsToWorldCoords viewPort mousePos
    TS segmentsR (leftCornersR, rightCornersR) _
      = updateTrackState trackState virtualWaypoint curWidth
    lastRealCorners | Just l0 <- revLast leftCornersR , Just r0 <- revLast rightCornersR
                    = (l0 , r0)
                    | otherwise
                    = (Vec (-100) 0 , Vec 100 0) -- TODO
    heading = vecAngle $ virtualWaypoint ^-^ fst (unsafeRevLast $ view ts_waypointsR trackState)
    vsegment = virtualSegment lastRealCorners virtualWaypoint curWidth heading
    renderedTrack = renderTrack $ vsegment : revKeepReversed segmentsR
    pointer = translate (realToFrac ptrX) (realToFrac ptrY) $ color white $ circle 4
    cornerCircles = pictures
                    $  map (renderPoint red) (revKeepReversed leftCornersR)
                    ++ map (renderPoint green) (revKeepReversed rightCornersR)
  in
    pictures [applyViewPort viewPort renderedTrack, pointer, applyViewPort viewPort cornerCircles]

extractTrackDescription :: [(Polar, Vec World, Radians Double)] -> TrackDescription
extractTrackDescription = TrackDescription . reverse . map (\(p,_,_) -> p)

--------------------------------------------------------------------------------
