module Editor.Render where

--------------------------------------------------------------------------------
import Util
import Vec
import Editor.TrackState
import Track.Render
import Track

import Graphics.Gloss

import Control.Lens
--------------------------------------------------------------------------------

-- renderPlacingTrackMode :: Vec World -> TrackState -> Double -> Picture
-- renderPlacingTrackMode virtualWaypoint trackState curWidth = 
--   let
--     TS segmentsR (leftCornersR, rightCornersR) _ _
--       = addWaypoint trackState virtualWaypoint curWidth
--     lastRealCorners | Just l0 <- revLast leftCornersR , Just r0 <- revLast rightCornersR
--                     = (l0 , r0)
--                     | otherwise
--                     = (Vec (-100) 0 , Vec 100 0) -- TODO
--     heading = vecAngle $ virtualWaypoint ^-^ fst (unsafeRevLast $ view ts_waypointsR trackState)
--     vsegment = virtualSegment lastRealCorners virtualWaypoint curWidth heading
--     cornerCircles = pictures
--                     $  map (renderPoint red) (revKeepReversed leftCornersR)
--                     ++ map (renderPoint green) (revKeepReversed rightCornersR)
--   in
--     pictures [ renderTrack $ Track (vsegment : revKeepReversed segmentsR) (view ts_pillars trackState)
--              , cornerCircles
--              ]
-- 
-- renderPlacingPillarMode :: Vec World -> TrackState -> Double -> Picture
-- renderPlacingPillarMode virtualPillarPos trackState rad =
--   let
--     TS segmentsR _ _ pillars
--       = addWaypoint trackState virtualPillarPos 0
--   in 
--     renderTrack
--     $ Track (revKeepReversed segmentsR) ((virtualPillarPos,rad) : pillars)

renderControlNode :: Double -> Color -> Vec World -> Picture
renderControlNode radius col pos
  = color col $ translatePic pos $ circlePic radius
