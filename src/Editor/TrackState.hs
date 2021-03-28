module Editor.TrackState where

--------------------------------------------------------------------------------
import Vec
import Angle
import Track
import Track.Road

import Control.Lens
--------------------------------------------------------------------------------

-- Just a list that has been tagged as "reversed": the actual order
-- is the other way around.
newtype Rev a = Rev { revKeepReversed :: [a] } deriving (Show, Read, Ord, Eq)

data TrackState = TS
  { _ts_segmentsCache :: Rev RoadSegment -- Contains all but the last two track segments (the last two are generated every frame) [TODO, not how it works right now, but how it should work probably]
  , _ts_trackCorners  :: (Rev (Vec World), Rev (Vec World)) -- track corners in reverse order (TODO check whether they are)
  , _ts_waypointsR    :: Rev Waypoint -- waypoints in reverse order
  , _ts_pillars       :: [(Vec World , Double)]
  }
  deriving (Show)
makeLenses 'TS

extractTrack :: TrackState -> Track
extractTrack TS { _ts_waypointsR = waypointsR , _ts_pillars = pillars }
  = Track (fromWaypoints (revRev waypointsR)) pillars

trackStateFromSaveData :: TrackSaveData -> TrackState
trackStateFromSaveData (TrackSaveData waypoints pillars) =
  let corners = bimap init init $ roadCorners waypoints
      road   = roadFromCorners corners
  in TS (revList road)
        (bimap revList revList corners)
        (revList waypoints)
        pillars


extractSaveData :: TrackState -> TrackSaveData
extractSaveData (TS _ _ waypointsR pillars) = TrackSaveData (revRev waypointsR) pillars

emptyEditorTrackState :: TrackState
emptyEditorTrackState
  = TS revEmpty (revEmpty , revEmpty) (revSingleton (zeroVec, 100)) []

-- | Extend the track state by placing down a new waypoint.
addWaypoint :: TrackState -> Vec World -> Double -> TrackState
addWaypoint
  (TS segmentsR (leftCornersR , rightCornersR) waypointsR pillars)
  newPos
  curWidth
  -- General case for placing down any but the first track segment.
  | [wLast,wLast2] <- revTakeFromEnd 2 waypointsR
  = let
      wNew = (newPos , curWidth)
      newWaypoints = waypointsR `revSnoc` wNew
      [headingBefore , headingAfter] = headings [wLast2,wLast,wNew]
      (newLeftCorner , newRightCorner) = waypointCorners wLast headingBefore headingAfter
      newSegment = RoadSegment [ unsafeRevLast leftCornersR
                               , newLeftCorner
                               , newRightCorner
                               , unsafeRevLast rightCornersR
                               ]
    in
      TS (segmentsR `revSnoc` newSegment)
         (leftCornersR `revSnoc` newLeftCorner , rightCornersR `revSnoc` newRightCorner)
         newWaypoints
         pillars 

  -- Special case for placing down the very first road segment.
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

-- | Add a pillar to the track state.
addPillar :: TrackState -> Pillar -> TrackState
addPillar trackState pillar = over ts_pillars (pillar:) trackState

-- | Compute the "phantom" segment from the last placed track waypoint to the mouse cursor.
virtualSegment :: (Vec World , Vec World) -> Vec World -> Double -> Angle -> RoadSegment
virtualSegment (l0 , r0) newPos width heading = RoadSegment [l0 , l1 , r1 , r0]
  where
    l1 = rotVec heading (Vec (-width) 0) ^+^ newPos
    r1 = rotVec heading (Vec   width  0) ^+^ newPos

--------------------------------------------------------------------------------

revNull :: Rev a -> Bool
revNull (Rev []) = True
revNull _       = False

revEmpty :: Rev a
revEmpty = Rev []

revSingleton :: a -> Rev a
revSingleton a = Rev [a]

revList :: [a] -> Rev a
revList = Rev . reverse

revRev :: Rev a -> [a]
revRev (Rev xs) = reverse xs

revSnoc :: Rev a -> a -> Rev a
revSnoc (Rev xs) x = Rev (x : xs)

revInit' :: Rev a -> Rev a
revInit' (Rev (_:xs)) = Rev xs
revInit' _            = Rev []

revLast :: Rev a -> Maybe a
revLast (Rev (x:_)) = Just x
revLast _           = Nothing

unsafeRevLast :: Rev a -> a
unsafeRevLast (Rev (x:_)) = x

revTakeFromEnd :: Int -> Rev a -> [a]
revTakeFromEnd n (Rev xs) = take n xs
