module State where

--------------------------------------------------------------------------------
import Vec
import Angle
import Track
import Input
import Polar

import Control.Lens
import Data.Bifunctor
--------------------------------------------------------------------------------

-- Just a list that has been tagged as "reversed": the actual order
-- is the other way around.
newtype Rev a = Rev { revKeepReversed :: [a] } deriving (Show, Read, Ord, Eq)

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

--------------------------------------------------------------------------------
  
data TrackState = TS
  { _ts_segmentsCache  :: Rev TrackSegment -- Contains all but the last two track segments (the last two are generated every frame) [TODO, not how it works right now, but how it should work probably]
  , _ts_trackCorners :: (Rev (Vec World), Rev (Vec World)) -- track corners in reverse order (TODO check whether they are)
  , _ts_waypointsR   :: Rev Waypoint -- waypoints in reverse order
  }
  deriving (Show)
makeLenses 'TS

trackStateFromWaypoints :: [Waypoint] -> TrackState
trackStateFromWaypoints waypoints =
  let corners = trackCorners waypoints
      track   = trackFromCorners corners
  in TS (revInit' $ revList track) (bimap revList revList corners) (revList waypoints)

emptyEditorTrackState :: TrackState
emptyEditorTrackState = TS revEmpty (revEmpty , revEmpty) (revSingleton (zeroVec, 100))

--------------------------------------------------------------------------------

data GameState = GameState
  { _gs_playerPos  :: Vec World
  , _gs_playerVel  :: Vec World
  , _gs_playerRot  :: Radians Double
  , _gs_trackState :: TrackState
  , _gs_track      :: Track
  }
makeLenses 'GameState

initialGameState :: TrackState -> GameState
initialGameState ts = GameState zeroVec zeroVec 0 ts (fromWaypoints' $ revRev $ view ts_waypointsR ts)

--------------------------------------------------------------------------------

data EditorState = EditorState
  { _es_viewPort   :: ViewPort
  , _es_trackState :: TrackState
  , _es_pointerPos :: Vec Window
  , _es_curWidth   :: Double
  , _es_saveToFile :: Bool
  }
makeLenses 'EditorState

emptyEditorState :: EditorState
emptyEditorState
  = EditorState (ViewPort zeroVec 0 1) emptyEditorTrackState zeroVec 100 False

--------------------------------------------------------------------------------

type ProgramState = Either GameState EditorState
