module Game where

--------------------------------------------------------------------------------
import Vec
import Angle
import Track
import Input
import RenderTrack

import Graphics.Gloss

import Control.Lens
import qualified Data.Set as Set
import Data.Set (Set)
--------------------------------------------------------------------------------

data GameState = GameState
  { _gs_playerPos     :: Vec World
  , _gs_playerVel     :: Vec World
  , _gs_playerRot     :: Radians Double
  , _gs_track         :: Track
  , _gs_collisionGrid :: CollisionGrid
  }
makeLenses 'GameState

initialGameState :: Layout -> GameState
initialGameState (Layout track _) = GameState zeroVec zeroVec 0 track grid
  where
    grid     = mkCollisionGrid cellSize (map _tsShape track)
    cellSize = 50

updateWorld :: Double -> Input -> GameState -> GameState
updateWorld
  dt
  (Input keysDown _ (Vec mouseDx _))
  (GameState pos0 vel0 rot0 track grid)
  = GameState pos1 vel1 rot1 track grid
  where
    accelerating = Accelerating `Set.member` keysDown

    speed0   = norm vel0
    moveDir0 = normalize vel0
    drag     = (- (if onRoad grid pos0 then k_drag else k_dragOffroad) * speed0 ** 2) *^ moveDir0

    rot1 = rot0 + Radians (mouseSensitivity * mouseDx)
    pos1 = pos0 ^+^ vel1
    acc  = if accelerating then rotVec rot1 (Vec 0 k_acceleration) else zeroVec
    vel1 = vel0 ^+^ acc ^+^ drag

onRoad :: CollisionGrid -> Vec World -> Bool
onRoad grid v = any (v `pointInConvexPolygon`) (grid `collisionGridLookup` v)
  -- = any (pointInConvexPolygon pos . _tsShape) track
--   = any (polygonPolygonOverlap pg . _tsShape) track
--   where pg = map (^+^ pos) [Vec 10 10 , Vec 10 (-10) , Vec (-10) (-10) , Vec (-10) 10]

renderGameState :: GameState -> Picture
renderGameState (GameState (Vec x y) _ rot track _) =
  let
    transform = rotate (negate $ realToFrac $ _unDegrees $ radToDeg rot)
              . translate (- realToFrac x) (- realToFrac y)

    world = transform $ renderTrack track


    -- (leftCorners, rightCorners) = view ts_trackCorners trackState
    -- cornerCircles = pictures
    --   $ map (renderPoint red) (revKeepReversed leftCorners) ++ map (renderPoint green) (revKeepReversed rightCorners)
    -- wps = transform $ pictures $ map (renderPoint white . fst) (revKeepReversed $ view ts_waypointsR trackState)
    -- cellSize = 50
    -- box (Vec x' y') = translate (realToFrac x') (realToFrac y')
    --   $ color orange $ polygon $ map toTup [zeroVec , Vec cellSize 0 , Vec cellSize cellSize , Vec 0 cellSize]
    -- pts = transform $ pictures $ concatMap (map (box . (cellSize*^) . fromTup . fst) . scanPolygon cellSize zeroVec . view tsShape) track

  in
    pictures [world, playerPic] -- , transform cornerCircles, wps] -- , pts]

isoscelesTrianglePath :: Float -> Float -> Path
isoscelesTrianglePath base height = [ (-base/2, -height/3)
                                    , ( base/2, -height/3)
                                    , ( 0     ,  height*2/3)
                                    ]
playerPic :: Picture
playerPic = (color red . polygon) (isoscelesTrianglePath 14 23)


k_acceleration, k_drag :: Double
k_acceleration = 0.2
k_drag         = 0.001
k_dragOffroad  = 0.1
