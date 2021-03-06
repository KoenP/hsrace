module Game where

--------------------------------------------------------------------------------
import Vec
import Angle
import Track
import Input
import State
import RenderTrack

import Graphics.Gloss

import Control.Lens
import qualified Data.Set as Set
import Data.Set (Set)
--------------------------------------------------------------------------------

updateWorld :: Double -> Input -> GameState -> ProgramState
updateWorld dt (Input _ keysTriggered _) _ | Mode `Set.member` keysTriggered
  = Right emptyEditorState
updateWorld
  dt
  (Input keysDown _ (Vec mouseDx _))
  (GameState pos0 vel0 rot0 trackState track)
  = Left (GameState pos1 vel1 rot1 trackState track)
  where
    accelerating = Accelerating `Set.member` keysDown

    speed0   = norm vel0
    moveDir0 = normalize vel0
    drag     = (- k_drag * speed0 ** 2) *^ moveDir0

    rot1 = rot0 + Radians (mouseSensitivity * mouseDx)
    pos1 = pos0 ^+^ vel1
    acc  = if accelerating then rotVec rot1 (Vec 0 k_acceleration) else zeroVec
    vel1 = vel0 ^+^ acc ^+^ drag

renderGameState :: GameState -> Picture
renderGameState (GameState (Vec x y) _ rot trackState track) =
  let
    transform = rotate (negate $ realToFrac $ _unDegrees $ radToDeg rot)
              . translate (- realToFrac x) (- realToFrac y)

    world = transform $ renderTrack (reverse track)


    (leftCorners, rightCorners) = view ts_trackCorners trackState
    cornerCircles = pictures
      $ map (renderPoint red) (revKeepReversed leftCorners) ++ map (renderPoint green) (revKeepReversed rightCorners)

    wps = transform $ pictures $ map (renderPoint white . fst) (revKeepReversed $ view ts_waypointsR trackState)

  in pictures [world, playerPic, transform cornerCircles, wps]

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
