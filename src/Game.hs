module Game where

--------------------------------------------------------------------------------
import Vec
import Angle
import Track
import Input
import State

import Graphics.Gloss

import Control.Lens
import qualified Data.Set as Set
import Data.Set (Set)
--------------------------------------------------------------------------------


updateWorld :: Double -> Input -> GameState -> ProgramState
updateWorld dt (Input _ keysTriggered _) _ | Mode `Set.member` keysTriggered
  = Right emptyEditorState
updateWorld dt (Input keysDown _ (Vec mouseDx _)) (GameState pos0 vel0 rot0)
  = Left (GameState pos1 vel1 rot1)
  where
    accelerating = Accelerating `Set.member` keysDown
    
    speed0 = norm vel0
    moveDir0 = normalize vel0
    drag = (- k_drag * speed0 ** 2) *^ moveDir0

    rot1 = rot0 + Radians (mouseSensitivity * mouseDx)
    pos1 = pos0 ^+^ vel1
    acc  = if accelerating then rotVec rot1 (Vec 0 k_acceleration) else zeroVec
    vel1 = vel0 ^+^ acc ^+^ drag

k_acceleration, k_drag :: Double
k_acceleration = 0.2
k_drag         = 0.001
