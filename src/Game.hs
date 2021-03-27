module Game where

--------------------------------------------------------------------------------
import Vec
import Angle
import Track
import Input
import Track.Render
import Track.CollisionGrid
import Track.Polygon
import Track.Road
import SF
import Types
import Util

import Graphics.Gloss

import Prelude hiding ((.), id)
import Control.Lens
import qualified Data.Set as Set
import Data.Set (Set)
--------------------------------------------------------------------------------

k_acceleration, k_drag :: Double
k_acceleration = 0.2
k_drag         = 0.001
k_dragOffroad  = 0.1

onRoad :: CollisionGrid Polygon -> Vec World -> Bool
onRoad grid v = any (v `pointInPolygon`) (grid `collisionGridLookup` v)

isoscelesTrianglePath :: Float -> Float -> Path
isoscelesTrianglePath base height = [ (-base/2, -height/3)
                                    , ( base/2, -height/3)
                                    , ( 0     ,  height*2/3)
                                    ]

playerPic :: Picture
playerPic = (color red . polygon) (isoscelesTrianglePath 14 23)

renderGameState :: Vec World -> Angle -> Road -> Picture
renderGameState pos rot track =
  let
    transform = rotatePic (-rot) . translatePic (neg pos)
    world = transform $ renderRoad track
  in
    pictures [world, playerPic]

game :: Game
game switchTo (Track track pillars) =
  let
    grid = mkCollisionGrid 50 (map _tsShape track)
  in
    Mode $ proc input -> do
    changeMode_ <- changeMode switchTo -< input
  
    let accelerating = keyDown Accelerating input
    let Input { _input_mouseMovement = (Vec mouseDx _)} = input
    rotation <- cumsum -< Radians (mouseDx * mouseSensitivity)

    let acceleration | accelerating = rotVec rotation (Vec 0 k_acceleration)
                     | otherwise    = zeroVec
    rec
      dPosition <- delay zeroVec -< position
      dVelocity <- delay zeroVec -< velocity
      let dSpeed = norm dVelocity
      let dragFactor = if onRoad grid dPosition then k_drag else k_dragOffroad
      let drag = (- dragFactor * dSpeed ** 2) *^ normalize dVelocity
      velocity <- cumsum -< acceleration ^+^ drag
      position <- cumsum -< velocity
    
    returnA -< (changeMode_, Output (renderGameState position rotation track) Nothing)
