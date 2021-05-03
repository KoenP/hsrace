module Game where

--------------------------------------------------------------------------------
import Vec
import Angle
import Input
import Track
import Track.CollisionGrid
import Track.Polygon
import SF
import Types
import Util

import Graphics.Gloss

import Prelude hiding ((.), id)
import Data.Maybe
import Control.Applicative
--------------------------------------------------------------------------------
-- Hooks.
data Hook = NoHook | HookTravelling (Vec World) | HookAttached (Vec World)
type HookAttachedCheck = Vec World -> Maybe (Vec World)
type HookMode = HookAttachedCheck -> Mode (Input, Vec World, Vec World) Hook

k_acceleration, k_drag, k_hookSpeed :: Double
-- k_acceleration = 0.05
-- k_drag         = 0
-- k_acceleration = 0.3
-- k_drag         = 0.0015
k_acceleration = 0.2
k_drag         = 0.0005

k_dragOffroad  = 0.1
k_hookSpeed    = 80

isoscelesTrianglePath :: Float -> Float -> Path
isoscelesTrianglePath base height = [ (-base/2, -height/3)
                                    , ( base/2, -height/3)
                                    , ( 0     ,  height*2/3)
                                    ]

playerPic :: Picture
playerPic = polygon (isoscelesTrianglePath 14 23)

renderGameState :: Double -> Vec World -> Picture -> Picture -> Hook -> Picture
renderGameState avgSpeed pos playerTrace trackPic hook =
  let
    zoom = clamp (0.2,1) $ lerp 1 0.2 (avgSpeed / 27)
    transform = applyViewPort $ ViewPort pos 0 zoom
    world = transform trackPic
    hookPic = color white $ transform $ renderHook pos hook
  in
    pictures [world, hookPic, transform playerTrace]

renderHook :: Vec World -> Hook -> Picture
renderHook startPos (HookTravelling endPos)
  = pictures [linePic [startPos, endPos]
             , translatePic endPos
               $ rotatePic (vecAngle (endPos ^-^ startPos)) hookHead
             ]
renderHook startPos (HookAttached endPos)
  = pictures [linePic [startPos, endPos]
             , translatePic endPos
               $ rotatePic (vecAngle (endPos ^-^ startPos)) hookHead
             ]
renderHook _        _
  = blank

hookHead :: Picture
hookHead = translatePic (Vec 0 7) $ arc (-180) 0 hookRad
  where
    hookRad = 7

noHookMode :: HookMode
noHookMode coll = Mode $ proc (input, startPos, velocity) -> do
  launchHook <- sampleOnRisingEdge -< (keyDown LaunchHook input, hookTravellingMode startPos velocity coll)
  returnA -< (launchHook, NoHook)

hookTravellingMode :: Vec World -> Vec World -> HookMode
hookTravellingMode startPos velocity coll = Mode $ proc (input,_,_) -> do
  pos <- cumsumFrom startPos -< velocity
  cancelHook <- sampleOnRisingEdge -< (not (keyDown LaunchHook input), noHookMode coll)
  let attachHook = (\p -> hookAttachedMode p coll) <$> coll pos
  returnA -< (attachHook <|> cancelHook, HookTravelling pos)

hookAttachedMode :: Vec World -> HookMode
hookAttachedMode pos coll = Mode $ proc (input,_,_) -> do
  cancelHook <- notYet <<< sampleOnRisingEdge -< (not (keyDown LaunchHook input), noHookMode coll)
  returnA -< (cancelHook, HookAttached pos)
  
-- TODO apply a fix to make sure the tether doesn't stretch
spanTether :: Vec World -> Hook -> Vec World -> Maybe (Vec World)
spanTether pos (HookAttached anchor) vel
  | delta <- anchor ^-^ pos
  , theta <- vel `internalAngle` delta
  , theta > (pi/2)
  = Just $ vel `projectOnto` perp delta
spanTether _   _                     _
  = Nothing
  
velocitySF :: ((Vec World, Vec World, Hook) ~> (Vec World, Maybe (Vec World)))
velocitySF = stateful (zeroVec, Nothing) step
  where
    step _ (dPos, acc, hook) (dVel, _) = let vel    = dVel ^+^ acc
                                             tether = spanTether dPos hook vel
                                         in (fromMaybe vel tether, tether)

-- | Main game loop.
game :: Game
game switchTo (GameTrack onRoad pillars trackPic) =
  Mode $ proc input -> do
    -- Switch to editor mode.
    changeMode_ <- changeMode switchTo -< input

    -- Orient the player.
    let cursorPos = _input_cursorPos input
  
    -- Position and heading.
    let accelerating = keyDown Accelerating input
    rec
      let
        rotation = vecAngle cursorPos
        thrust | accelerating = rotVec rotation (Vec 0 k_acceleration)
               | otherwise    = zeroVec

      dPosition <- delay zeroVec -< position
      dVelocity <- delay zeroVec -< velocity
      hook      <- runMode (noHookMode (error "hooks not properly supported now")) -< (input, dPosition, fromPolar k_hookSpeed rotation)
      let dSpeed = norm dVelocity

      let dragFactor = if onRoad dPosition then k_drag else k_dragOffroad
      let drag = (- dragFactor * dSpeed ** 2) *^ normalize dVelocity
      (velocity, tether) <- velocitySF -< (dPosition, thrust ^+^ drag, hook)
      position <- cumsum -< velocity


    thrustAnim <- thrusterAnimation -< (accelerating, rotation, position)
    let playerPicture = playerPic 
    playerTrace <- fadingTrace 1 red
      -< translatePic position $ rotatePic rotation playerPic

    avgSpeed <- averageRecentHistory 120 -< norm velocity
    
    returnA -<
      ( changeMode_
      , Output (pictures [renderGameState avgSpeed position (pictures [playerTrace, thrustAnim]) trackPic hook])
                         -- , thrustAnim])
        Nothing )

-- -- traceAnimation :: (Vec World, Angle) ~> Picture
-- -- traceAnimation = proc (pos, rot) -> do
-- --   let pic = translatePic pos $ rotatePic rot playerPic
-- --   returnA -< _
-- 
thrusterAnimation :: (Bool, Angle, Vec World) ~> Picture
thrusterAnimation =
  let
    frames = map (color yellow . translatePic (Vec 0 12) . polygon)
             [isoscelesTrianglePath 5 5, isoscelesTrianglePath 7 15]
  in
    proc (thrusterOn, rotation, pos) -> do
      pic <- slideShow 0.05 frames -< ()
      returnA -< if thrusterOn
                then translatePic pos (rotatePic (pi + rotation) pic)
                else blank

-- TODO make independent of frame rate
fadingTrace :: Int -> Color -> Picture ~> Picture
fadingTrace nFrames col =
  let (r,g,b,_) = rgbaOfColor col
      alphas = map (\n -> 1 / fromIntegral n) [1..nFrames]
        -- reverse [i / fromIntegral nFrames | i <- map fromIntegral [0..nFrames-1]]
      colors = [makeColor r g b a | a <- alphas]
  in pictures . zipWith color colors <$> recentHistory nFrames

-- -- hook ::  
-- -- hook startPos velocity = constant velocity >>> cumsumFrom startPos >>^ attach
-- --   where
-- --     attach :: Vec World -> 
--   
-- -- isoscelesTrianglePath 14 23
