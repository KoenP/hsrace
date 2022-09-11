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
import Grid
import Editor.Pillar
import Overlay
import Game.Render ( render, RenderData(RenderData) )
import Game.Types

import Graphics.Gloss

import Prelude hiding ((.), id)
import Data.Maybe
import Control.Applicative
--------------------------------------------------------------------------------

-- k_acceleration, k_drag, k_hookSpeed :: Double
-- k_acceleration = 0.05 -- 0.2
-- k_drag         = 0 -- 0.0005
-- 
-- k_dragOffroad :: Double
-- k_dragOffroad  = 0 -- 0.1
-- k_hookSpeed    = 4000

accelerationPS, dragPS, hookSpeedPS :: PerSecond Double
accelerationPS = PerSecond 500
dragPS         = PerSecond 0
hookSpeedPS    = PerSecond 4000

hook :: [Pillar] -> (Vec World, Vec World, Bool) ~> Hook
hook pillars = (\(x,y,z) -> (x,selectPillar y, z)) ^>> runMode noHookMode
  where
    selectPillar = selectedPillar pillars

    noHookMode :: Mode (Vec World, Maybe (Vec World), Bool) Hook
    noHookMode = Mode $ arr $ \(startPos, goal, launchHook) ->
      let event = sample launchHook (hookTravellingMode startPos) <*> goal
      in (event, NoHook)

    hookTravellingMode :: Vec World -> Vec World
                       -> Mode (Vec World, Maybe (Vec World), Bool) Hook
    hookTravellingMode startPos goal =
      let time = startPos <-> goal / unPerSecond hookSpeedPS -- k_hookSpeed
      in Mode $ proc (_, _, launchHook) -> do
        t <- fmap (/time) clock -< ()
        let pos = lerp startPos goal t
        let releaseEvent = sample (not launchHook) noHookMode
        let attachEvent = sample (t >= 1) (hookAttachedMode goal)
        returnA -< (releaseEvent <|> attachEvent, HookTravelling pos)

    hookAttachedMode :: Vec World -> Mode (Vec World, Maybe (Vec World), Bool) Hook
    hookAttachedMode pos = Mode $ arr $ \(_,_,keepAttached)
      -> (sample (not keepAttached) noHookMode, HookAttached pos)
  
selectedPillar :: [Pillar] -> Vec World -> Maybe (Vec World)
selectedPillar pillars =
  let cellSize = 400
      grid = mkGrid cellSize (repeat () `zip` pillars)
  in fmap fst <$> closestNearby grid 

-- TODO apply a fix to make sure the tether doesn't stretch
spanTether :: Vec World -> Hook -> Vec World -> Maybe (Vec World)
spanTether pos (HookAttached anchor) vel
  | delta <- anchor ^-^ pos
  , theta <- vel `internalAngle` delta
  , theta > (pi/2)
  = Just $ vel `projectOnto` perp delta
spanTether _   _                     _
  = Nothing
  
-- | Computes velocity based on delayed position, current acceleration
--   and current state of the hookshot. Also outputs the contribution of
--   the hook tether, if relevant.
velocitySF :: ((Vec World, PerSecond (Vec World), Hook)
               ~> (PerSecond (Vec World), Maybe (Vec World)))
velocitySF = stateful (PerSecond zeroVec, Nothing) step
  where
    step dt (dPos, acc, hook) (dVel, _)
      = let vel    = unPerSecond dVel ^+^ frameDelta acc dt
            tether = spanTether dPos hook vel
        in (PerSecond (fromMaybe vel tether), tether)
           
{-
x0 = 0 m
v0 = 10 m/s 
a = 1 m/s/s 

dt = 0.5 s

v1 = v0 + a*dt
x1 = x0 + v1*dt
-}

-- | Main game loop.
game :: Game
game switchTo (GameTrack onRoad pillars trackPic checkpoints) =
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
        thrust | accelerating = rotVec rotation . Vec 0 <$> accelerationPS
               | otherwise    = PerSecond zeroVec
      
      dPosition <- delay zeroVec -< position
      dVelocity <- delay zeroVec -< velocity
      dCursorWorldPos <- delay zeroVec -< cursorWorldPos
      hook      <- hook pillars
        -< (dPosition, dCursorWorldPos, keyDown LaunchHook input) -- (input, dPosition, fromPolar k_hookSpeed rotation)
      let dSpeed = norm dVelocity
      
      let dragFactor = 0 -- if onRoad dPosition then k_drag else k_dragOffroad
      let drag = (- dragFactor * dSpeed ** 2) *^ normalize dVelocity
      (velocity, _tether) <- velocitySF -< (dPosition, thrust ^+^ drag, hook)
      position <- integral -< velocity

      -- Calculate viewport and cursor world position.
      avgSpeed :: Double <- averageRecentHistory 120 -< norm velocity

      let
        (zoomMin, zoomMax) = (0.3, 0.3) -- (0.2, 1)
        zoom               = clamp (zoomMin, zoomMax) $ lerp zoomMax zoomMin (avgSpeed / 500)
        viewPort           = ViewPort position 0 zoom
        cursorWorldPos     = windowCoordsToWorldCoords viewPort cursorPos

    timePassed_ <- timePassed -< ()
    fps <- (1/) ^<< timeDelta -< ()
    let 
      clockPic = translatePic (Vec (-20) 0)
                 $ text (minutesSecondsCentiseconds timePassed_)
      wsize    = _input_windowSize input
      fpsPic   = fromWindowLeft wsize 10 (text (show (floor fps :: Int)))
      overlay  = fromWindowTop wsize 70 $ color white $ Graphics.Gloss.scale 0.5 0.5
                 $ pictures [clockPic, fpsPic]
                 
    pic <- render pillars trackPic
      -< RenderData viewPort position rotation accelerating hook
    
    returnA -<
      ( changeMode_
      , Output (pictures [pic, overlay]) Nothing
      )

